# _lmm_helpers.R
# Shared helpers for fitting the preregistered LMM specification and pulling
# tidy treatment-vs-control contrasts out of it. Sourced by every analysis
# script that fits the main attitude / mechanism model
# (01_main_attitudes.R, 02_mechanism.R, and the SI scripts that re-fit
# subsets of the same spec).

# clean_contrast_name(): emmeans wraps contrast names whose levels contain
# spaces or parens in extra outer parens, e.g. "(AI (Info-prompted) S1) -
# Control". Strip both the " - <ref>" suffix and the outer wrap so
# downstream code can key off plain labels. Handles either "control"
# (raw factor) or "Control" (relabelled fig1 factors).
clean_contrast_name <- function(x) {
  x <- sub(" - [Cc]ontrol$", "", as.character(x))
  sub("^\\((.+)\\)$", "\\1", x)
}

# fit_pooled_lmm(): fit the preregistered LMM with the given column as the
# categorical predictor. The RHS is fixed at the preregistered defaults
# (`pre + issue + <group_col> + attempt + (1 | persuader) + (1 | persuadee)`);
# `extra_fe` lets robustness fits add e.g. `+ study` without forking the
# helper.
#   data       data frame with columns post, pre, issue, attempt,
#              persuader, persuadee, plus <group_col>
#              (and `study` if extra_fe includes "study")
#   group_col  string naming the categorical predictor
#   extra_fe   character vector of additional fixed effects beyond the
#              preregistered defaults. Pass `"study"` for the
#              with-study-FE robustness fit; default `character(0)` keeps
#              the preregistered RHS.
#   ref        reference level for the group factor (default "control")
#   label      short string used only for the logging line
fit_pooled_lmm <- function(data, group_col, extra_fe = character(0),
                           ref = "control", label = group_col) {
  d <- data
  d[[group_col]] <- forcats::fct_relevel(factor(d[[group_col]]), ref)
  rhs <- paste(c("pre", "issue", group_col, "attempt", extra_fe),
               collapse = " + ")
  rhs <- paste0(rhs, " + (1 | persuader) + (1 | persuadee)")
  fm  <- stats::as.formula(paste("post ~", rhs))
  message(sprintf("  [%s] fit: %s", label, deparse1(fm)))
  lme4::lmer(
    fm,
    data    = d,
    REML    = TRUE,
    control = lme4::lmerControl(optimizer = "bobyqa",
                                optCtrl   = list(maxfun = 100000))
  )
}

# tidy_te_vs_control(): extract treatment-vs-control contrasts from an
# emmeans grid keyed on `group_col`. Returns a tibble with one row per
# non-control level and columns (group, estimate, lower_ci, upper_ci,
# p_value). CIs are asymptotic (z-based) Wald CIs and p-values are
# unadjusted: each treatment-vs-control contrast is preregistered, so we
# override emmeans' `trt.vs.ctrl` Dunnett multiplicity correction.
tidy_te_vs_control <- function(model, group_col, ref = "control") {
  fm <- stats::as.formula(paste("~", group_col))
  emmeans::emmeans(model, fm, lmer.df = "asymptotic") |>
    emmeans::contrast(method = "trt.vs.ctrl", ref = ref) |>
    summary(infer = TRUE, adjust = "none") |>
    tibble::as_tibble() |>
    dplyr::transmute(
      group    = clean_contrast_name(contrast),
      estimate = estimate,
      lower_ci = asymp.LCL,
      upper_ci = asymp.UCL,
      p_value  = p.value
    )
}
