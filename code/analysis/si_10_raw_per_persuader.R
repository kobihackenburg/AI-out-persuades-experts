# si_10_raw_per_persuader.R
# =============================================================================
# Unshrunken per-persuader treatment effects from a pooled S1+S2 LMM with
# `persuader` switched from random to fixed effect.
#
# Why this exists: the EB BLUPs from `pooled_s1_s2_lmm` (saved by
# `01_main_attitudes.R` section 4b) collapse toward each class mean
# because per-persuader sample sizes are small. This script addresses
# whether any individual human's *raw* (unshrunken) mean exceeds AI's
# 13.9 pp reference. It refits the same pooled S1+S2 LMM with one term
# changed -- `(1 | persuader)` becomes `persuader_occasion` as a fixed
# effect -- and reports per-persuader point estimates and one-sided
# tests against AI.
#
# Definitions:
# - persuader_occasion: each Coached Elite Debater contributes two
#   levels (one under group=elite_debater for their pre-coaching
#   conversations, one under group=coached_elite_debater for their
#   post-coaching ones), so the count parallels the 318 BLUPs.
# - AI reference: the saved S1+S2 AI mean (13.92 pp from
#   `limits_ai_reference.rds`), treated as fixed in the test.
#   Treating AI as known shrinks the test denominator and so makes it
#   *easier* to find a human significantly above AI, which is the
#   conservative direction for the null being assessed ("no human
#   exceeds AI").
# - One-sided test:
#       z_h = (estimate_h - ai_ref) / se_h
#       p_h = 1 - Phi(z_h)
#   Tally: K1 = # with point estimate > AI ref; K2 = # with lower 95%
#   CI > AI ref; K3 = # with p < .05; p_min = smallest p across humans.
#
# Output: `output/results/si_raw_per_persuader.rds` with one row per
# persuader_occasion (humans only):
#   persuader_id, group, estimate, se, lower_ci, upper_ci,
#   z_vs_ai, p_one_sided.
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/analysis/_lmm_helpers.R"))

message("\n==== si_10_raw_per_persuader.R ====")

# =============================================================================
# 1. Data assembly (same prep as 01_main_attitudes.R)
# =============================================================================
prep <- function(df, study_label) {
  df |>
    complete_case_filter(label = paste("si_10_raw", study_label)) |>
    dplyr::transmute(
      study     = study_label,
      persuadee = persuadee_id,
      persuader = dplyr::case_when(
        !is.na(persuader_id)                            ~ persuader_id,
        persuader_class == "ai" & !is.na(ai_model_full) ~ paste0("ai_", ai_model_full),
        TRUE                                            ~ paste0("solo_", persuadee_session)
      ),
      issue   = issue_id,
      group   = persuader_class,
      pre     = pre_attitude,
      post    = post_attitude,
      attempt = dplyr::coalesce(as.integer(persuadee_attempt_number), 1L)
    )
}

studies <- load_studies(c("study_1", "study_2"))

pooled_df_s1_s2 <- dplyr::bind_rows(
  prep(studies$study_1, "study_1"),
  prep(studies$study_2, "study_2")
) |>
  dplyr::mutate(
    study = factor(study),
    group = forcats::fct_relevel(droplevels(group), "control")
  )

# Drop AI rows: we test humans vs the external S1+S2 AI reference (13.92
# pp), so AI rows do not need to be in this fit. Collapse all controls
# into one reference level "control".
fit_df <- pooled_df_s1_s2 |>
  dplyr::filter(group != "ai") |>
  dplyr::mutate(
    persuader_occasion = dplyr::case_when(
      group == "control" ~ "control",
      TRUE               ~ paste0(persuader, "::", as.character(group))
    )
  ) |>
  dplyr::mutate(
    persuader_occasion = forcats::fct_relevel(factor(persuader_occasion),
                                              "control")
  )

n_levels    <- dplyr::n_distinct(fit_df$persuader_occasion)
n_human_lvl <- dplyr::n_distinct(fit_df$persuader_occasion) - 1L
message(sprintf("  fit_df: %d rows, %d persuader_occasion levels (1 control + %d humans), %d issues, %d persuadees",
                nrow(fit_df), n_levels, n_human_lvl,
                dplyr::n_distinct(fit_df$issue),
                dplyr::n_distinct(fit_df$persuadee)))

# =============================================================================
# 2. Refit pooled S1+S2 LMM with persuader as fixed effect
# =============================================================================
# Same RHS as `pooled_s1_s2_lmm` except `(1 | persuader)` -> fixed
# `persuader_occasion`. The persuadee random intercept stays identical.
m_raw <- lme4::lmer(
  post ~ pre + issue + attempt + persuader_occasion + (1 | persuadee),
  data    = fit_df,
  REML    = TRUE,
  control = lme4::lmerControl(optimizer = "bobyqa",
                              optCtrl   = list(maxfun = 1e5))
)

message("  refit complete.")

# =============================================================================
# 3. Per-persuader contrasts vs control + lenient test against AI
# =============================================================================
# With "control" as the reference level, the lmer fixed-effect coefficient
# `persuader_occasion<level>` IS the level-vs-control contrast directly
# (treatment contrast coding); SEs come from the vcov diagonal. No
# emmeans marginalization needed.
fe        <- lme4::fixef(m_raw)
vc        <- as.matrix(stats::vcov(m_raw))
po_idx    <- grep("^persuader_occasion", names(fe))
po_levels <- sub("^persuader_occasion", "", names(fe)[po_idx])
estimate  <- as.numeric(fe[po_idx])
se        <- sqrt(diag(vc)[po_idx])

ai_ref <- as.numeric(load_rds("limits_ai_reference"))

raw_tbl <- tibble::tibble(
  level       = po_levels,
  estimate    = estimate,
  se          = se,
  lower_ci    = estimate - stats::qnorm(0.975) * se,
  upper_ci    = estimate + stats::qnorm(0.975) * se,
  z_vs_ai     = (estimate - ai_ref) / se,
  p_one_sided = stats::pnorm((estimate - ai_ref) / se, lower.tail = FALSE)
)

# Filter to human levels (carry "::" separator). Control is the reference
# level so it has no fixed-effect row of its own.
si_raw_per_persuader <- raw_tbl |>
  dplyr::filter(grepl("::", level)) |>
  dplyr::mutate(
    persuader_id = sub("::.*$", "", level),
    group        = sub("^.*::", "", level)
  ) |>
  dplyr::select(persuader_id, group, estimate, se, lower_ci, upper_ci,
                z_vs_ai, p_one_sided)

save_rds(si_raw_per_persuader, "si_raw_per_persuader")

# =============================================================================
# 4. Audit log: K1 / K2 / K3 / p_min + top-5 raw point estimates
# =============================================================================
n_total       <- nrow(si_raw_per_persuader)
n_above       <- sum(si_raw_per_persuader$estimate    > ai_ref)
n_lower_above <- sum(si_raw_per_persuader$lower_ci    > ai_ref)
n_p_lt_05     <- sum(si_raw_per_persuader$p_one_sided < 0.05)
p_min         <- min(si_raw_per_persuader$p_one_sided)

message(sprintf("\n  AI reference (S1+S2 mean):              %.3f pp", ai_ref))
message(sprintf("  N persuader-occasions (humans):         %d", n_total))
message(sprintf("  K1: point estimate > AI ref:            %d", n_above))
message(sprintf("  K2: lower 95%% CI > AI ref:             %d", n_lower_above))
message(sprintf("  K3: one-sided p < .05 vs AI:            %d", n_p_lt_05))
message(sprintf("  smallest one-sided p across humans:     %.3g", p_min))

message("\n  top 5 raw per-persuader point estimates (humans):")
top <- si_raw_per_persuader |>
  dplyr::arrange(dplyr::desc(estimate)) |>
  head(5)
for (i in seq_len(nrow(top))) {
  r <- top[i, ]
  message(sprintf("    %s::%s    %+5.2f pp [%+5.2f, %+5.2f]   z = %5.2f  p_1s = %.3g",
                  r$persuader_id, r$group, r$estimate,
                  r$lower_ci, r$upper_ci,
                  r$z_vs_ai, r$p_one_sided))
}

message("\n  done.")
