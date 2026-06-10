# si_07_ai_gap_net_of_facts.R
# =============================================================================
# Direct test of the claim "fact density does not fully account for AI's
# persuasive advantage". The aggregate scatter in Fig. 3b shows a strong
# log-fact-density -> persuasion relationship pooled across human and AI
# conditions ($R^2 \approx 0.89$ at the per-condition level), and the
# manuscript's mechanism section argues qualitatively from Constrained
# AI's location in that scatter (lots of facts, little persuasion) that
# fact density alone cannot be the whole story.
#
# This script replaces that qualitative argument with a single regression
# coefficient: pool all fact-checked S1+S2 conversations, regress post-
# attitude on log(n_fact_claims) and a binary AI vs human indicator (plus
# the usual covariates and crossed random effects), and read off the
# is_human coefficient. That coefficient answers exactly the question
# "at the same fact count, is there still a persuasion gap between AI
# and human persuaders?"
#
#   post ~ pre + issue + attempt + log(n_fact_claims) + is_human
#        + (1 | persuader) + (1 | persuadee)
#
# Sample: fact-checked conversations only (controls already excluded
# by the fact-checked filter -- controls have no persuader so no
# facts). Two pooled samples are built: S1+S2 (matches the
# Fig. 3b / si_06 denominator and backs the headline claim) and
# S1+S2+S3 (with Professional Canvassers and the three Study 3 AI
# models added; backs the canvasser-robustness parenthetical in the
# mechanism section).
#
# We report four specifications:
#   (1) all_ai_vs_humans (S1+S2): is_human binary, lumping AI
#       (info-prompted + Constrained) vs all human classes.
#   (2) unconstrained_ai_vs_humans (S1+S2): same as (1) but with
#       Constrained AI dropped, so the contrast is Unconstrained AI vs
#       humans -- closer to the manuscript's headline claim about
#       throughput-relaxed AI.
#   (3) all_ai_vs_humans_with_s3: spec (1) re-fit on S1+S2+S3, i.e.
#       Professional Canvassers and the three Study 3 AI models added
#       to the sample. Used for the body-text canvasser-robustness
#       parenthetical in the mechanism section.
#   (4) unconstrained_ai_vs_humans_with_s3: spec (2) re-fit on
#       S1+S2+S3.
#
# Output: `output/results/si_ai_gap_net_of_facts.rds` (one row per spec)
# with columns spec, beta_is_human, se, lower_ci, upper_ci, p_two,
# beta_log_facts, n_obs, etc.
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/analysis/_lmm_helpers.R"))

message("\n==== si_07_ai_gap_net_of_facts.R ====")

# =============================================================================
# 1. Data prep (mirrors si_06_fact_density_within.R)
# =============================================================================
prep <- function(df, study_label) {
  df |>
    complete_case_filter(label = paste("si_07", study_label)) |>
    dplyr::transmute(
      study     = study_label,
      persuadee = persuadee_id,
      persuader = dplyr::case_when(
        !is.na(persuader_id)                            ~ persuader_id,
        persuader_class == "ai" & !is.na(ai_model_full) ~ paste0("ai_", ai_model_full),
        TRUE                                            ~ paste0("solo_", persuadee_session)
      ),
      issue           = issue_id,
      group           = persuader_class,
      pre             = pre_attitude,
      post            = post_attitude,
      attempt         = dplyr::coalesce(as.integer(persuadee_attempt_number), 1L),
      ai_constrained  = ai_constrained,
      fact_checked    = fact_checked,
      n_fact_claims   = n_fact_claims
    )
}

studies <- load_studies(c("study_1", "study_2", "study_3"))

build_base <- function(study_keys, label) {
  pooled <- dplyr::bind_rows(
    purrr::map(study_keys, \(k) prep(studies[[k]], k))
  ) |>
    dplyr::mutate(
      study = factor(study),
      group = forcats::fct_relevel(droplevels(group), "control")
    )

  out <- pooled |>
    dplyr::filter(fact_checked,
                  !is.na(n_fact_claims),
                  group != "control") |>
    dplyr::mutate(
      log_n_facts = log(n_fact_claims),
      is_human    = as.integer(group != "ai")
    )

  stopifnot(all(out$n_fact_claims > 0L))

  message(sprintf("  base sample [%s]: %d rows, %d issues, %d persuaders, %d persuadees",
                  label,
                  nrow(out),
                  dplyr::n_distinct(out$issue),
                  dplyr::n_distinct(out$persuader),
                  dplyr::n_distinct(out$persuadee)))
  message(sprintf("    AI rows = %d (info-prompted + constrained); human rows = %d",
                  sum(out$is_human == 0L),
                  sum(out$is_human == 1L)))
  out
}

# Headline sample: S1+S2 only (matches the Fig. 3b denominator)
base_df    <- build_base(c("study_1", "study_2"),               "S1+S2")
# Robustness sample: + Study 3 (Professional Canvassers and S3 AI models)
base_df_s3 <- build_base(c("study_1", "study_2", "study_3"),    "S1+S2+S3")

# =============================================================================
# 2. Fit each spec
# =============================================================================
fit_lmm <- function(d) {
  lme4::lmer(
    post ~ pre + issue + attempt + log_n_facts + is_human
         + (1 | persuader) + (1 | persuadee),
    data    = d,
    REML    = TRUE,
    control = lme4::lmerControl(optimizer = "bobyqa",
                                optCtrl   = list(maxfun = 1e5))
  )
}

drop_constrained <- function(d) {
  d |> dplyr::filter(!(group == "ai" & dplyr::coalesce(ai_constrained, FALSE)))
}

# (1) S1+S2, all AI vs humans
m1            <- fit_lmm(base_df)
# (2) S1+S2, Unconstrained AI vs humans
df_unconstr   <- drop_constrained(base_df)
m2            <- fit_lmm(df_unconstr)
# (3) S1+S2+S3, all AI vs humans
m3            <- fit_lmm(base_df_s3)
# (4) S1+S2+S3, Unconstrained AI vs humans
df_unconstr_s3 <- drop_constrained(base_df_s3)
m4            <- fit_lmm(df_unconstr_s3)

# =============================================================================
# 4. Wald extraction (z-based, matches manuscript convention)
# =============================================================================
extract_row <- function(model, label, n_obs) {
  fe <- lme4::fixef(model)
  vc <- as.matrix(stats::vcov(model))
  out <- list(spec = label, n_obs = n_obs)
  for (term in c("is_human", "log_n_facts")) {
    b   <- fe[[term]]
    se  <- sqrt(vc[term, term])
    z   <- b / se
    p   <- 2 * stats::pnorm(-abs(z))
    lo  <- b - stats::qnorm(0.975) * se
    hi  <- b + stats::qnorm(0.975) * se
    out[[paste0("beta_",  term)]] <- b
    out[[paste0("se_",    term)]] <- se
    out[[paste0("lower_", term)]] <- lo
    out[[paste0("upper_", term)]] <- hi
    out[[paste0("z_",     term)]] <- z
    out[[paste0("p_",     term)]] <- p
  }
  tibble::as_tibble(out)
}

si_ai_gap_net_of_facts <- dplyr::bind_rows(
  extract_row(m1, "all_ai_vs_humans",                  nrow(base_df)),
  extract_row(m2, "unconstrained_ai_vs_humans",        nrow(df_unconstr)),
  extract_row(m3, "all_ai_vs_humans_with_s3",          nrow(base_df_s3)),
  extract_row(m4, "unconstrained_ai_vs_humans_with_s3", nrow(df_unconstr_s3))
)
save_rds(si_ai_gap_net_of_facts, "si_ai_gap_net_of_facts")

# =============================================================================
# 5. Audit log (Wald CIs, two-sided p)
# =============================================================================
for (i in seq_len(nrow(si_ai_gap_net_of_facts))) {
  r <- si_ai_gap_net_of_facts[i, ]
  message(sprintf("\n  spec: %s   (n = %d)", r$spec, r$n_obs))
  message(sprintf("    is_human       = %+6.3f pp  [%+6.3f, %+6.3f]   z = %5.2f   p = %.3g",
                  r$beta_is_human,    r$lower_is_human,
                  r$upper_is_human,   r$z_is_human,
                  r$p_is_human))
  message(sprintf("    log_n_facts    = %+6.3f pp  [%+6.3f, %+6.3f]   z = %5.2f   p = %.3g  (per +1 log fact = doubling)",
                  r$beta_log_n_facts, r$lower_log_n_facts,
                  r$upper_log_n_facts, r$z_log_n_facts,
                  r$p_log_n_facts))
  per_double <- log(2) * r$beta_log_n_facts
  per_d_lo   <- log(2) * r$lower_log_n_facts
  per_d_hi   <- log(2) * r$upper_log_n_facts
  message(sprintf("    => persuasion change per doubling of facts: %+5.2f pp  [%+5.2f, %+5.2f]",
                  per_double, per_d_lo, per_d_hi))
}

message("\n  done.")
