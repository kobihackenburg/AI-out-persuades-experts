# si_06_fact_density_within.R
# =============================================================================
# Within-issue, within-condition robustness check for the fact-density --
# persuasion relationship plotted in Figure 3b.
#
# Figure 3b is a per-condition aggregate scatter (TE vs. mean fact claims),
# which a careful reader could attribute to issue-level co-variation: maybe
# certain issues are both more persuadable AND elicit more facts. To rule
# this out, we fit a conversation-level LMM that adds the per-conversation
# fact-claim count as a continuous predictor on top of the preregistered
# attitude-change spec, with `condition` as a fixed effect so the
# coefficient on `n_fact_claims` is identified within-issue, within-
# condition (i.e. it can't pick up between-condition differences in mean
# fact density or between-issue persuadability).
#
#   post ~ pre + issue + condition + n_fact_claims + attempt
#        + (1 | persuader) + (1 | persuadee)
#
# `condition` is the same `group_ai_separate` label used to colour Fig 3b
# (humans by class; AI split by model x study; Constrained AI as one arm).
# Sample is fact-checked sessions only (matches Fig 3b's denominator), so
# a per-row `n_fact_claims` is always observed. We save a tidy RDS and a
# numbers.json companion (`fact_density_within.coef.*`) for the SI prose
# to reference.
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/analysis/_lmm_helpers.R"))

message("\n==== si_06_fact_density_within.R ====")

prep <- function(df, study_label) {
  df |>
    complete_case_filter(label = paste("si_06", study_label)) |>
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
      ai_model_full   = ai_model_full,
      fact_checked    = fact_checked,
      n_fact_claims   = n_fact_claims
    )
}

studies <- load_studies(c("study_1", "study_2", "study_3"))

pooled_df <- dplyr::bind_rows(
  prep(studies$study_1, "study_1"),
  prep(studies$study_2, "study_2"),
  prep(studies$study_3, "study_3")
) |>
  dplyr::mutate(
    study = factor(study),
    group = forcats::fct_relevel(droplevels(group), "control")
  )

# Same `group_ai_separate` labelling as 01_main_attitudes.R, so `condition`
# in this LMM matches the colour key on Fig 3b.
ai_model_label <- function(x) {
  dplyr::case_when(
    x == "claude-4.1-opus" ~ "Claude Opus 4.1",
    x == "claude-4.6-opus" ~ "Claude Opus 4.6",
    x == "gemini-2.5-pro"  ~ "Gemini 2.5 Pro",
    x == "gpt-4o-latest"   ~ "ChatGPT-4o (latest)",
    x == "gpt-5-4"         ~ "GPT-5.4",
    x == "grok-4"          ~ "Grok 4.20",
    TRUE                   ~ NA_character_
  )
}

fit_df <- pooled_df |>
  dplyr::mutate(
    constrained_flag = dplyr::coalesce(ai_constrained, FALSE),
    group_unified = dplyr::case_when(
      group == "control"                ~ "Control",
      group == "ai" &  constrained_flag ~ "AI (Constrained)",
      group == "ai" & !constrained_flag ~ "AI (Info-prompted)",
      group == "random_lay_person"      ~ "Random Layperson",
      group == "elite_lay_person"       ~ "Elite Layperson",
      group == "canvasser"              ~ "Professional Canvasser",
      group == "elite_debater"          ~ "Elite Debater",
      group == "coached_elite_debater"  ~ "Coached Elite Debater",
      TRUE                              ~ NA_character_
    ),
    ai_lbl = ai_model_label(ai_model_full),
    condition = dplyr::case_when(
      group == "ai" &  constrained_flag ~ paste0(ai_lbl, " (Constrained)"),
      group == "ai" & !constrained_flag ~ paste0(
        ai_lbl, " (Info) ",
        toupper(sub("study_", "S", study))
      ),
      TRUE                              ~ group_unified
    )
  ) |>
  dplyr::filter(!is.na(condition),
                condition != "Control",
                fact_checked,
                !is.na(n_fact_claims)) |>
  dplyr::mutate(
    condition = factor(condition)
  )

message(sprintf("  fit_df: %d fact-checked rows, %d conditions, %d issues, %d persuaders, %d persuadees",
                nrow(fit_df),
                dplyr::n_distinct(fit_df$condition),
                dplyr::n_distinct(fit_df$issue),
                dplyr::n_distinct(fit_df$persuader),
                dplyr::n_distinct(fit_df$persuadee)))

m <- lme4::lmer(
  post ~ pre + issue + condition + n_fact_claims + attempt
       + (1 | persuader) + (1 | persuadee),
  data    = fit_df,
  REML    = TRUE,
  control = lme4::lmerControl(optimizer = "bobyqa",
                              optCtrl   = list(maxfun = 100000))
)

# Wald summary on `n_fact_claims` only (same z-based convention as the
# rest of the manuscript: estimate, SE, two-sided p, 95% CI).
fe       <- lme4::fixef(m)
vc       <- as.matrix(stats::vcov(m))
beta     <- fe[["n_fact_claims"]]
se       <- sqrt(vc["n_fact_claims", "n_fact_claims"])
z        <- beta / se
p_two    <- 2 * stats::pnorm(-abs(z))
ci_lower <- beta - stats::qnorm(0.975) * se
ci_upper <- beta + stats::qnorm(0.975) * se

# Per-10-fact effect (more readable in prose than per-1-fact).
beta_per10     <- 10 * beta
ci_per10_lower <- 10 * ci_lower
ci_per10_upper <- 10 * ci_upper

fact_density_within <- tibble::tibble(
  estimate_per_fact      = beta,
  se_per_fact            = se,
  lower_ci_per_fact      = ci_lower,
  upper_ci_per_fact      = ci_upper,
  p_value                = p_two,
  estimate_per_10_facts  = beta_per10,
  lower_ci_per_10_facts  = ci_per10_lower,
  upper_ci_per_10_facts  = ci_per10_upper,
  n_obs                  = nrow(fit_df),
  n_conditions           = dplyr::n_distinct(fit_df$condition),
  n_issues               = dplyr::n_distinct(fit_df$issue),
  n_persuaders           = dplyr::n_distinct(fit_df$persuader),
  n_persuadees           = dplyr::n_distinct(fit_df$persuadee)
)
save_rds(fact_density_within, "fact_density_within")

message("\n  n_fact_claims coefficient (within issue + within condition):")
message(sprintf("    per +1 fact:   %+5.4f pp  [%+5.4f, %+5.4f]  (p = %.3g)",
                beta, ci_lower, ci_upper, p_two))
message(sprintf("    per +10 facts: %+5.2f pp  [%+5.2f, %+5.2f]  (p = %.3g)",
                beta_per10, ci_per10_lower, ci_per10_upper, p_two))
message("\n  done.")
