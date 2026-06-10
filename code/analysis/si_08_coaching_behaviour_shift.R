# si_08_coaching_behaviour_shift.R
# =============================================================================
# Did coaching shift Elite Debater behaviour toward AI-like throughput?
#
# Compares the 43 returning Elite Debaters' Study 1 vs Study 2 conversations
# on two outcomes: (a) words per persuader message and (b) number of
# fact-checkable claims per conversation. Primary test is a per-persuader
# paired t (each debater contributes one S1 mean and one S2 mean); a mixed
# model with persuader random intercept is reported as a cross-check.
#
# Backs the Discussion mechanism paragraph in manuscript.tex stating that
# coaching did materially shift debater behaviour toward AI-like throughput
# (more words per message, more fact-checkable claims per conversation)
# even though it did not close the persuasion gap, supporting the
# structural-bottleneck interpretation.
#
# Outputs (under output/results/, consumer after `->`):
#   coaching_behaviour_shift.rds   -> 99_extract_numbers.R
#                                     (coaching_shift.<outcome>.*)
#                                     and SI subsection \label{si:coaching-shift}
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))

message("\n==== si_08_coaching_behaviour_shift.R ====")

studies <- load_studies(c("study_1", "study_2"))

prep <- function(df, class_target, study_label) {
  df |>
    dplyr::filter(persuader_class == class_target,
                  !is.na(persuader_id),
                  !is.na(n_persuader_msgs), n_persuader_msgs > 0,
                  !is.na(persuader_words)) |>
    dplyr::transmute(
      study           = study_label,
      persuader_id,
      class           = class_target,
      n_persuader_msgs,
      persuader_words,
      words_per_msg   = persuader_words / n_persuader_msgs,
      fact_checked,
      n_fact_claims
    )
}

ed  <- prep(studies$study_1, "elite_debater",         "study_1")
ced <- prep(studies$study_2, "coached_elite_debater", "study_2")

both <- dplyr::bind_rows(ed, ced) |>
  dplyr::mutate(
    class = factor(class, levels = c("elite_debater", "coached_elite_debater"))
  )

message(sprintf("  elite_debater (S1):         n_conv = %d, n_persuader = %d",
                nrow(ed),  dplyr::n_distinct(ed$persuader_id)))
message(sprintf("  coached_elite_debater (S2): n_conv = %d, n_persuader = %d",
                nrow(ced), dplyr::n_distinct(ced$persuader_id)))

# ---- helpers ---------------------------------------------------------------
fit_paired <- function(df, value_col) {
  wide <- df |>
    dplyr::group_by(persuader_id, class) |>
    dplyr::summarise(m = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = class, values_from = m) |>
    dplyr::filter(!is.na(elite_debater), !is.na(coached_elite_debater))
  list(
    wide = wide,
    test = stats::t.test(wide$coached_elite_debater,
                         wide$elite_debater,
                         paired = TRUE)
  )
}

fit_lmm <- function(df, value_col) {
  fm <- stats::as.formula(sprintf("%s ~ class + (1 | persuader_id)", value_col))
  m  <- lme4::lmer(fm, data = df, REML = TRUE,
                   control = lme4::lmerControl(optimizer = "bobyqa",
                                               optCtrl   = list(maxfun = 100000)))
  fe   <- lme4::fixef(m)
  vc   <- as.matrix(stats::vcov(m))
  beta <- unname(fe["classcoached_elite_debater"])
  se   <- sqrt(vc["classcoached_elite_debater", "classcoached_elite_debater"])
  list(estimate = beta, se = se,
       p_value  = 2 * stats::pnorm(-abs(beta / se)))
}

make_row <- function(outcome, paired, lmm, baseline_mean, n_conv_s1, n_conv_s2) {
  t  <- paired$test
  tibble::tibble(
    outcome        = outcome,
    n_pairs        = nrow(paired$wide),
    n_conv_s1      = n_conv_s1,
    n_conv_s2      = n_conv_s2,
    baseline_mean  = baseline_mean,
    estimate       = unname(t$estimate),
    lower_ci       = t$conf.int[1L],
    upper_ci       = t$conf.int[2L],
    t_stat         = unname(t$statistic),
    df             = unname(t$parameter),
    p_value        = t$p.value,
    pct_change     = unname(t$estimate) / baseline_mean,
    lmm_estimate   = lmm$estimate,
    lmm_se         = lmm$se,
    lmm_p_value    = lmm$p_value
  )
}

# ---- (a) Words per message -------------------------------------------------
words_paired <- fit_paired(both, "words_per_msg")
words_lmm    <- fit_lmm   (both, "words_per_msg")

# ---- (b) Fact-checkable claims per conversation ----------------------------
# Restrict to fact-checked conversations (matches the denominator used in
# the mechanism facts panel / mechanism_facts_vs_te).
both_fc <- both |> dplyr::filter(fact_checked, !is.na(n_fact_claims))
facts_paired <- fit_paired(both_fc, "n_fact_claims")
facts_lmm    <- fit_lmm   (both_fc, "n_fact_claims")

# ---- assemble + save -------------------------------------------------------
coaching_behaviour_shift <- dplyr::bind_rows(
  make_row("words_per_msg",
           words_paired, words_lmm,
           baseline_mean = mean(ed$words_per_msg),
           n_conv_s1     = nrow(ed),
           n_conv_s2     = nrow(ced)),
  make_row("fact_claims_per_conv",
           facts_paired, facts_lmm,
           baseline_mean = mean(both_fc$n_fact_claims[both_fc$class == "elite_debater"]),
           n_conv_s1     = sum(both_fc$class == "elite_debater"),
           n_conv_s2     = sum(both_fc$class == "coached_elite_debater"))
)
save_rds(coaching_behaviour_shift, "coaching_behaviour_shift")

# ---- audit log -------------------------------------------------------------
for (i in seq_len(nrow(coaching_behaviour_shift))) {
  r <- coaching_behaviour_shift[i, ]
  message(sprintf(
    "  %-22s  paired delta = %+5.3f  [%+5.3f, %+5.3f]  t(%d) = %5.3f  p = %.3g  (%+5.1f%% vs baseline %.2f; n_pairs = %d)",
    r$outcome, r$estimate, r$lower_ci, r$upper_ci,
    as.integer(r$df), r$t_stat, r$p_value,
    100 * r$pct_change, r$baseline_mean, r$n_pairs))
  message(sprintf(
    "    LMM cross-check (class fixed, (1|persuader)): beta = %+5.3f (SE %.3f)  p = %.3g",
    r$lmm_estimate, r$lmm_se, r$lmm_p_value))
}

message("\n  done.")
