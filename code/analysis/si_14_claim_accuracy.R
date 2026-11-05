# si_14_claim_accuracy.R
# =============================================================================
# Accurate vs inaccurate claims, Studies 1-2 (the Fig. 3b sample).
#
#   (A) Does the number of accurate claims predict persuasive impact as well
#       as the total number of claims? Same 13 conditions and same OLS on
#       log10(mean claims per conversation) as Fig. 3b, with the predictor
#       swapped for the mean number of all, accurate and inaccurate claims.
#   (B) Can persuadees detect inaccuracy? Conversation-level Pearson
#       correlations of the "lied to me or made things up" rating with the
#       share and number of inaccurate claims, and of the "I learned a lot"
#       rating with the number of accurate and of inaccurate claims; for all
#       persuaders and separately for AI and human persuaders.
#
# A claim is accurate if its veracity score exceeds 50 (si_09). Claim counts
# come from code/analysis/py/si_14_claim_counts.py (fact-checked
# conversations, veracity-scored persuader claims only).
#
# Outputs (under output/results/, consumer after `->`):
#   claim_accuracy_by_condition.rds -> fig_accurate_claims_vs_impact
#   claim_accuracy_r2.rds           -> fig_accurate_claims_vs_impact
#                                      + 99_extract_numbers (R^2)
#   claim_accuracy_detection.rds    -> tab_accuracy_detection
#                                      + 99_extract_numbers (r, 95% CI)
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))

message("\n==== si_14_claim_accuracy.R ====")

counts <- readr::read_csv(file.path(RESULTS_PY, "si_14_claim_counts.csv"),
                          show_col_types = FALSE)

studies <- load_studies(c("study_1", "study_2"))
conv <- purrr::imap_dfr(studies, ~ dplyr::mutate(.x, study = .y)) |>
  complete_case_filter(label = "si_14") |>
  dplyr::filter(persuader_class != "control") |>
  dplyr::inner_join(counts, by = "row_id") |>
  dplyr::mutate(
    ai = persuader_class == "ai",
    model = dplyr::recode(ai_model_full, "gpt-4o-latest" = "ChatGPT-4o (latest)",
                          "claude-4.1-opus" = "Claude Opus 4.1", "gemini-2.5-pro" = "Gemini 2.5 Pro"),
    condition = dplyr::case_when(
      ai & dplyr::coalesce(ai_constrained, FALSE) ~ paste0(model, " (Constrained)"),
      ai ~ paste0(model, " (Info) ", ifelse(study == "study_1", "S1", "S2")),
      persuader_class == "random_lay_person"     ~ "Random Layperson",
      persuader_class == "elite_lay_person"      ~ "Elite Layperson",
      persuader_class == "elite_debater"         ~ "Elite Debater",
      persuader_class == "coached_elite_debater" ~ "Coached Elite Debater"
    )
  )

# =============================================================================
# (A) Condition level: Fig. 3b with all / accurate / inaccurate claims
# =============================================================================
te <- load_rds("mechanism_facts_vs_te") |>
  dplyr::select(condition, type, study, estimate, lower_ci, upper_ci)
by_cond <- conv |>
  dplyr::group_by(condition) |>
  dplyr::summarise(n = dplyr::n(), claims = mean(n_claims),
                   accurate = mean(n_accurate), inaccurate = mean(n_inaccurate),
                   .groups = "drop") |>
  dplyr::inner_join(te, by = "condition")
stopifnot(nrow(by_cond) == 13L)

r2 <- function(x) summary(lm(by_cond$estimate ~ log10(by_cond[[x]])))$r.squared
cond_r2 <- tibble::tibble(predictor = c("claims", "accurate", "inaccurate"),
                          r_squared = vapply(predictor, r2, numeric(1)))

save_rds(by_cond, "claim_accuracy_by_condition")
save_rds(cond_r2, "claim_accuracy_r2")

# =============================================================================
# (B) Detection of inaccuracy
# =============================================================================
detect_df <- conv |>
  dplyr::filter(n_claims > 0) |>
  dplyr::mutate(share_inaccurate = n_inaccurate / n_claims)
cor_row <- function(d, x, y, subset) {
  ok <- stats::complete.cases(d[[x]], d[[y]])
  ct <- stats::cor.test(d[[x]][ok], d[[y]][ok])
  tibble::tibble(subset, x, y, n = sum(ok), r = unname(ct$estimate),
                 lower_ci = ct$conf.int[1], upper_ci = ct$conf.int[2], p_value = ct$p.value)
}
detect <- dplyr::bind_rows(lapply(list(list(detect_df, "all"),
                                       list(dplyr::filter(detect_df, ai), "ai"),
                                       list(dplyr::filter(detect_df, !ai), "human")),
  function(s) dplyr::bind_rows(
    cor_row(s[[1]], "share_inaccurate", "rating_deception", s[[2]]),
    cor_row(s[[1]], "n_inaccurate",     "rating_deception", s[[2]]),
    cor_row(s[[1]], "n_accurate",       "rating_learning",  s[[2]]),
    cor_row(s[[1]], "n_inaccurate",     "rating_learning",  s[[2]])
  )))

save_rds(detect, "claim_accuracy_detection")

# =============================================================================
# Audit log
# =============================================================================
message("  Condition-level OLS of persuasive impact on log10(mean claims/conv), 13 conditions:")
for (i in seq_len(nrow(cond_r2))) {
  message(sprintf("    %-10s  R^2 = %.3f", cond_r2$predictor[i], cond_r2$r_squared[i]))
}
message("  Detection correlations (conversation level):")
for (i in seq_len(nrow(detect))) {
  r <- detect[i, ]
  message(sprintf("    %-5s  %-16s ~ %-16s  n = %4d  r = %+.3f [%+.3f, %+.3f]",
                  r$subset, r$x, r$y, r$n, r$r, r$lower_ci, r$upper_ci))
}

message("\n  done.")
