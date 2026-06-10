# si_03_attrition.R
# Preregistered post-treatment attrition + differential-attrition tests.
#
# Definitions:
#   - DENOMINATOR = successfully matched participants only. "Matched" means
#     the participant cleared pre-treatment / matchmaking and was exposed to
#     the assigned condition. Carried on every data row as
#     `successfully_matched`.
#       * Human arms: persuader-side multiplayer chat resolved.
#       * AI / Control arms: persuadee reached Phase3* or `complete`.
#   - NUMERATOR = matched participants who did NOT give us a valid
#     post-treatment attitude (`attrited == TRUE`).
#
# Rows where `successfully_matched == FALSE` are pre-treatment dropouts
# (mostly failed matchmaking in human arms) and are reported in a separate
# `pre_treatment` block for transparency, NOT counted as attrition.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))

message("\n==== si_03_attrition.R ====")

studies <- load_studies(c("study_1", "study_2", "study_3", "study_4"))

# `condition` mirrors `persuader_class` everywhere except Study 2 AI rows,
# which split into `ai_info` vs `ai_constrained`. Used both for the
# per-condition rate table and the differential-attrition chi-square so
# the displayed cells line up with the cells actually being tested.
make_condition <- function(df) {
  cls <- as.character(df$persuader_class)
  ai  <- !is.na(cls) & cls == "ai"
  con <- ifelse(!is.na(df$ai_constrained) & df$ai_constrained,
                "ai_constrained", "ai_info")
  ifelse(ai, con, cls)
}

# ---- Per-study post-treatment attrition (matched-only) -------------------
per_study_summary <- purrr::imap_dfr(studies, ~ {
  df      <- .x
  matched <- df |> dplyr::filter(successfully_matched)
  tibble::tibble(
    study        = .y,
    n_assigned   = nrow(df),
    n_matched    = nrow(matched),
    n_attrited   = sum(matched$attrited, na.rm = TRUE),
    pct_attrited = if (nrow(matched) > 0) mean(matched$attrited, na.rm = TRUE) else NA_real_
  )
})

# ---- Per-condition post-treatment attrition (matched-only) ---------------
# `condition` mirrors `persuader_class` everywhere except S2 AI rows, which
# split into ai_info vs ai_constrained so the displayed per-arm attrition
# rates line up with the cells the chi-square test below actually uses.
per_condition <- purrr::imap_dfr(studies, ~ {
  df <- .x |> dplyr::filter(successfully_matched)
  df$condition <- make_condition(df)
  df |>
    dplyr::group_by(study_id, persuader_class, condition) |>
    dplyr::summarise(
      n            = dplyr::n(),
      n_attrited   = sum(attrited, na.rm = TRUE),
      pct_attrited = mean(attrited, na.rm = TRUE),
      .groups      = "drop"
    ) |>
    dplyr::mutate(study = .y) |>
    dplyr::relocate(study)
})

# ---- Differential-attrition test (matched-only) --------------------------
# Per-study chi-square of attrited vs condition. Null = attrition
# probability does not depend on assigned condition (among matched).
differential_tests <- purrr::imap_dfr(studies, ~ {
  d <- .x |>
    dplyr::filter(successfully_matched,
                  !is.na(persuader_class), !is.na(attrited))
  d$condition <- make_condition(d)
  n_d <- nrow(d)
  if (n_d == 0 || dplyr::n_distinct(d$condition) < 2) {
    return(tibble::tibble(study = .y, chisq = NA_real_, df = NA_integer_,
                          p.value = NA_real_, n = n_d, n_arms = NA_integer_))
  }
  tab  <- table(d$condition, d$attrited)
  test <- suppressWarnings(stats::chisq.test(tab))
  tibble::tibble(study   = .y,
                 chisq   = unname(test$statistic),
                 df      = unname(test$parameter),
                 p.value = test$p.value,
                 n       = n_d,
                 n_arms  = nrow(tab))
})

# ---- Pre-treatment dropout / failed-match transparency block -------------
# Not attrition: reported separately so reviewers can see how many assigned
# participants we lost before any treatment exposure (mostly failed
# matchmaking in human arms).
pre_treatment_per_study <- purrr::imap_dfr(studies, ~ {
  df <- .x
  tibble::tibble(
    study             = .y,
    n_assigned        = nrow(df),
    n_matched         = sum(df$successfully_matched, na.rm = TRUE),
    n_pre_dropout     = sum(!df$successfully_matched, na.rm = TRUE),
    pct_pre_dropout   = mean(!df$successfully_matched, na.rm = TRUE),
    match_rate        = mean(df$successfully_matched, na.rm = TRUE)
  )
})

pre_treatment_per_class <- purrr::imap_dfr(studies, ~ {
  df <- .x
  df |>
    dplyr::group_by(study_id, persuader_class) |>
    dplyr::summarise(
      n_assigned      = dplyr::n(),
      n_matched       = sum(successfully_matched, na.rm = TRUE),
      n_pre_dropout   = sum(!successfully_matched, na.rm = TRUE),
      pct_pre_dropout = mean(!successfully_matched, na.rm = TRUE),
      match_rate      = mean(successfully_matched, na.rm = TRUE),
      .groups         = "drop"
    ) |>
    dplyr::mutate(study = .y) |>
    dplyr::relocate(study)
})

attrition <- list(
  per_study           = per_study_summary,
  per_condition       = per_condition,
  differential_tests  = differential_tests,
  pre_treatment       = list(
    per_study = pre_treatment_per_study,
    per_class = pre_treatment_per_class
  )
)
save_rds(attrition, "attrition")

# ---- Audit log ----------------------------------------------------------
message("\n  Post-treatment attrition (matched only):")
for (i in seq_len(nrow(per_study_summary))) {
  s <- per_study_summary[i, ]
  message(sprintf("    %-9s  matched=%5d / assigned=%5d   attrited=%4d  (%.2f%%)",
                  s$study, s$n_matched, s$n_assigned,
                  s$n_attrited, 100 * s$pct_attrited))
}
message("\n  Differential-attrition chi-square (matched only, Constrained AI split out):")
for (i in seq_len(nrow(differential_tests))) {
  d <- differential_tests[i, ]
  message(sprintf("    %-9s  chi2=%7.3f  df=%d  p=%.4f  n=%d  arms=%d",
                  d$study, d$chisq, d$df, d$p.value, d$n, d$n_arms))
}
message("\n  Pre-treatment dropout (failed match / never exposed):")
for (i in seq_len(nrow(pre_treatment_per_study))) {
  p <- pre_treatment_per_study[i, ]
  message(sprintf("    %-9s  pre_dropout=%5d / assigned=%5d   (match rate %.2f%%)",
                  p$study, p$n_pre_dropout, p$n_assigned, 100 * p$match_rate))
}
message("  done.")
