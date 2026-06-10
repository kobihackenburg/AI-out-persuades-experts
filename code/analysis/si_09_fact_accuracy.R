# si_09_fact_accuracy.R
# SI: factual accuracy of persuader claims. The Stage-2 web-search verifier
# scores every extracted claim on a 0-100 veracity scale (0 = completely
# inaccurate, 100 = completely accurate). Following Hackenburg et al. (2025),
# a claim is counted as "accurate" if its veracity score exceeds 50.
#
# Computes, per condition and per study: the number of fact-checked
# conversations, the total number of extracted claims, the mean claims per
# conversation, the proportion of claims rated accurate (>50), and the mean
# veracity. Per-condition accuracy is joined to the per-condition persuasive-
# impact estimates (canvasser_facts_vs_te, the S1-S3 facts-vs-impact table
# built in 02_mechanism.R) to back the SI accuracy-vs-impact figure.
#
# NOTE ON DATA SOURCE: per-claim veracity scores live ONLY in the per-claim
# fact-check CSVs under data/fact_checks/ (the study_X.jsonl files store just a
# per-session mean). This is the one analysis script that reads those CSVs
# directly; every other script reads data/*.jsonl via _load_data.R. Factual
# accuracy is an intrinsic property of the persuader's messages, so we
# aggregate over every fact-checked claim in the shipped fact-check dataset
# (file-level), independent of outcome attrition. Each CSV file already
# corresponds to one (study, role), which maps one-to-one onto the condition
# labels used by the facts figure.
#
# Writes (output/results/):
#   fact_accuracy_by_condition.rds  -> per condition, joined to TE estimate
#   fact_accuracy_by_study.rds      -> per-study summary statistics

source(here::here("code/analysis/_setup.R"))

message("\n==== si_09_fact_accuracy.R ====")

FACT_CHECK_ROOT    <- file.path(DATA, "fact_checks")
ACCURATE_THRESHOLD <- 50  # veracity_score > 50 counts as "accurate"

# ---- (study, role) -> condition label used in canvasser_facts_vs_te -----
# The model versions differ by study (e.g. Claude is Opus 4.1 in S1/S2 but
# Opus 4.6 in S3/S4), so model labels are resolved per study, matching
# ai_model_label() in 02_mechanism.R.
model_full_label <- function(study, model_key) {
  if (study %in% c("study_1", "study_2")) {
    dplyr::case_when(
      model_key == "claude"  ~ "Claude Opus 4.1",
      model_key == "chatgpt" ~ "ChatGPT-4o (latest)",
      model_key == "gemini"  ~ "Gemini 2.5 Pro",
      TRUE                   ~ NA_character_
    )
  } else if (study == "study_3") {
    dplyr::case_when(
      model_key == "claude" ~ "Claude Opus 4.6",
      model_key == "gpt"    ~ "GPT-5.4",
      model_key == "grok"   ~ "Grok 4.20",
      TRUE                  ~ NA_character_
    )
  } else if (study == "study_4") {
    dplyr::case_when(model_key == "claude" ~ "Claude Opus 4.6",
                     TRUE ~ NA_character_)
  } else {
    NA_character_
  }
}

condition_label <- function(study, role) {
  sx <- toupper(sub("study_", "S", study))
  if (grepl("^constrained_ai_", role)) {
    mk <- sub("^constrained_ai_", "", role)
    return(paste0(model_full_label(study, mk), " (Constrained)"))
  }
  if (grepl("^ai_", role)) {
    mk <- sub("^ai_", "", role)
    return(paste0(model_full_label(study, mk), " (Info) ", sx))
  }
  dplyr::case_when(
    role == "elite_debater"         ~ "Elite Debater",
    role == "elite_lay_person"      ~ "Elite Layperson",
    role == "random_lay_person"     ~ "Random Layperson",
    role == "coached_elite_debater" ~ "Coached Elite Debater",
    role == "canvasser"             ~ "Professional Canvasser",
    TRUE                            ~ NA_character_
  )
}

# ---- aggregate per file (= per study x role) ----------------------------
files <- list.files(FACT_CHECK_ROOT, pattern = "_fact_checks\\.csv$",
                    recursive = TRUE, full.names = TRUE)
stopifnot(length(files) > 0L)

per_file <- purrr::map_dfr(files, function(f) {
  study <- basename(dirname(f))
  role  <- sub("_fact_checks\\.csv$", "", basename(f))
  role  <- sub("_MAIN$", "", role)
  df <- readr::read_csv(f, col_types = readr::cols(.default = readr::col_character()))
  v  <- suppressWarnings(as.numeric(df$veracity_score))
  is_claim <- !is.na(v)
  tibble::tibble(
    study        = study,
    role         = role,
    condition    = condition_label(study, role),
    n_conv       = dplyr::n_distinct(df$chat_session_id),
    n_claims     = sum(is_claim),
    n_accurate   = sum(v[is_claim] > ACCURATE_THRESHOLD),
    sum_veracity = sum(v[is_claim])
  )
})

# ---- per-study summary ---------------------------------------------------
per_study <- per_file |>
  dplyr::group_by(study) |>
  dplyr::summarise(
    n_conv       = sum(n_conv),
    n_claims     = sum(n_claims),
    n_accurate   = sum(n_accurate),
    sum_veracity = sum(sum_veracity),
    .groups      = "drop"
  ) |>
  dplyr::mutate(
    mean_claims_per_conv = n_claims / n_conv,
    prop_accurate        = n_accurate / n_claims,
    mean_veracity        = sum_veracity / n_claims,
    study_label          = paste0("Study ", sub("study_", "", study))
  ) |>
  dplyr::arrange(study)

save_rds(per_study, "fact_accuracy_by_study")

# ---- per-condition accuracy, joined to persuasive impact -----------------
per_condition_raw <- per_file |>
  dplyr::group_by(condition) |>
  dplyr::summarise(
    n_conv       = sum(n_conv),
    n_claims     = sum(n_claims),
    n_accurate   = sum(n_accurate),
    sum_veracity = sum(sum_veracity),
    .groups      = "drop"
  ) |>
  dplyr::mutate(
    prop_accurate = n_accurate / n_claims,
    mean_veracity = sum_veracity / n_claims
  )

# canvasser_facts_vs_te is the S1-S3 facts-vs-impact table (one row per
# condition: AI split by model x study, humans at class level, plus the three
# constrained-AI arms). Joining on it guarantees the accuracy figure plots
# exactly the same condition set as the facts figure, with matching impact
# estimates and colour/shape encodings.
te <- load_rds("canvasser_facts_vs_te") |>
  dplyr::select(condition, type, study, mean_fact_claims,
                estimate, lower_ci, upper_ci)

acc_by_condition <- te |>
  dplyr::left_join(
    per_condition_raw |>
      dplyr::select(condition, n_claims, n_accurate, prop_accurate, mean_veracity),
    by = "condition"
  )

n_unmatched <- sum(is.na(acc_by_condition$prop_accurate))
if (n_unmatched > 0L) {
  warning(sprintf("%d facts-panel condition(s) had no matching fact-check file:\n  %s",
                  n_unmatched,
                  paste(acc_by_condition$condition[is.na(acc_by_condition$prop_accurate)],
                        collapse = "\n  ")))
}

save_rds(acc_by_condition, "fact_accuracy_by_condition")

# ---- audit log -----------------------------------------------------------
message("\n  Per-study fact accuracy:")
for (i in seq_len(nrow(per_study))) {
  r <- per_study[i, ]
  message(sprintf("    %-8s  n_conv = %5d  n_claims = %7d  facts/conv = %5.1f  %% accurate = %4.1f  mean veracity = %4.1f",
                  r$study_label, r$n_conv, r$n_claims, r$mean_claims_per_conv,
                  100 * r$prop_accurate, r$mean_veracity))
}
message("\n  Per-condition fact accuracy (joined to S1-S3 facts panel):")
for (i in seq_len(nrow(acc_by_condition))) {
  r <- acc_by_condition[i, ]
  message(sprintf("    %-32s %-18s  %% accurate = %s",
                  r$condition, r$type,
                  ifelse(is.na(r$prop_accurate), "NA",
                         sprintf("%4.1f (n_claims = %d)", 100 * r$prop_accurate, r$n_claims))))
}
message("  done.")
