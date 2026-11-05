# si_12_conversation_descriptives.R
# =============================================================================
# Per-condition conversation descriptives: turns, duration, words per
# message, and (human persuaders only) reply time and the share of the
# conversation spent awaiting the persuader's reply.
#
# A turn is one exchange: a persuadee message and the persuader's reply, so
# turns = n_persuadee_msgs (the unit of the 2-10 turn cap).
#
# Inputs:
#   data/study_{1..4}.jsonl (via _load_data.R)
#   output/results/_py/si_12_reply_times.csv (code/analysis/py/si_12_reply_times.py)
#
# Outputs (under output/results/, consumer after `->`):
#   conversation_descriptives.rds   -> tab_conversation_descriptives
#                                      + 99_extract_numbers (Study 2 mean turns)
#   conversation_median_turns.rds   -> 99_extract_numbers (median turns,
#                                      Studies 1-3 and Study 4)
#
# Sample: matched, non-attrited treatment conversations with at least one
# persuader message and a positive duration.
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))

message("\n==== si_12_conversation_descriptives.R ====")

studies <- load_studies(c("study_1", "study_2", "study_3", "study_4"))

conv <- purrr::imap_dfr(studies, ~ dplyr::mutate(.x, study = .y)) |>
  dplyr::filter(dplyr::coalesce(successfully_matched, FALSE),
                !dplyr::coalesce(attrited, FALSE),
                !is.na(persuader_class), persuader_class != "control",
                !is.na(n_persuader_msgs), n_persuader_msgs > 0L,
                !is.na(conv_duration_s),  conv_duration_s > 0)

per_arm <- conv |>
  dplyr::mutate(
    persuader_wpm = persuader_words / n_persuader_msgs,
    persuadee_wpm = persuadee_words / pmax(n_persuadee_msgs, 1L)
  ) |>
  dplyr::group_by(study, persuader_class, ai_constrained) |>
  dplyr::summarise(
    n_conv          = dplyr::n(),
    turns           = mean(n_persuadee_msgs),
    duration_min    = mean(conv_duration_s) / 60,
    persuader_wpm   = mean(persuader_wpm, na.rm = TRUE),
    persuadee_wpm   = mean(persuadee_wpm, na.rm = TRUE),
    persuader_total = mean(persuader_words, na.rm = TRUE),
    persuadee_total = mean(persuadee_words, na.rm = TRUE),
    .groups = "drop"
  )

reply_path <- file.path(RESULTS_PY, "si_12_reply_times.csv")
if (!file.exists(reply_path)) {
  stop(sprintf("Missing %s. Run code/analysis/py/si_12_reply_times.py first.", reply_path))
}
reply <- readr::read_csv(reply_path) |>
  dplyr::transmute(study, persuader_class, n_conv_timed = n_conv,
                   reply_time_s = reply_gap_s, share_awaiting = persuader_share)

per_arm <- per_arm |>
  dplyr::left_join(reply, by = c("study", "persuader_class"))

# The timestamp sample must be the same conversations as the table's.
timed <- per_arm |> dplyr::filter(!is.na(n_conv_timed))
stopifnot(nrow(timed) == nrow(reply), all(timed$n_conv == timed$n_conv_timed))
per_arm <- per_arm |> dplyr::select(-n_conv_timed)

median_turns <- conv |>
  dplyr::mutate(block = ifelse(study == "study_4", "s4", "s1_s3")) |>
  dplyr::group_by(block) |>
  dplyr::summarise(n_conv = dplyr::n(),
                   median_turns = stats::median(n_persuadee_msgs),
                   .groups = "drop")

print(per_arm |> dplyr::select(study, persuader_class, ai_constrained, n_conv,
                               turns, reply_time_s, share_awaiting), n = Inf)
print(median_turns)

save_rds(per_arm, "conversation_descriptives")
save_rds(median_turns, "conversation_median_turns")

message("  done.")
