# _load_data.R
# Reads data/study_X.jsonl, slices the analysis-relevant fields
# into a wide tibble (one row per persuadee-conversation), and memoises
# the result to output/results/_data/study_X.rds. Subsequent calls just
# readRDS() from the cache.
#
# Implementation notes:
#   * jsonlite::stream_in() returns a data.frame whose non-scalar
#     top-level columns are themselves nested data.frames. We slice into
#     those nested frames by name and assemble one flat tibble. This
#     avoids both (a) row-by-row tibble construction (O(n^2) in R) and
#     (b) jsonlite::flatten() recursively walking the giant
#     conversation.messages list-column (which makes that approach take
#     ~hours on 12K rows / 365MB files).

if (!exists("RESULTS_DATA", inherits = TRUE)) {
  source(here::here("code/analysis/_setup.R"))
}

# Slice spec for load_study(). Each entry maps a destination column to a
# nested-path lookup in the parsed JSONL data.frame. Anything not listed
# here is dropped. Add a row here if you need a new column downstream.
COLUMN_SPEC <- tibble::tribble(
  ~dest,                       ~path,
  # ids + study context
  "row_id",                    "row_id",
  "study_id",                  "study/id",
  "phase_type",                "study/phase_type",
  "tournament_round",          "study/round",
  "issue_pool",                "study/issue_pool",
  # treatment / arm
  "condition",                 "treatment/condition",
  "persuader_class",           "treatment/persuader_class",
  "ai_model",                  "treatment/ai_model",
  "ai_provider",               "treatment/ai_provider",
  "ai_model_full",             "treatment/ai_model_name_full",
  "ai_constrained",            "treatment/ai_constrained",
  "control_topic",             "treatment/control_topic",
  "issue_id",                  "treatment/issue_id",
  "issue_stance",              "treatment/issue_stance",
  "arm_label",                 "treatment/assigned_group_label",
  # persuadee bookkeeping
  "persuadee_id",              "persuadee/participant_id",
  "persuadee_session",         "persuadee/session_id",
  "session_phase",             "persuadee/session_phase",
  "session_started_at",        "persuadee/session_started_at",
  "session_completed_at",      "persuadee/session_completed_at",
  "attrited",                  "persuadee/attrited",
  "successfully_matched",      "persuadee/successfully_matched",
  "persuadee_attempt_number",  "persuadee/persuadee_attempt_number",
  # persuadee demographics
  "age",                       "persuadee/demographics/age",
  "gender",                    "persuadee/demographics/gender_text",
  "education",                 "persuadee/demographics/education_text",
  "income",                    "persuadee/demographics/income_text",
  "ethnicity",                 "persuadee/demographics/ethnicity_text",
  "party",                     "persuadee/demographics/party_affiliation_text",
  "ideology_coded",            "persuadee/demographics/ideology_coded",
  "ideology_text",             "persuadee/demographics/ideology_text",
  "ai_trust",                  "persuadee/demographics/ai_trust",
  "ai_trust_high",             "persuadee/demographics/ai_trust_high",
  # persuadee psychometrics + knowledge
  "political_knowledge_n",     "persuadee/political_knowledge/political_knowledge_n_correct",
  "political_knowledge_hi",    "persuadee/political_knowledge/political_knowledge_high",
  "empathic_trust",            "persuadee/psych_measures/empathic_trust_average",
  "dogmatism",                 "persuadee/psych_measures/dogmatism_average",
  # pre/post attitudes
  "pre_attitude",              "persuadee/pre_treatment/pre_attitude_average",
  "post_attitude",             "persuadee/post_treatment/post_attitude_average",
  "pre_importance",            "persuadee/pre_treatment/pre_importance",
  "post_importance",           "persuadee/post_treatment/post_importance",
  "pre_issue_knowledge",       "persuadee/pre_treatment/pre_issue_knowledge_overall",
  # Study 4 donation covariate (preregistered LMM control). NA for S1-S3.
  "pre_donation_willingness",  "persuadee/pre_treatment/pre_donation_willingness",
  # partner-rating items (Studies 1-3 shared)
  "rating_enjoyment",          "persuadee/post_treatment/post_conversation_enjoyment",
  "rating_learning",           "persuadee/post_treatment/post_conversation_learning",
  "rating_arguments",          "persuadee/post_treatment/post_conversation_arguments",
  "rating_bias",               "persuadee/post_treatment/post_conversation_bias",
  "rating_deception",          "persuadee/post_treatment/post_conversation_deception",
  "rating_empathy",            "persuadee/post_treatment/post_conversation_empathy",
  "rating_anthropo",           "persuadee/post_treatment/post_conversation_anthropomorphism",
  # persuader bookkeeping
  "persuader_type",            "persuader/type",
  "persuader_id",              "persuader/participant_id",
  "persuader_session",         "persuader/session_id",
  # persuader demographics (NA when persuader is AI / control / not collected)
  "persuader_age",             "persuader/demographics/age",
  "persuader_gender",          "persuader/demographics/gender_text",
  "persuader_education",       "persuader/demographics/education_text",
  "persuader_ethnicity",       "persuader/demographics/ethnicity_text",
  "persuader_party",           "persuader/demographics/party_affiliation_text",
  "persuader_ideology_coded",  "persuader/demographics/ideology_coded",
  "persuader_ideology_text",   "persuader/demographics/ideology_text",
  # persuader-side debater experience (Study 1 Elite Debaters only).
  # `debating_years` is free text; numeric parsing rules + free-text imputation
  # live in code/analysis/99_extract_numbers.R (parse_debating_years()).
  # `debating_achievements` is free text describing championship/finals/semis
  # claims; binary codes against the rubric in code/analysis/elite_debater/rubric.md
  # are produced by code/analysis/elite_debater/annotate.py and stored in
  # code/analysis/elite_debater/audit.csv.
  "persuader_debating_years",        "persuader/persuader_pre_survey/extras/debating_years",
  "persuader_debating_achievements", "persuader/persuader_pre_survey/extras/debating_achievements",
  "persuader_study_prep_time",       "persuader/persuader_pre_survey/extras/study_prep_time",
  # persuader-side career experience (Studies 3 & 4 Professional Canvassers
  # only). The PRE-survey items canvassing-{conversations,funds-raised,years}
  # are free-text; the *_num columns are parsed numeric values, and the *_raw
  # column carries the original free-text experience description for audit.
  "persuader_career_conversations", "persuader/persuader_pre_survey/canvassing_background/canvassing_conversations_num",
  "persuader_career_funds_raised",  "persuader/persuader_pre_survey/canvassing_background/canvassing_funds_raised_num",
  "persuader_career_years",         "persuader/persuader_pre_survey/canvassing_background/canvassing_years_num",
  "persuader_career_experience_raw","persuader/persuader_pre_survey/canvassing_background/canvassing_experience_raw",
  # conversation stats
  "n_messages",                "conversation/message_stats/n_messages",
  "n_persuader_msgs",          "conversation/message_stats/n_persuader_messages",
  "n_persuadee_msgs",          "conversation/message_stats/n_persuadee_messages",
  "persuader_words",           "conversation/message_stats/persuader_n_words_total",
  "persuadee_words",           "conversation/message_stats/persuadee_n_words_total",
  "conv_duration_s",           "conversation/duration_seconds",
  # fact check
  "fact_checked",              "fact_check_summary/checked",
  "n_fact_claims",             "fact_check_summary/n_total_claims",
  "mean_fact_veracity",        "fact_check_summary/mean_claim_veracity",
  # outcomes
  "attitude_change",           "outcomes/primary_attitude_change_signed",
  "attitude_change_absolute",  "outcomes/primary_attitude_change_absolute",
  "donation_amount",           "outcomes/donation_amount",
  # Study 4 donation-mechanism battery (14 items + average). The source
  # field is `study_specific.mechanism_self_report.mechanism_*` at the
  # top of the JSONL row -- NOT under `persuadee`. NA for S1-S3 (those
  # studies don't ship a mechanism battery in the data). Short
  # `mech_*` prefix mirrors the existing `rating_*` convention for the
  # partner-perception items.
  "mech_advocacy",       "study_specific/mechanism_self_report/mechanism_advocacy",
  "mech_commitment",     "study_specific/mechanism_self_report/mechanism_commitment",
  "mech_consistency",    "study_specific/mechanism_self_report/mechanism_consistency",
  "mech_disappointment", "study_specific/mechanism_self_report/mechanism_disappointment",
  "mech_emotion",        "study_specific/mechanism_self_report/mechanism_emotion",
  "mech_empathy",        "study_specific/mechanism_self_report/mechanism_empathy",
  "mech_followthrough",  "study_specific/mechanism_self_report/mechanism_followthrough",
  "mech_identity",       "study_specific/mechanism_self_report/mechanism_identity",
  "mech_impact",         "study_specific/mechanism_self_report/mechanism_impact",
  "mech_knowledge",      "study_specific/mechanism_self_report/mechanism_knowledge",
  "mech_learning",       "study_specific/mechanism_self_report/mechanism_learning",
  "mech_mental_image",   "study_specific/mechanism_self_report/mechanism_mental_image",
  "mech_prior_decision", "study_specific/mechanism_self_report/mechanism_prior_decision",
  "mech_regret",         "study_specific/mechanism_self_report/mechanism_regret",
  "mech_average",        "study_specific/mechanism_self_report/mechanism_average"
)


# .slice_path(): walk a "/"-separated nested-data.frame path, returning
# the leaf vector or NULL if any step is missing.
.slice_path <- function(raw_df, path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  cur   <- raw_df
  for (p in parts) {
    if (is.null(cur) || !p %in% names(cur)) return(NULL)
    cur <- cur[[p]]
  }
  cur
}


# load_study(): load a single study's cleaned JSONL into a tibble, with
# RDS memoisation. `refresh = TRUE` forces a rebuild.
load_study <- function(id, refresh = FALSE) {
  stopifnot(is.character(id), length(id) == 1L)
  cache_path <- file.path(RESULTS_DATA, paste0(id, ".rds"))
  jsonl_path <- file.path(DATA, paste0(id, ".jsonl"))

  if (!refresh && file.exists(cache_path)) {
    message(sprintf("[load_study] cache hit  %s", cache_path))
    return(readRDS(cache_path))
  }

  if (!file.exists(jsonl_path)) {
    stop(sprintf("Data not found: %s.", jsonl_path))
  }

  message(sprintf("[load_study] reading    %s", jsonl_path))
  t0 <- Sys.time()
  con <- file(jsonl_path, open = "rb")
  on.exit(close(con), add = TRUE)
  raw <- jsonlite::stream_in(con, verbose = FALSE)

  # Slice each requested path; missing columns become NA so downstream
  # scripts always see a stable column set across studies.
  n_rows <- nrow(raw)
  cols   <- vector("list", nrow(COLUMN_SPEC))
  names(cols) <- COLUMN_SPEC$dest
  missing_paths <- character()
  for (i in seq_len(nrow(COLUMN_SPEC))) {
    val <- .slice_path(raw, COLUMN_SPEC$path[i])
    if (is.null(val)) {
      cols[[i]] <- rep(NA, n_rows)
      missing_paths <- c(missing_paths, COLUMN_SPEC$path[i])
    } else {
      cols[[i]] <- val
    }
  }
  if (length(missing_paths) > 0L) {
    message(sprintf("[load_study] %d of %d mapped paths missing in %s (filled NA): %s",
                    length(missing_paths), nrow(COLUMN_SPEC), id,
                    paste(missing_paths[seq_len(min(5L, length(missing_paths)))],
                          collapse = ", ")))
  }
  df <- tibble::as_tibble(cols)

  # Stable factor for persuader_class so downstream models / plots get the
  # same axis ordering everywhere without each script re-specifying it.
  if ("persuader_class" %in% names(df)) {
    levels_present <- intersect(PERSUADER_CLASS_ORDER, unique(df$persuader_class))
    df$persuader_class <- factor(df$persuader_class, levels = levels_present)
  }
  if ("issue_id" %in% names(df)) {
    df$issue_id <- factor(df$issue_id)
  }

  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(df, cache_path)
  message(sprintf("[load_study] cached     %s  (%d rows x %d cols, %.1fs)",
                  cache_path, nrow(df), ncol(df),
                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  df
}

# load_studies(): convenience to load + bind several studies at once.
load_studies <- function(ids, refresh = FALSE) {
  out <- lapply(ids, load_study, refresh = refresh)
  names(out) <- ids
  out
}
