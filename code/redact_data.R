#!/usr/bin/env Rscript
# redact_data.R
# Build the PUBLIC, de-identified copy of the project's data from the private
# raw data. This is the data-side half of the "one codebase, two repos"
# release workflow (the file-assembly half lives in scripts/build_public.sh).
#
# Design principle: ship ONLY what `make all` actually reads, de-identified.
# The analysis pipeline consumes the raw JSONL exclusively through
# code/analysis/_load_data.R::COLUMN_SPEC, plus a tiny number of extra reads
# enumerated below. Everything else in the raw files (full message
# transcripts, system prompts, open-text reflections, IP/UA metadata, etc.)
# is never read and is dropped here simply by not being whitelisted.
#
# What this script reads (private)      ->  what it writes (public)
#   data/study_{0,1,2,3,4}.jsonl        ->  <TARGET>/data/study_*.jsonl
#   data/fact_checks/**/*_fact_checks.csv  ->  <TARGET>/data/fact_checks/...
#   code/analysis/elite_debater/annotations.json
#                                       ->  <TARGET>/code/analysis/elite_debater/
#                                            {annotations.json, audit.csv}
#   (private) .redact_id_map.csv        <-  reverse map raw_id -> anon_id
#
# De-identification:
#   * Every Prolific participant id and session id is replaced by a stable
#     pseudonym (anon_0000001, ...). The SAME real id maps to the SAME
#     pseudonym everywhere it appears (across studies, across persuadee /
#     persuader roles, in fact-check files, and in the elite-debater bundle),
#     so all within- and cross-study linkage the analyses rely on is
#     preserved while real ids are not published.
#   * The reverse map is written to .redact_id_map.csv in the PRIVATE repo
#     (git-ignored). It is the only artefact that can re-identify rows; never
#     commit or share it.
#
# Free-text fields that ARE whitelisted but would leak detail are handled
# field-by-field rather than shipped verbatim:
#   * persuader debating_years      -> parsed to the numeric value the
#                                      pipeline derives anyway (free text dropped)
#   * persuader study_prep_time     -> parsed to the numeric hours the pipeline
#                                      derives anyway (free text dropped)
#   * persuader debating_achievements -> dropped (only ever read as the coded
#                                      booleans in the elite_debater bundle)
#   * canvasser career_experience_raw -> dropped (only feeds a private audit CSV)
#   * persuader nationality_text     -> normalised to canonical country names
#                                      (preserves debater.countries_n exactly)
# The standardised demographic *_text fields (gender/education/income/
# ethnicity/party/ideology) are low-cardinality category labels (verified
# 3-6 levels each), not free text, and are kept as-is for subgroup analyses.
#
# Usage:
#   Rscript code/redact_data.R [TARGET_ROOT]
#     TARGET_ROOT  public-repo root to populate (default: ../persuasion-4-public)
#
# Re-running is deterministic: identical raw data -> identical pseudonyms and
# identical output, so the public repo can be re-synced from the private one
# at any time.

suppressWarnings(suppressMessages({
  source(here::here("code/analysis/_load_data.R"))  # brings in COLUMN_SPEC, .slice_path, libs
}))

args        <- commandArgs(trailingOnly = TRUE)
TARGET_ROOT <- if (length(args) >= 1L) args[[1L]] else
  normalizePath(file.path(here::here(), "..", "persuasion-4-public"), mustWork = FALSE)
TARGET_DATA <- file.path(TARGET_ROOT, "data")
TARGET_ED   <- file.path(TARGET_ROOT, "code", "analysis", "elite_debater")
MAP_PATH    <- here::here(".redact_id_map.csv")

REDACT_STUDIES <- c("study_0", "study_1", "study_2", "study_3", "study_4")

# Paths to anonymise (participant + session ids + the opaque row key).
ID_PATHS <- c(
  "row_id",
  "persuadee/participant_id", "persuadee/session_id",
  "persuader/participant_id", "persuader/session_id"
)
# Free-text paths to blank entirely (never read at runtime, or only via a
# private audit artefact).
DROP_PATHS <- c(
  "persuader/persuader_pre_survey/extras/debating_achievements",
  "persuader/persuader_pre_survey/canvassing_background/canvassing_experience_raw"
)
YEARS_PATH       <- "persuader/persuader_pre_survey/extras/debating_years"
PREP_PATH        <- "persuader/persuader_pre_survey/extras/study_prep_time"
NATIONALITY_PATH <- "persuader/demographics/nationality_text"  # study_1 only

# ----------------------------------------------------------------------------
# Stable pseudonym map (real id -> anon_NNNNNNN), shared across all inputs.
# ----------------------------------------------------------------------------
.id_map  <- new.env(parent = emptyenv())
.id_seen <- character(0)  # preserves first-seen order for the dumped map
.id_n    <- 0L

anon_one <- function(v) {
  if (is.na(v) || !nzchar(v)) return(NA_character_)
  hit <- .id_map[[v]]
  if (is.null(hit)) {
    .id_n <<- .id_n + 1L
    hit   <- sprintf("anon_%07d", .id_n)
    assign(v, hit, envir = .id_map)
    .id_seen <<- c(.id_seen, v)
  }
  hit
}
anon_vec <- function(x) vapply(as.character(x), anon_one, character(1L), USE.NAMES = FALSE)

# ----------------------------------------------------------------------------
# Field parsers copied verbatim from 99_extract_numbers.R so the redacted data
# reproduces the same derived numbers. Keep in sync if those parsers change.
# ----------------------------------------------------------------------------
parse_debating_years <- function(s) {
  if (is.na(s)) return(NA_real_)
  s <- trimws(as.character(s))
  if (s == "") return(NA_real_)
  num <- suppressWarnings(as.numeric(sub("[^0-9.].*$", "", s)))
  if (!is.na(num) && grepl("^[0-9.]+\\s*$", s)) return(num)
  if (!is.na(num) && grepl("^[0-9.]+\\s*\\.\\s*$", s)) return(num)
  nums <- as.numeric(stats::na.omit(unlist(regmatches(
    s, gregexpr("[0-9]+(?:\\.[0-9]+)?", s)))))
  nums <- nums[!is.na(nums) & nums > 0 & nums < 50]
  if (length(nums) == 0L) return(NA_real_)
  low <- tolower(s)
  has_uni    <- grepl("universit|college|undergrad", low)
  has_school <- grepl("\\bschool\\b|high school|high.?school", low)
  if (has_uni && has_school && length(nums) >= 2L) return(sum(nums[1:2]))
  nums[1]
}

norm_country <- function(s) {
  if (is.na(s) || s == "") return(character(0))
  s <- gsub("&#x2F;", "/", s, fixed = TRUE)
  s <- gsub("&#39;",  "'", s, fixed = TRUE)
  s <- gsub("&amp;",  "&", s, fixed = TRUE)
  s <- gsub("\\([^)]*parents?[^)]*\\)", "", s, ignore.case = TRUE)
  parts <- strsplit(s, "[,/]| and |[-]", perl = TRUE)[[1L]]
  out <- character(0)
  for (p in parts) {
    t <- tolower(trimws(p))
    t <- sub("^(i am a|i am|the)\\s+", "", t)
    t <- sub("[[:space:].]+$", "", t)
    t <- gsub("\\([^)]*\\)", "", t)
    t <- trimws(t)
    if (t == "") next
    key <- switch(
      t,
      "australian"="Australia", "american"="United States",
      "us"="United States", "british"="United Kingdom",
      "uk"="United Kingdom", "english"="United Kingdom",
      "scottish"="United Kingdom", "welsh"="United Kingdom",
      "serbian"="Serbia", "croatian"="Croatia", "romanian"="Romania",
      "german"="Germany", "irish"="Ireland", "spanish"="Spain",
      "swiss"="Switzerland", "bangladeshi"="Bangladesh",
      "canadian"="Canada", "jordanian"="Jordan", "dutch"="Netherlands",
      "filipino"="Philippines", "ghanaian"="Ghana", "greek"="Greece",
      "hong kong sar"="Hong Kong", "south african"="South Africa",
      "new zealander"="New Zealand", "indian"="India",
      "israeli"="Israel", "italian"="Italy", "malaysian"="Malaysia",
      "mexican"="Mexico", "nigerian"="Nigeria", "russian"="Russia",
      "singapore"="Singapore", "singaporean"="Singapore",
      "slovenian"="Slovenia",
      tools::toTitleCase(t)
    )
    out <- c(out, key)
  }
  unique(out)
}

normalise_nationality <- function(x) {
  vapply(x, function(s) {
    cs <- norm_country(s)
    if (length(cs) == 0L) NA_character_ else paste(cs, collapse = ", ")
  }, character(1L), USE.NAMES = FALSE)
}

# ----------------------------------------------------------------------------
# Per-column transform applied before the row is serialised.
# ----------------------------------------------------------------------------
transform_col <- function(path, v) {
  if (path %in% ID_PATHS)   return(anon_vec(v))
  if (path %in% DROP_PATHS) return(rep(NA, length(v)))
  if (path == YEARS_PATH)   return(as.character(vapply(v, parse_debating_years, numeric(1L))))
  if (path == PREP_PATH)    return(as.character(suppressWarnings(as.numeric(trimws(as.character(v))))))
  if (path == NATIONALITY_PATH) return(normalise_nationality(v))
  v
}

# set_in(): assign `value` at a "/"-split nested path within a list.
set_in <- function(lst, parts, value) {
  if (length(parts) == 1L) {
    lst[[parts]] <- value
    return(lst)
  }
  sub <- lst[[parts[[1L]]]]
  if (is.null(sub)) sub <- list()
  lst[[parts[[1L]]]] <- set_in(sub, parts[-1L], value)
  lst
}

# ----------------------------------------------------------------------------
# Redact one study's JSONL.
# ----------------------------------------------------------------------------
redact_study <- function(id) {
  src <- file.path(DATA, paste0(id, ".jsonl"))
  if (!file.exists(src)) {
    message(sprintf("  [skip] %s (not found)", src))
    return(invisible())
  }
  message(sprintf("  reading %s", src))
  con <- file(src, open = "rb")
  raw <- jsonlite::stream_in(con, verbose = FALSE)
  close(con)
  n <- nrow(raw)

  paths <- COLUMN_SPEC$path
  if (id == "study_1") paths <- c(paths, NATIONALITY_PATH)  # debater countries_n only

  colvals <- vector("list", length(paths))
  names(colvals) <- paths
  for (p in paths) {
    v <- .slice_path(raw, p)
    if (is.null(v)) v <- rep(NA, n)
    colvals[[p]] <- transform_col(p, v)
  }

  parts_list <- lapply(paths, function(p) strsplit(p, "/", fixed = TRUE)[[1L]])
  lines <- vapply(seq_len(n), function(i) {
    lst <- list()
    for (k in seq_along(paths)) {
      lst <- set_in(lst, parts_list[[k]], colvals[[k]][i])
    }
    jsonlite::toJSON(lst, auto_unbox = TRUE, na = "null", null = "null", digits = NA)
  }, character(1L))

  dir.create(TARGET_DATA, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(TARGET_DATA, paste0(id, ".jsonl"))
  writeLines(lines, out, useBytes = TRUE)
  message(sprintf("  wrote   %s  (%d rows)", out, n))
}

# ----------------------------------------------------------------------------
# Redact the per-claim fact-check CSVs (si_09 reads chat_session_id + veracity).
# ----------------------------------------------------------------------------
redact_fact_checks <- function() {
  root <- file.path(DATA, "fact_checks")
  if (!dir.exists(root)) {
    message("  [skip] data/fact_checks/ (not found)")
    return(invisible())
  }
  files <- list.files(root, pattern = "_fact_checks\\.csv$",
                      recursive = TRUE, full.names = TRUE)
  message(sprintf("  %d fact-check files", length(files)))
  for (f in files) {
    rel <- sub(paste0("^", root, "/?"), "", f)
    df  <- readr::read_csv(f, col_types = readr::cols(.default = readr::col_character()),
                           progress = FALSE)
    keep <- tibble::tibble(
      chat_session_id = anon_vec(df$chat_session_id),
      veracity_score  = df$veracity_score
    )
    dest <- file.path(TARGET_DATA, "fact_checks", rel)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(keep, dest)
  }
  message(sprintf("  wrote   %s", file.path(TARGET_DATA, "fact_checks")))
}

# ----------------------------------------------------------------------------
# Redact the elite-debater annotation bundle. 99_extract_numbers.R reads only
# audit.csv (row count + the 7 boolean criteria), never the evidence strings
# or ids. We anonymise ids, blank evidence, drop input.json (raw achievements).
# ----------------------------------------------------------------------------
ED_CRITERIA <- c(
  "is_world_champion",
  "is_continental_champion",
  "is_continental_champion_any_division",
  "reached_continental_semis_or_better",
  "reached_major_semis_or_better",
  "is_best_speaker_at_major",
  "is_continental_finalist"
)

redact_elite_debater <- function() {
  src <- here::here("code/analysis/elite_debater/annotations.json")
  if (!file.exists(src)) {
    message("  [skip] elite_debater/annotations.json (not found)")
    return(invisible())
  }
  payload <- jsonlite::read_json(src, simplifyVector = FALSE)
  anns <- payload$annotations
  for (i in seq_along(anns)) {
    anns[[i]]$persuader_id <- anon_one(anns[[i]]$persuader_id)
    for (c in ED_CRITERIA) {
      ev <- paste0(c, "_evidence")
      if (!is.null(anns[[i]][[ev]])) anns[[i]][[ev]] <- ""
    }
  }
  payload$annotations <- anns

  dir.create(TARGET_ED, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(payload, file.path(TARGET_ED, "annotations.json"),
                       auto_unbox = TRUE, pretty = TRUE, null = "null")

  # Rebuild audit.csv from the anonymised annotations (matches annotate.py
  # --from-cache output: persuader_id + per-criterion bool + empty evidence).
  cols <- list(persuader_id = vapply(anns, function(a) a$persuader_id, character(1L)))
  for (c in ED_CRITERIA) {
    cols[[c]] <- vapply(anns, function(a) toupper(as.character(isTRUE(a[[c]]))), character(1L))
    cols[[paste0(c, "_evidence")]] <- rep("", length(anns))
  }
  audit <- tibble::as_tibble(cols)
  audit <- audit[order(audit$persuader_id), , drop = FALSE]
  readr::write_csv(audit, file.path(TARGET_ED, "audit.csv"))
  message(sprintf("  wrote   %s  (%d debaters)",
                  file.path(TARGET_ED, "annotations.json"), length(anns)))
}

# ----------------------------------------------------------------------------
# Run.
# ----------------------------------------------------------------------------
message("==== redact_data.R ====")
message(sprintf("target root: %s", TARGET_ROOT))

message("\n-- studies --")
for (id in REDACT_STUDIES) redact_study(id)

message("\n-- fact checks --")
redact_fact_checks()

message("\n-- elite debater bundle --")
redact_elite_debater()

# Dump the private reverse map (first-seen order).
map_df <- tibble::tibble(
  raw_id  = .id_seen,
  anon_id = vapply(.id_seen, function(k) .id_map[[k]], character(1L), USE.NAMES = FALSE)
)
readr::write_csv(map_df, MAP_PATH)
message(sprintf("\nwrote id map (%d ids) to %s  [PRIVATE - do not commit]",
                nrow(map_df), MAP_PATH))
message("done.")
