# _setup.R
# Sourced at the top of every analysis / figure / table script. Centralises
# library loads, project paths, and runtime options so each downstream
# script can stay focused on its own model / figure / table.

# ---- libraries ----------------------------------------------------------
suppressPackageStartupMessages({
  library(here)        # project-relative paths
  library(jsonlite)    # read data/*.jsonl
  library(tibble)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(forcats)
  library(readr)
  library(lme4)        # linear mixed-effects models
  library(lmerTest)    # p-values for lme4
  library(emmeans)     # estimated marginal means + contrasts
  library(broom)
  library(broom.mixed)
  library(ggplot2)
  library(scales)
  library(gt)          # SI tables
})

# ---- project paths ------------------------------------------------------
PROJ_ROOT       <- here::here()
DATA            <- file.path(PROJ_ROOT, "data")
OUTPUT          <- file.path(PROJ_ROOT, "output")
RESULTS         <- file.path(OUTPUT, "results")
RESULTS_DATA    <- file.path(RESULTS, "_data")
RESULTS_PY      <- file.path(RESULTS, "_py")   # CSVs written by code/analysis/py/*.py
FIGURES_MAIN    <- file.path(OUTPUT, "figures", "main")
FIGURES_SI      <- file.path(OUTPUT, "figures", "si")
TABLES_MAIN     <- file.path(OUTPUT, "tables", "main")
TABLES_SI       <- file.path(OUTPUT, "tables", "si")

for (p in c(RESULTS, RESULTS_DATA, RESULTS_PY, FIGURES_MAIN, FIGURES_SI, TABLES_MAIN, TABLES_SI)) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

# ---- runtime options ----------------------------------------------------
options(
  dplyr.summarise.inform = FALSE,
  readr.show_col_types   = FALSE,
  scipen                 = 999
)

# Canonical study IDs in the order they appear in the paper.
STUDY_IDS <- c("study_0", "study_1", "study_2", "study_3", "study_4", "study_1_practice")

# Persuader-class display order across plots/tables. Drives factor levels in
# every script that pivots on persuader_class, so axis ordering stays uniform.
# Levels are the exact persuader_class strings recorded in the data.
PERSUADER_CLASS_ORDER <- c(
  "control",
  "tournament",            # study 0
  "random_lay_person",     # study 1
  "elite_lay_person",      # study 1
  "canvasser",             # studies 3, 4
  "elite_debater",         # study 1
  "coached_elite_debater", # study 2
  "ai"
)

PERSUADER_CLASS_LABELS <- c(
  control                  = "Control",
  tournament               = "Tournament",
  random_lay_person        = "Random Laypeople",
  elite_lay_person         = "Selected Laypeople",
  canvasser                = "Professional Canvassers",
  elite_debater            = "Elite Debaters",
  coached_elite_debater    = "Coached Elite Debaters",
  ai                       = "AI"
)

# ---- small helpers ------------------------------------------------------
# save_rds(): always write to output/results/<name>.rds, never elsewhere.
save_rds <- function(object, name) {
  stopifnot(is.character(name), length(name) == 1L)
  path <- file.path(RESULTS, paste0(name, ".rds"))
  saveRDS(object, path)
  message(sprintf("  saved %s (%s)", path, format(object.size(object), units = "auto")))
  invisible(path)
}

load_rds <- function(name) {
  path <- file.path(RESULTS, paste0(name, ".rds"))
  if (!file.exists(path)) {
    stop(sprintf("Missing RDS: %s. Run the analysis script that produces it.", path))
  }
  readRDS(path)
}

# complete_case_filter(): apply the project-standard CONSORT-style filter
# (assigned -> matched -> complete-case) and print a per-call audit line.
# `df` must have `successfully_matched` and `attrited` columns; when
# `require_attitudes` is TRUE (the default) it must also have `pre_attitude`
# and `post_attitude`. Filter order (also documented in data/README.md
# and code/analysis/README.md):
#   1. drop pre-treatment dropouts        (!successfully_matched)
#   2. drop post-treatment attrition      (attrited)
#   3. drop residual missing-outcome rows (pre_attitude / post_attitude NA)
# Step 1 removes participants who never cleared matchmaking but may still
# carry a recorded post-attitude, so they must not enter any outcome model.
#
# Used by the persuasive-effect / mechanism / donation analyses; NOT by
# si_03_attrition.R (which operates on the matched subset, attrition being
# its outcome) or _load_data.R (which keeps the raw source tibble).
#
# Audit line:
#   "[<label>] N assigned = N0 -> matched = N1 (-D dropouts)
#                            -> CC = N2 (-A attrited, -P pre NA, -Q post NA)"
complete_case_filter <- function(df, require_attitudes = TRUE,
                                 label = "complete-case") {
  n_raw   <- nrow(df)
  n_drop  <- sum(!dplyr::coalesce(df$successfully_matched, FALSE))
  matched <- df |> dplyr::filter(dplyr::coalesce(successfully_matched, FALSE))
  n_match <- nrow(matched)
  n_attr  <- sum(dplyr::coalesce(matched$attrited, FALSE))
  cc      <- matched |> dplyr::filter(!dplyr::coalesce(attrited, FALSE))
  if (require_attitudes) {
    n_pre_na  <- sum(is.na(cc$pre_attitude))
    n_post_na <- sum(is.na(cc$post_attitude))
    cc <- cc |> dplyr::filter(!is.na(pre_attitude), !is.na(post_attitude))
  } else {
    n_pre_na <- 0L
    n_post_na <- 0L
  }
  message(sprintf(
    "  [%s] N assigned = %d -> matched = %d (-%d dropouts) -> CC = %d (-%d attrited, -%d pre NA, -%d post NA)",
    label, n_raw, n_match, n_drop, nrow(cc), n_attr, n_pre_na, n_post_na))
  cc
}

# ---- Elite-debater years of debating (free-text self-report) -------------
# Shared by 99_extract_numbers.R, tab_persuader_demographics.R and
# code/redact_data.R, which ships the parsed value (idempotent on it).
# Years parsing rule: prefer bare-numeric self-reports; for entries that
# combine "X years university, Y in school" or include school years
# explicitly, use the total years of debating including school. This is
# the rule that produces the manuscript's headline mean of 8.9 (a
# first-number-only rule yields 8.8). Both rules are documented here so
# any future change can be made deliberately.
parse_debating_years <- function(s) {
  if (is.na(s)) return(NA_real_)
  s <- trimws(as.character(s))
  if (s == "") return(NA_real_)
  # Try strict numeric parse first (handles "10", "10.", "5.5").
  num <- suppressWarnings(as.numeric(sub("[^0-9.].*$", "", s)))
  if (!is.na(num) && grepl("^[0-9.]+\\s*$", s)) return(num)
  if (!is.na(num) && grepl("^[0-9.]+\\s*\\.\\s*$", s)) return(num)
  # Free-text cases. Pull out all integer-ish tokens.
  nums <- as.numeric(stats::na.omit(unlist(regmatches(
    s, gregexpr("[0-9]+(?:\\.[0-9]+)?", s)))))
  nums <- nums[!is.na(nums) & nums > 0 & nums < 50]
  if (length(nums) == 0L) return(NA_real_)
  low <- tolower(s)
  has_uni    <- grepl("universit|college|undergrad", low)
  has_school <- grepl("\\bschool\\b|high school|high.?school", low)
  # "4 university, 6 in school" -> sum the first two numbers (explicit split).
  # Requires both keywords AND at least 2 numbers.
  if (has_uni && has_school && length(nums) >= 2L) return(sum(nums[1:2]))
  # Otherwise (incl. "15 (including school and university)" /
  # "9 years including 2 years of high school" / "12 years.") -> take
  # the first number, which the respondent reported as the total.
  nums[1]
}
