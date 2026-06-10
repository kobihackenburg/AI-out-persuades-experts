# tab_persuadee_demographics.R
# SI Table: per-study persuadee demographics on the final analytic sample
# (one row per unique persuadee per study, deduplicated on persuadee_id).
# Shows the one-number summary for each collected demographic; per-level
# breakdowns for income, ethnicity, education, and party are reported in
# the subgroups table further below.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_persuadee_demographics.R ====")

studies <- load_studies(c("study_1", "study_2", "study_3", "study_4"))

bach_or_higher <- function(x) {
  tolower(as.character(x)) %in%
    c("bsc/ba", "master's degree", "phd", "bsc / ba", "bachelor",
      "bachelor's degree")
}

# Income bracket ordering (ascending). The raw income column in the data ships
# the GBP symbol as UTF-8 bytes (or the printable escape "<U+00A3>"
# depending on locale); we therefore rank brackets by the first integer
# found in each string, which is locale-independent.
INCOME_LADDER_BY_LOW <- c(
  "0"   = 1L,    # "Less than ...20,000"
  "20"  = 2L,    # "...20,000 - ...29,999"
  "30"  = 3L,    # "...30,000 - ...49,999"
  "50"  = 4L,    # "...50,000 - ...74,999"
  "75"  = 5L,    # "...75,000 - ...99,999"
  "100" = 6L     # "...100,000 or more"
)
INCOME_SHORT_BY_IDX <- c(
  "<\u00a320k",
  "\u00a320-29k",
  "\u00a330-49k",
  "\u00a350-74k",
  "\u00a375-99k",
  "\u00a3100k+"
)

income_bracket_idx <- function(x) {
  s <- as.character(x)
  # Strip everything up to the first ASCII digit, then take the leading
  # integer; that's the bracket's lower bound (or 0 for "Less than X").
  has_less <- grepl("Less than", s, ignore.case = TRUE)
  num      <- suppressWarnings(as.integer(sub("^[^0-9]*([0-9]+).*", "\\1", s)))
  key      <- ifelse(has_less, "0", as.character(num))
  unname(INCOME_LADDER_BY_LOW[key])
}

median_income_bracket <- function(income_vec) {
  idx <- income_bracket_idx(income_vec)
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0L) return(NA_character_)
  med <- ceiling(length(idx) / 2)
  INCOME_SHORT_BY_IDX[sort(idx)[med]]
}

per_study <- purrr::imap_dfr(studies, function(df, study_id) {
  outcome_col <- if (study_id == "study_4") "donation_amount" else "post_attitude"
  cc <- df |>
    dplyr::filter(dplyr::coalesce(successfully_matched, FALSE),
                  !dplyr::coalesce(attrited, FALSE),
                  !is.na(.data[[outcome_col]])) |>
    dplyr::distinct(persuadee_id, .keep_all = TRUE)

  tibble::tibble(
    study           = study_id,
    n               = nrow(cc),
    age_mean        = mean(cc$age, na.rm = TRUE),
    age_sd          = stats::sd(cc$age, na.rm = TRUE),
    pct_women       = 100 * mean(tolower(cc$gender) %in% c("female", "woman", "f"), na.rm = TRUE),
    pct_white       = 100 * mean(cc$ethnicity == "White", na.rm = TRUE),
    median_income   = median_income_bracket(cc$income),
    pct_bach_plus   = 100 * mean(bach_or_higher(cc$education), na.rm = TRUE),
    pct_left        = 100 * mean(tolower(cc$ideology_text) %in% c("left", "centre-left"), na.rm = TRUE),
    pct_centre      = 100 * mean(tolower(cc$ideology_text) %in% c("centre/moderate", "centre", "moderate"), na.rm = TRUE),
    pct_right       = 100 * mean(tolower(cc$ideology_text) %in% c("centre-right", "right"), na.rm = TRUE),
    pct_lab         = 100 * mean(tolower(cc$party) %in% c("labour"), na.rm = TRUE),
    pct_con         = 100 * mean(tolower(cc$party) %in% c("conservative"), na.rm = TRUE),
    pct_other_party = 100 * mean(!tolower(cc$party) %in% c("labour", "conservative") &
                                   !is.na(cc$party), na.rm = TRUE)
  )
})

tbl_df <- per_study |>
  dplyr::transmute(
    Study             = sub("study_", "Study ", study),
    n                 = format(n, big.mark = ","),
    `Age (M, SD)`     = sprintf("%.1f (%.1f)", age_mean, age_sd),
    `% Women`         = sprintf("%.1f", pct_women),
    `% White`         = sprintf("%.1f", pct_white),
    `Median income`   = median_income,
    `% BA+`           = sprintf("%.1f", pct_bach_plus),
    `% Left`          = sprintf("%.1f", pct_left),
    `% Centre`        = sprintf("%.1f", pct_centre),
    `% Right`         = sprintf("%.1f", pct_right),
    `% Labour`        = sprintf("%.1f", pct_lab),
    `% Conservative`  = sprintf("%.1f", pct_con),
    `% Other party`   = sprintf("%.1f", pct_other_party)
  )

tbl <- gt::gt(tbl_df) |>
  style_table(
    title    = gt::md("**Persuadee demographics by study.**"),
    subtitle = "Self-reported on the persuadee pre-study survey, on the final analytic sample (deduplicated to one row per persuadee). One-number summary per demographic; per-level breakdowns for income, ethnicity, education, and party are reported in the subgroups table further below."
  )

save_table(tbl, "tab_persuadee_demographics", TABLES_SI,
           label  = "tab:persuadee_demographics",
           layout = "auto")

tex_path <- file.path(TABLES_SI, "tab_persuadee_demographics.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\textbf\\{Age \\(M, SD\\)\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Age \\\\\\\\ (M, SD)}}}", txt)
txt <- gsub("\\\\textbf\\{Median income\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Median \\\\\\\\ income}}}", txt)
txt <- gsub("\\\\textbf\\{\\\\% Women\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{\\\\% \\\\\\\\ Women}}}", txt)
txt <- gsub("\\\\textbf\\{\\\\% White\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{\\\\% \\\\\\\\ White}}}", txt)
txt <- gsub("\\\\textbf\\{\\\\% BA\\+\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{\\\\% \\\\\\\\ BA+}}}", txt)
txt <- gsub("\\\\textbf\\{\\\\% Left\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{\\\\% \\\\\\\\ Left}}}", txt)
txt <- gsub("\\\\textbf\\{\\\\% Centre\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{\\\\% \\\\\\\\ Centre}}}", txt)
txt <- gsub("\\\\textbf\\{\\\\% Right\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{\\\\% \\\\\\\\ Right}}}", txt)
txt <- gsub("\\\\textbf\\{\\\\% Labour\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{\\\\% \\\\\\\\ Labour}}}", txt)
txt <- gsub("\\\\textbf\\{\\\\% Conservative\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{\\\\% \\\\\\\\ Conservative}}}", txt)
txt <- gsub("\\\\textbf\\{\\\\% Other party\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{\\\\% \\\\\\\\ Other party}}}", txt)
txt <- gsub("\\\\begin\\{table\\}\\[!t\\]", "\\\\begin{table}[H]", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
