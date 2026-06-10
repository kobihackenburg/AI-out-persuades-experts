# tab_persuader_demographics.R
# SI Table: per-class persuader demographics (age, gender, education,
# ideology, debating experience, prep hours). One row per unique
# persuader within a class; Coached Elite Debaters appear as their own
# row even though they overlap as individuals with Elite Debaters.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_persuader_demographics.R ====")

studies <- load_studies(c("study_1", "study_2", "study_3", "study_4"))

bound <- dplyr::bind_rows(
  studies$study_1 |> dplyr::mutate(study = "study_1"),
  studies$study_2 |> dplyr::mutate(study = "study_2"),
  studies$study_3 |> dplyr::mutate(study = "study_3"),
  studies$study_4 |> dplyr::mutate(study = "study_4")
)

humans <- bound |>
  dplyr::filter(persuader_type == "human", !is.na(persuader_id),
                !is.na(persuader_class),
                persuader_class %in% c("random_lay_person", "elite_lay_person",
                                       "canvasser", "elite_debater",
                                       "coached_elite_debater"))

per_p <- humans |>
  dplyr::group_by(persuader_class, persuader_id) |>
  dplyr::summarise(
    age              = dplyr::first(stats::na.omit(persuader_age)),
    gender           = dplyr::first(stats::na.omit(persuader_gender)),
    education        = dplyr::first(stats::na.omit(persuader_education)),
    ideology_text    = dplyr::first(stats::na.omit(persuader_ideology_text)),
    debating_years   = suppressWarnings(as.numeric(
                          dplyr::first(stats::na.omit(persuader_debating_years)))),
    prep_hours       = suppressWarnings(as.numeric(
                          dplyr::first(stats::na.omit(persuader_study_prep_time)))),
    .groups = "drop"
  )

is_centre_right <- function(x) tolower(as.character(x)) %in% c("centre-right", "right")
is_centre_left  <- function(x) tolower(as.character(x)) %in% c("left", "centre-left")
pct <- function(x, pat) 100 * mean(tolower(as.character(x)) %in% pat, na.rm = TRUE)
bach_or_higher <- function(x) {
  tolower(as.character(x)) %in%
    c("bsc/ba", "master's degree", "phd", "bsc / ba", "bachelor",
      "bachelor's degree")
}

summary_df <- per_p |>
  dplyr::group_by(persuader_class) |>
  dplyr::summarise(
    n                = dplyr::n(),
    age_mean         = mean(age, na.rm = TRUE),
    age_sd           = stats::sd(age, na.rm = TRUE),
    pct_women        = pct(gender, c("female", "woman", "f")),
    pct_bach_plus    = 100 * mean(bach_or_higher(education), na.rm = TRUE),
    pct_left         = 100 * mean(is_centre_left(ideology_text), na.rm = TRUE),
    pct_right        = 100 * mean(is_centre_right(ideology_text), na.rm = TRUE),
    debating_years   = if (any(!is.na(debating_years))) mean(debating_years, na.rm = TRUE) else NA_real_,
    prep_hours       = if (any(!is.na(prep_hours))) mean(prep_hours, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) |>
  dplyr::mutate(class_order = match(as.character(persuader_class),
                                    PERSUADER_CLASS_ORDER)) |>
  dplyr::arrange(class_order)

tbl_df <- summary_df |>
  dplyr::transmute(
    Class           = unname(PERSUADER_CLASS_LABELS[as.character(persuader_class)]),
    n               = format(n, big.mark = ","),
    `Age (mean, SD)`= sprintf("%.1f (%.1f)", age_mean, age_sd),
    `% Women`       = sprintf("%.1f", pct_women),
    `% BA+`         = sprintf("%.1f", pct_bach_plus),
    `% Left`        = sprintf("%.1f", pct_left),
    `% Right`       = sprintf("%.1f", pct_right),
    `Debating yrs`  = ifelse(is.na(debating_years), "--", sprintf("%.1f", debating_years)),
    `Prep hrs`      = ifelse(is.na(prep_hours), "--", sprintf("%.1f", prep_hours))
  )

tbl <- gt::gt(tbl_df) |>
  style_table(
    title    = gt::md("**Persuader demographics by class.**"),
    subtitle = "One row per unique persuader; Debating yrs and Prep hrs reported only for the two debater classes."
  )

save_table(tbl, "tab_persuader_demographics", TABLES_SI,
           label  = "tab:persuader_demographics",
           layout = "auto")

tex_path <- file.path(TABLES_SI, "tab_persuader_demographics.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\textbf\\{Age \\(mean, SD\\)\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Age \\\\\\\\ (mean, SD)}}}", txt)
txt <- gsub("\\\\textbf\\{\\\\% Women\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{\\\\% \\\\\\\\ Women}}}", txt)
txt <- gsub("\\\\textbf\\{\\\\% BA\\+\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{\\\\% \\\\\\\\ BA+}}}", txt)
txt <- gsub("\\\\textbf\\{\\\\% Left\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{\\\\% \\\\\\\\ Left}}}", txt)
txt <- gsub("\\\\textbf\\{\\\\% Right\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{\\\\% \\\\\\\\ Right}}}", txt)
txt <- gsub("\\\\textbf\\{Debating yrs\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Debating \\\\\\\\ yrs}}}", txt)
txt <- gsub("\\\\textbf\\{Prep hrs\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Prep \\\\\\\\ hrs}}}", txt)
txt <- gsub("\\\\begin\\{table\\}\\[!t\\]", "\\\\begin{table}[H]", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
