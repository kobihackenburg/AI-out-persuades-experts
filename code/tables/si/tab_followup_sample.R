# tab_followup_sample.R
# SI Table: follow-up survey sample, return rates and timing by study.
# Studies 1-3 count complete-case conversations on issues the persuadee
# discussed in a single conversation; Study 4 counts the preregistered
# analysis sample of the donation model.
# Reads RDS written by code/analysis/si_13_followup.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_followup_sample.R ====")

DAYS_PER_MONTH <- 365.25 / 12

fmt_n_pct <- function(n, rate) sprintf("%s (%.0f%%)", format(n, big.mark = ","), 100 * rate)
fmt_gap <- function(median, lo, hi) {
  sprintf("%.0f [%.0f, %.0f]", median, lo, hi)
}
fmt_months <- function(days) sprintf("%.1f", days / DAYS_PER_MONTH)

s123 <- load_rds("followup_sample")
s4   <- load_rds("followup_s4_sample")

rows_s123 <- s123 |>
  dplyr::filter(study != "pooled S1-2") |>
  dplyr::transmute(
    Study = dplyr::recode(study, study_1 = "Study 1", study_2 = "Study 2", study_3 = "Study 3",
                          `pooled S1-3` = "Studies 1--3"),
    `Eligible conversations` = format(eligible_rows, big.mark = ","),
    `Re-surveyed conversations` = fmt_n_pct(fu_rows, fu_rate),
    `Eligible persuadees` = format(persuadees, big.mark = ","),
    `Returning persuadees` = fmt_n_pct(fu_persuadees, fu_persuadees / persuadees),
    `Interval, days` = fmt_gap(median_gap_days, min_gap_days, max_gap_days),
    `Median interval, months` = fmt_months(median_gap_days)
  )

# Study 4 has one conversation per persuadee.
rows_s4 <- tibble::tibble(
  Study = "Study 4",
  `Eligible conversations` = format(s4$rows_prereg_covariates, big.mark = ","),
  `Re-surveyed conversations` = fmt_n_pct(s4$fu_rows_prereg_covariates,
                                          s4$fu_rows_prereg_covariates / s4$rows_prereg_covariates),
  `Eligible persuadees` = `Eligible conversations`,
  `Returning persuadees` = `Re-surveyed conversations`,
  `Interval, days` = fmt_gap(s4$median_gap_days, s4$min_gap_days, s4$max_gap_days),
  `Median interval, months` = fmt_months(s4$median_gap_days)
)

tbl <- gt::gt(dplyr::bind_rows(rows_s123, rows_s4)) |>
  style_table(
    title    = gt::md("**Follow-up survey: sample, return rates and timing.**"),
    subtitle = paste0(
      "Studies 1--3: complete-case conversations on issues the persuadee discussed in a single ",
      "conversation (eligible), and those whose persuadee re-rated the issue at follow-up (re-surveyed). ",
      "Study 4: the preregistered analysis sample of the donation model; each persuadee had one ",
      "conversation. Interval is the time from the conversation to the follow-up response among ",
      "re-surveyed conversations, median [range]."
    )
  )

save_table(tbl, "tab_followup_sample", TABLES_SI,
           label  = "tab:followup_sample",
           layout = "shrink",
           new    = TRUE)

# Plain tabular so \resizebox can scale the table to the text width.
tex_path <- file.path(TABLES_SI, "tab_followup_sample.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
stack_header <- function(txt, header, top, bottom) {
  gsub(sprintf("\\\\textbf\\{%s\\}", header),
       sprintf("\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{%s \\\\\\\\ %s}}}", top, bottom),
       txt)
}
txt <- stack_header(txt, "Eligible conversations", "Eligible", "conversations")
txt <- stack_header(txt, "Re-surveyed conversations", "Re-surveyed", "conversations")
txt <- stack_header(txt, "Eligible persuadees", "Eligible", "persuadees")
txt <- stack_header(txt, "Returning persuadees", "Returning", "persuadees")
txt <- stack_header(txt, "Interval, days", "Interval,", "days")
txt <- stack_header(txt, "Median interval, months", "Median interval,", "months")
con <- file(tex_path, open = "w", encoding = "UTF-8")
writeLines(txt, con)
close(con)

message("  done.")
