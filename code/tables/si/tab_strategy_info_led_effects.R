# tab_strategy_info_led_effects.R
# SI Table: persuasive effects with human conversations split into
# information-led and non-information-led (preregistered pooled Studies 1-3
# LMM), under the main definition and excluding open questions / reflective
# listening. Effects vs control, then AI and between-strategy contrasts.
# Reads RDS written by code/analysis/si_19_persuader_strategies.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_strategy_info_led_effects.R ====")

ROW_LABELS <- c(
  ai                                 = "AI",
  ai_constrained                     = "Constrained AI",
  human_info_led                     = "Information-led human conversations",
  human_non_info_led                 = "Non-information-led human conversations",
  ai_minus_non_info_led_humans       = "AI minus non-information-led humans",
  ai_minus_info_led_humans           = "AI minus information-led humans",
  non_info_led_minus_info_led_humans = "Non-information-led minus information-led humans"
)

fmt_cell <- function(estimate, lower_ci, upper_ci) {
  sprintf("%+.2f [%+.2f, %+.2f]", estimate, lower_ci, upper_ci)
}

te <- load_rds("strategy_info_led_te") |>
  dplyr::transmute(definition, row = group, panel = "Effect vs control",
                   value = fmt_cell(estimate, lower_ci, upper_ci))
contr <- load_rds("strategy_info_led_contrasts") |>
  dplyr::transmute(definition, row = as.character(contrast), panel = "Contrasts",
                   value = fmt_cell(estimate, lower_ci, upper_ci))

wide <- dplyr::bind_rows(te, contr) |>
  tidyr::pivot_wider(names_from = definition, values_from = value) |>
  dplyr::arrange(match(row, names(ROW_LABELS))) |>
  dplyr::transmute(panel, Condition = unname(ROW_LABELS[row]),
                   `Main definition` = main,
                   `Excluding open questions, reflective listening` = no_deep)

subtitle <- paste0(
  "Pooled Studies 1-3 preregistered LMM (post ~ pre + issue + group + attempt + (1 | persuader) + ",
  "(1 | persuadee)) in which human conversations are split by the persuader's self-reported strategy. ",
  "A conversation is non-information-led if the persuader rated a non-informational strategy higher ",
  "than facts and evidence. Main definition: the non-informational strategies are telling a story, ",
  "framing the issue in terms of the partner's values, and beginning with open questions and ",
  "reflective listening; the last column drops open questions and reflective listening from that set. ",
  "Estimates in pp [95% CI]."
)

tbl <- gt::gt(wide, groupname_col = "panel") |>
  style_table(
    title    = gt::md("**Persuasive effects of information-led and non-information-led human conversations.**"),
    subtitle = subtitle
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_strategy_info_led_effects", TABLES_SI,
           label  = "tab:strategy_info_led_effects",
           layout = "shrink",
           new    = TRUE)

tex_path <- file.path(TABLES_SI, "tab_strategy_info_led_effects.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
txt <- gsub("\\textbf{Excluding open questions, reflective listening}",
            "\\textbf{\\raisebox{-0.5\\height}{\\shortstack{Excluding open questions, \\\\ reflective listening}}}",
            txt, fixed = TRUE)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
