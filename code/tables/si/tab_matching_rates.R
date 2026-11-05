# tab_matching_rates.R
# SI Table: share of assigned persuadee sessions that cleared matchmaking, by
# study x condition (Studies 1-3). Reads output/results/matching_rates.rds
# (built by code/analysis/si_23_matching_order.R).

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_matching_rates.R ====")

CONDITION_LABELS <- c(
  PERSUADER_CLASS_LABELS,
  ai_constrained = "AI (Constrained)"
)
CONDITION_ORDER <- c(PERSUADER_CLASS_ORDER, "ai_constrained")

rates <- load_rds("matching_rates") |>
  dplyr::filter(!is.na(group)) |>
  dplyr::mutate(cond_order = match(group, CONDITION_ORDER)) |>
  dplyr::arrange(study, cond_order) |>
  dplyr::transmute(
    Study        = sub("study_", "Study ", study),
    Condition    = unname(CONDITION_LABELS[group]),
    Assigned     = format(assigned, big.mark = ","),
    Matched      = format(matched,  big.mark = ","),
    `% matched`  = sprintf("%.1f", 100 * match_rate)
  )

tbl <- gt::gt(rates, groupname_col = "Study") |>
  style_table(
    title    = gt::md("**Matchmaking success by study and condition (Studies 1--3).**"),
    subtitle = paste0(
      "Assigned is the number of persuadee sessions randomized to a condition; Matched is the subset ",
      "paired with a partner and exposed to treatment. Persuadees in human conditions waited for a free ",
      "human persuader (4-minute timeout) and could re-attempt the study if unmatched; persuadees in AI and ",
      "control conditions were paired with a model.")
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_matching_rates", TABLES_SI,
           label  = "tab:matching_rates",
           layout = "auto",
           new    = TRUE)

tex_path <- file.path(TABLES_SI, "tab_matching_rates.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
