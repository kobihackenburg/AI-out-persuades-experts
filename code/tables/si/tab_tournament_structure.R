# tab_tournament_structure.R
# SI Table: preregistered four-round structure of the Study 0 selection
# tournament. These are the design constants from the Study 0 preregistration
# (conversations required per persuader per round, the target advancement rate,
# and the per-issue advancement quota applied across the 10 policy stances).
# They define the elimination rule that was applied regardless of the realised
# Round 1 pool size; realised per-round counts are in tab_tournament_overview.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_tournament_structure.R ====")

tbl_df <- tibble::tibble(
  Round                          = c("1", "2", "3", "4"),
  `Conversations per persuader`  = c("2", "2", "3", "6"),
  `Target advancement`           = c("Top 80%", "Top 70%", "Top 50%",
                                     "Top ~10% overall"),
  `Advanced per issue`           = c("80", "56", "28", "10")
)

tbl <- gt::gt(tbl_df) |>
  style_table(
    title    = gt::md("**Preregistered structure of the Study 0 selection tournament.**"),
    subtitle = gt::md("Design constants from the Study 0 preregistration. Advancement was applied as an equal per-issue quota across the 10 policy stances, narrowing the Round 1 pool to the most persuasive ~10%. Realised per-round conversation and participant counts are reported in the tournament-overview table.")
  )

save_table(tbl, "tab_tournament_structure", TABLES_SI,
           label  = "tab:tournament_structure",
           layout = "auto")

tex_path <- file.path(TABLES_SI, "tab_tournament_structure.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
txt <- gsub("\\\\textbf\\{Conversations per persuader\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Conversations \\\\\\\\ per persuader}}}", txt)
txt <- gsub("\\\\textbf\\{Advanced per issue\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Advanced \\\\\\\\ per issue}}}", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
