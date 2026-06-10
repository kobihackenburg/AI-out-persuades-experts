# tab_tournament_overview.R
# SI Table: Study 0 four-round persuasion tournament overview --
# per-round persuader, persuadee, and conversation counts.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_tournament_overview.R ====")

s0 <- load_study("study_0")

per_round <- s0 |>
  dplyr::filter(!is.na(tournament_round)) |>
  dplyr::group_by(tournament_round) |>
  dplyr::summarise(
    n_conversations = dplyr::n(),
    n_persuaders    = dplyr::n_distinct(persuader_id, na.rm = TRUE),
    n_persuadees    = dplyr::n_distinct(persuadee_id, na.rm = TRUE),
    .groups = "drop"
  )

totals <- tibble::tibble(
  tournament_round = "Total (Rounds 1-4)",
  n_conversations  = sum(per_round$n_conversations),
  n_persuaders     = dplyr::n_distinct(s0$persuader_id, na.rm = TRUE),
  n_persuadees     = dplyr::n_distinct(s0$persuadee_id, na.rm = TRUE)
)

advance <- tibble::tibble(
  tournament_round = "Selected Laypeople (advanced to Study 1)",
  n_conversations  = NA_integer_,
  n_persuaders     = 87L,
  n_persuadees     = NA_integer_
)

tbl_df <- dplyr::bind_rows(per_round, totals, advance) |>
  dplyr::transmute(
    Round              = tournament_round,
    Conversations      = ifelse(is.na(n_conversations), "--",
                                format(n_conversations, big.mark = ",")),
    `Unique persuaders`= format(n_persuaders, big.mark = ","),
    `Unique persuadees`= ifelse(is.na(n_persuadees), "--",
                                format(n_persuadees, big.mark = ","))
  )

tbl <- gt::gt(tbl_df) |>
  style_table(
    title    = gt::md("**Study 0 persuasion tournament -- per-round counts.**"),
    subtitle = "Four-round single-elimination-style tournament. Full advancement criteria are described in the SI text above this table."
  )

save_table(tbl, "tab_tournament_overview", TABLES_SI,
           label  = "tab:tournament_overview",
           layout = "auto")

tex_path <- file.path(TABLES_SI, "tab_tournament_overview.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
txt <- gsub("\\\\textbf\\{Unique persuaders\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Unique \\\\\\\\ persuaders}}}", txt)
txt <- gsub("\\\\textbf\\{Unique persuadees\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Unique \\\\\\\\ persuadees}}}", txt)
txt <- gsub("\\\\begin\\{table\\}\\[!t\\]", "\\\\begin{table}[H]", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
