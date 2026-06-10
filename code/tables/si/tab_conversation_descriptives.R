# tab_conversation_descriptives.R
# SI Table: per-condition conversation descriptive statistics (turns,
# duration, words per persuader / persuadee message, persuader words per
# conversation). Backs the manuscript's claim that "treatment dialogues
# lasted 6.7 turns and 16 minutes on average".

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_conversation_descriptives.R ====")

studies <- load_studies(c("study_1", "study_2", "study_3", "study_4"))

bound <- dplyr::bind_rows(
  studies$study_1 |> dplyr::mutate(study = "study_1"),
  studies$study_2 |> dplyr::mutate(study = "study_2"),
  studies$study_3 |> dplyr::mutate(study = "study_3"),
  studies$study_4 |> dplyr::mutate(study = "study_4")
)

per_arm <- bound |>
  dplyr::filter(dplyr::coalesce(successfully_matched, FALSE),
                !dplyr::coalesce(attrited, FALSE),
                !is.na(persuader_class),
                !is.na(n_persuader_msgs), n_persuader_msgs > 0L,
                !is.na(conv_duration_s),  conv_duration_s > 0) |>
  dplyr::mutate(
    persuader_wpm = persuader_words / n_persuader_msgs,
    persuadee_wpm = persuadee_words / pmax(n_persuadee_msgs, 1L)
  ) |>
  dplyr::group_by(study, persuader_class, ai_constrained) |>
  dplyr::summarise(
    n_conv          = dplyr::n(),
    # A turn = one persuader message + one persuadee message (two messages),
    # so turns = n_messages / 2 (matches the main text and "6.7 turns" claim).
    turns           = mean(n_messages, na.rm = TRUE) / 2,
    duration_min    = mean(conv_duration_s, na.rm = TRUE) / 60,
    persuader_wpm   = mean(persuader_wpm, na.rm = TRUE),
    persuadee_wpm   = mean(persuadee_wpm, na.rm = TRUE),
    persuader_total = mean(persuader_words, na.rm = TRUE),
    persuadee_total = mean(persuadee_words, na.rm = TRUE),
    .groups = "drop"
  )

CONDITION_LABEL <- function(cls, constrained) {
  base <- unname(PERSUADER_CLASS_LABELS[as.character(cls)])
  base <- ifelse(is.na(base), as.character(cls), base)
  ifelse(cls == "ai" & isTRUE(constrained), "AI (Constrained)", base)
}

tbl_df <- per_arm |>
  dplyr::mutate(
    Study     = sub("study_", "Study ", study),
    Condition = mapply(CONDITION_LABEL, persuader_class, ai_constrained),
    n         = format(n_conv, big.mark = ","),
    Turns     = sprintf("%.1f", turns),
    `Duration (min)`          = sprintf("%.1f", duration_min),
    `Words per persuader msg` = sprintf("%.0f", persuader_wpm),
    `Words per persuadee msg` = sprintf("%.0f", persuadee_wpm),
    `Persuader words per conv` = sprintf("%.0f", persuader_total)
  ) |>
  dplyr::arrange(Study, match(persuader_class, PERSUADER_CLASS_ORDER),
                 dplyr::desc(ai_constrained)) |>
  dplyr::select(Study, Condition, n, Turns, `Duration (min)`,
                `Words per persuader msg`, `Words per persuadee msg`,
                `Persuader words per conv`)

tbl <- gt::gt(tbl_df, groupname_col = "Study") |>
  style_table(
    title    = gt::md("**Conversation descriptive statistics by study and condition.**"),
    subtitle = "Means across complete-case conversations."
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_conversation_descriptives", TABLES_SI,
           label  = "tab:conversation_descriptives",
           layout = "landscape")

tex_path <- file.path(TABLES_SI, "tab_conversation_descriptives.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\textbf\\{Duration \\(min\\)\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Duration \\\\\\\\ (min)}}}", txt)
txt <- gsub("\\\\textbf\\{Words per persuader msg\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Words per \\\\\\\\ persuader msg}}}", txt)
txt <- gsub("\\\\textbf\\{Words per persuadee msg\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Words per \\\\\\\\ persuadee msg}}}", txt)
txt <- gsub("\\\\textbf\\{Persuader words per conv\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Persuader words \\\\\\\\ per conv}}}", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
