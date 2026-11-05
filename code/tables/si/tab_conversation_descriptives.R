# tab_conversation_descriptives.R
# SI Table: per-condition conversation descriptive statistics (turns,
# duration, words per persuader / persuadee message, persuader words per
# conversation, and for human persuaders the mean reply time and share of the
# conversation spent awaiting the persuader's reply).
# Reads RDS written by code/analysis/si_12_conversation_descriptives.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_conversation_descriptives.R ====")

per_arm <- load_rds("conversation_descriptives")

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
    `Persuader words per conv` = sprintf("%.0f", persuader_total),
    `Mean reply time (s)` = ifelse(is.na(reply_time_s), "",
                                   sprintf("%.0f", reply_time_s)),
    `Share of conversation awaiting persuader reply` =
      ifelse(is.na(share_awaiting), "", sprintf("%.0f%%", 100 * share_awaiting))
  ) |>
  dplyr::arrange(Study, match(persuader_class, PERSUADER_CLASS_ORDER),
                 dplyr::desc(ai_constrained)) |>
  dplyr::select(Study, Condition, n, Turns, `Duration (min)`,
                `Words per persuader msg`, `Words per persuadee msg`,
                `Persuader words per conv`, `Mean reply time (s)`,
                `Share of conversation awaiting persuader reply`)

# ADDEDBEGIN / ADDEDEND are swapped for \added{...} after gt has escaped the text.
NEW_CAPTION <- paste(
  "ADDEDBEGIN",
  "A turn is one exchange: a persuadee message and the persuader's reply.",
  "Reply time is the interval between a persuadee message and the persuader's reply,",
  "excluding the persuader's prepared opening message.",
  "The share of conversation awaiting persuader reply is the persuader's total time to reply,",
  "including the opening message, divided by conversation duration.",
  "Both are reported for human persuaders only.",
  "ADDEDEND"
)

tbl <- gt::gt(tbl_df, groupname_col = "Study") |>
  style_table(
    title    = gt::md("**Conversation descriptive statistics by study and condition.**"),
    subtitle = paste("Means across complete-case conversations.", NEW_CAPTION)
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
txt <- gsub("\\\\textbf\\{Mean reply time \\(s\\)\\}",
            "\\\\textbf{\\\\added{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Mean reply \\\\\\\\ time (s)}}}}", txt)
txt <- gsub("\\\\textbf\\{Share of conversation awaiting persuader reply\\}",
            "\\\\textbf{\\\\added{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Share of \\\\\\\\ conversation \\\\\\\\ awaiting \\\\\\\\ persuader reply}}}}", txt)
txt <- sub("^\\\\centering$", "\\\\centering\n\\\\setlength{\\\\tabcolsep}{4pt}", txt)
txt <- gsub("ADDEDBEGIN ", "\\\\added{", txt)
txt <- gsub(" ADDEDEND", "}", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
