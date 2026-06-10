# tab_canvasser_per_issue.R
# SI Table: per-issue AI-vs-pooled-humans contrast refit on the full
# Studies 1-3 frame (i.e. with canvassers included). Canvasser-inclusion
# robustness analogue of tab_per_issue.R; cited from
# SI Section~\ref{si:mechanism-with-canvasser}.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_canvasser_per_issue.R ====")

iv <- load_rds("issue_level_ai_vs_humans")

ISSUE_LABELS <- c(
  historic_objects     = "Return historic objects to other countries",
  benefit_cap          = "Abolish the two-child benefit cap",
  immigration          = "Allow more immigrants",
  monarchy             = "Keep the monarchy",
  assisted_suicide     = "Legalise physician-assisted suicide",
  social_media_ban     = "Ban social media for under-16s",
  ukraine_peace_deal   = "Back Ukraine peace deal (cede territory)",
  protester_penalties  = "Tougher penalties for road-blocking protesters",
  controversial_speech = "Protect controversial speech at universities",
  pension_age          = "Raise the state pension age"
)

tbl_df <- iv |>
  dplyr::arrange(dplyr::desc(estimate)) |>
  dplyr::transmute(
    Issue       = unname(ifelse(is.na(ISSUE_LABELS[as.character(issue_id)]),
                                as.character(issue_id),
                                ISSUE_LABELS[as.character(issue_id)])),
    `n humans`  = n_humans,
    Estimate    = sprintf("%+.2f", estimate),
    SE          = sprintf("%.2f", se),
    `95% CI`    = sprintf("[%+.2f, %+.2f]", lower_ci, upper_ci),
    p           = ifelse(p_value < 0.001, "<.001", sprintf("%.3f", p_value))
  )

n_sig <- sum(iv$p_value < 0.05, na.rm = TRUE)

tbl <- gt::gt(tbl_df) |>
  style_table(
    title    = gt::md("**Per-issue AI advantage over pooled humans -- canvasser-inclusion refit.**"),
    subtitle = sprintf("Per-issue LMM contrast AI - mean(human classes), refit on the full Studies 1-3 frame (5 human classes including Professional Canvassers). Significant at p < .05 on %d of %d issues. Main-text analogue (Studies 1-2 only) is in Table~\\ref{tab:per_issue}.",
                       n_sig, nrow(iv))
  )

save_table(tbl, "tab_canvasser_per_issue", TABLES_SI,
           label  = "tab:canvasser_per_issue",
           layout = "auto")

# Force [H] float placement
local({
  path <- file.path(TABLES_SI, "tab_canvasser_per_issue.tex")
  txt  <- readLines(path)
  txt  <- sub("\\\\begin\\{table\\}\\[!t\\]", "\\\\begin{table}[H]", txt)
  writeLines(txt, path)
})
message("  done.")
