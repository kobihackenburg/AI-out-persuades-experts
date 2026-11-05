# tab_conviction_marker_means.R
# SI Table: mean density per 100 persuader words of Schoenegger et al.'s
# conviction markers (maximizers, boosters, hedges), unconstrained AI vs
# human persuaders, fact-checked Studies 1-2 conversations.
# Reads RDS written by code/analysis/si_19_conviction_markers.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_conviction_marker_means.R ====")

mm <- load_rds("conviction_marker_means")
ai    <- dplyr::filter(mm, persuader == "ai")
human <- dplyr::filter(mm, persuader == "human")

wide <- tibble::tibble(
  Marker = c("Maximizers", "Boosters", "Hedges", "Conversations"),
  AI = c(sprintf("%.2f", c(ai$maximizers, ai$boosters, ai$hedges)),
         format(ai$n, big.mark = ",")),
  `Human persuaders` = c(sprintf("%.2f", c(human$maximizers, human$boosters, human$hedges)),
                         format(human$n, big.mark = ","))
)

subtitle <- paste0(
  "Mean markers per 100 persuader words: maximizers (e.g. \"completely\"), boosters (e.g. \"very\") ",
  "and hedges (e.g. \"perhaps\"), using the word lists in Appendix E of Schoenegger et al. ",
  "Case-insensitive whole-word matching; a multi-word phrase counts once. Sample as in the ",
  "AI-vs-human claim-count analysis: fact-checked Studies 1-2 conversations with at least one claim, ",
  "unconstrained AI and all human classes."
)

tbl <- gt::gt(wide) |>
  style_table(
    title    = gt::md("**Linguistic markers of conviction, AI vs human persuaders.**"),
    subtitle = subtitle
  )

save_table(tbl, "tab_conviction_marker_means", TABLES_SI,
           label  = "tab:conviction_marker_means",
           new    = TRUE)

message("  done.")
