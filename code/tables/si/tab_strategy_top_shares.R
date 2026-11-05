# tab_strategy_top_shares.R
# SI Table: share of conversations in which each strategy was the human
# persuader's top-rated one, averaged within persuader and then across the
# persuaders in a class (Studies 1-3), with permutation tests of the class
# difference in strategy mix vs Random Laypeople.
# Reads RDS written by code/analysis/si_19_persuader_strategies.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_strategy_top_shares.R ====")

CLASS_COLS <- c("Random Laypeople", "Selected Laypeople", "Elite Debaters",
                "Coached Elite Debaters", "Professional Canvassers")

fmt_p <- function(p) ifelse(p < 0.001, "<.001", sprintf("%.3f", p))

shares  <- load_rds("strategy_top_shares")
counts  <- load_rds("strategy_counts")
perm    <- load_rds("strategy_perm_tests")

body <- shares |>
  dplyr::transmute(Strategy = strategy, class,
                   value = sprintf("%.1f", 100 * persuader_mean_share)) |>
  tidyr::pivot_wider(names_from = class, values_from = value)

n_rows <- counts |>
  dplyr::transmute(class = CLASS_COLS[as.integer(class)],
                   Persuaders = format(n_persuaders, big.mark = ","),
                   Conversations = format(n_conversations, big.mark = ",")) |>
  tidyr::pivot_longer(-class, names_to = "Strategy") |>
  tidyr::pivot_wider(names_from = class, values_from = value)

pairwise <- dplyr::filter(perm, test == "vs Random Laypeople")
perm_row <- tibble::tibble(Strategy = "Permutation p vs Random (Holm)",
                           `Random Laypeople` = "")
for (cl in CLASS_COLS[-1]) {
  perm_row[[cl]] <- fmt_p(pairwise$p_holm[pairwise$comparison == paste("Random Laypeople vs", cl)])
}

wide <- dplyr::bind_rows(body, n_rows, perm_row) |>
  dplyr::select(Strategy, dplyr::all_of(CLASS_COLS))

omni4 <- perm$p_perm[perm$test == "omnibus, four uncoached classes"]
omni5 <- perm$p_perm[perm$test == "omnibus, all five classes (debaters appear twice)"]
subtitle <- sprintf(
  paste0("Percentage of conversations in which each strategy received the persuader's highest rating ",
         "among the eight strategies (relying on instinct excluded; ties split equally), averaged within ",
         "persuader and then across the persuaders in each class. Human persuaders, Studies 1-3. Debaters ",
         "observed before and after coaching appear in both Elite Debater columns. Permutation tests shuffle ",
         "class labels across persuaders (%s draws; chi-squared statistic on conversation counts); p-values vs ",
         "Random Laypeople are Holm-adjusted across the four comparisons. Omnibus test across the four ",
         "uncoached classes p %s; across all five p %s."),
  format(5000, big.mark = ","),
  ifelse(omni4 < 0.001, "< .001", sprintf("= %.3f", omni4)),
  ifelse(omni5 < 0.001, "< .001", sprintf("= %.3f", omni5))
)

tbl <- gt::gt(wide) |>
  style_table(
    title    = gt::md("**Top-rated persuasion strategy by human persuader class (%).**"),
    subtitle = subtitle
  )

save_table(tbl, "tab_strategy_top_shares", TABLES_SI,
           label  = "tab:strategy_top_shares",
           layout = "shrink",
           new    = TRUE)

stack_header <- function(txt, label, parts) {
  gsub(sprintf("\\textbf{%s}", label),
       sprintf("\\textbf{\\raisebox{-0.5\\height}{\\shortstack{%s}}}", paste(parts, collapse = " \\\\ ")),
       txt, fixed = TRUE)
}
tex_path <- file.path(TABLES_SI, "tab_strategy_top_shares.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
for (cl in CLASS_COLS) txt <- stack_header(txt, cl, strsplit(sub(" (\\S+)$", "|\\1", cl), "|", fixed = TRUE)[[1]])
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
