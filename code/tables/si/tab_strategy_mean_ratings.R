# tab_strategy_mean_ratings.R
# SI Table: mean self-rating (0-100) of each of the nine strategy items for
# Random Laypeople and each other human class's difference from them
# (rating ~ class + (1 | persuader), Studies 1-3).
# Reads RDS written by code/analysis/si_19_persuader_strategies.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_strategy_mean_ratings.R ====")

OTHER_CLASSES <- c("Selected Laypeople", "Elite Debaters", "Coached Elite Debaters",
                   "Professional Canvassers")

mr <- load_rds("strategy_mean_ratings")
item_order <- unique(mr$strategy)

ref <- mr |>
  dplyr::filter(type == "mean", class == "Random Laypeople") |>
  dplyr::transmute(Strategy = strategy,
                   `Random Laypeople (mean)` = sprintf("%.1f [%.1f, %.1f]", estimate, conf.low, conf.high))

diffs <- mr |>
  dplyr::filter(type != "mean") |>
  dplyr::transmute(Strategy = strategy, class,
                   value = sprintf("%+.1f [%+.1f, %+.1f]%s", estimate, conf.low, conf.high,
                                   ifelse(p_holm < 0.05, "*", ""))) |>
  tidyr::pivot_wider(names_from = class, values_from = value)

wide <- ref |>
  dplyr::left_join(diffs, by = "Strategy") |>
  dplyr::arrange(match(Strategy, item_order)) |>
  dplyr::select(Strategy, `Random Laypeople (mean)`, dplyr::all_of(OTHER_CLASSES))

subtitle <- paste0(
  "Mean self-rating (0-100) of each strategy item for Random Laypeople, and each other class's ",
  "difference from Random Laypeople [95% CI], from one LMM per item (rating ~ class + ",
  "(1 | persuader)). Human persuaders, Studies 1-3. * Holm-adjusted p < .05 across the ",
  sum(mr$type != "mean"), " item-by-class contrasts. Selected Laypeople rated nearly every item ",
  "higher than Random Laypeople, consistent with differences in scale use, so the top-rated-strategy ",
  "shares are the primary measure of strategy mix."
)

tbl <- gt::gt(wide) |>
  style_table(
    title    = gt::md("**Mean strategy self-ratings by human persuader class.**"),
    subtitle = subtitle
  )

save_table(tbl, "tab_strategy_mean_ratings", TABLES_SI,
           label  = "tab:strategy_mean_ratings",
           layout = "landscape_fit",
           new    = TRUE)

stack_header <- function(txt, label, parts) {
  gsub(sprintf("\\textbf{%s}", label),
       sprintf("\\textbf{\\raisebox{-0.5\\height}{\\shortstack{%s}}}", paste(parts, collapse = " \\\\ ")),
       txt, fixed = TRUE)
}
tex_path <- file.path(TABLES_SI, "tab_strategy_mean_ratings.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- stack_header(txt, "Random Laypeople (mean)", c("Random Laypeople", "(mean)"))
for (cl in OTHER_CLASSES) {
  txt <- stack_header(txt, cl, c(sub(" \\S+$", "", cl), sub("^.* ", "", cl), "minus Random"))
}
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
