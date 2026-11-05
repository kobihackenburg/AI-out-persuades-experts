# tab_strategy_coaching_shift.R
# SI Table: change in self-reported strategy among the Elite Debaters
# observed both before (Study 1) and after (Study 2) coaching: top-rated
# strategy shares (paired t across debaters) and mean ratings
# (rating ~ coached + (1 | persuader)).
# Reads RDS written by code/analysis/si_19_persuader_strategies.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_strategy_coaching_shift.R ====")

cs <- load_rds("strategy_coaching_shift")
item_order <- unique(cs$strategy[cs$measure == "mean rating"])

shares <- cs |>
  dplyr::filter(measure == "top-strategy share") |>
  dplyr::transmute(Strategy = strategy,
                   `Share before` = sprintf("%.1f", 100 * before),
                   `Share after`  = sprintf("%.1f", 100 * after),
                   `Share change` = sprintf("%+.1f [%+.1f, %+.1f]", 100 * difference,
                                            100 * conf.low, 100 * conf.high))
ratings <- cs |>
  dplyr::filter(measure == "mean rating") |>
  dplyr::transmute(Strategy = strategy,
                   `Rating before` = sprintf("%.1f", before),
                   `Rating after`  = sprintf("%.1f", after),
                   `Rating change` = sprintf("%+.1f [%+.1f, %+.1f]", difference, conf.low, conf.high))

wide <- ratings |>
  dplyr::left_join(shares, by = "Strategy") |>
  dplyr::arrange(match(Strategy, item_order)) |>
  dplyr::mutate(dplyr::across(dplyr::starts_with("Share"), ~ dplyr::coalesce(.x, ""))) |>
  dplyr::select(Strategy, dplyr::starts_with("Share"), dplyr::starts_with("Rating"))

subtitle <- sprintf(
  paste0("The %d Elite Debaters with strategy reports both before coaching (Study 1) and after it ",
         "(Study 2). Share: percentage of conversations in which the strategy was the top-rated one, ",
         "averaged across debaters; change from a paired t-test across debaters. Rating: mean ",
         "self-rating (0-100) from rating ~ coached + (1 | persuader); change is the coached ",
         "coefficient. Changes are pp (shares) or rating points, with 95%% CIs. Relying on instinct is ",
         "not one of the eight strategies used for the top-rated share."),
  cs$n_persuaders[1]
)

tbl <- gt::gt(wide) |>
  style_table(
    title    = gt::md("**Self-reported strategies of Elite Debaters before and after coaching.**"),
    subtitle = subtitle
  )

save_table(tbl, "tab_strategy_coaching_shift", TABLES_SI,
           label  = "tab:strategy_coaching_shift",
           layout = "shrink",
           new    = TRUE)

stack_header <- function(txt, label, parts) {
  gsub(sprintf("\\textbf{%s}", label),
       sprintf("\\textbf{\\raisebox{-0.5\\height}{\\shortstack{%s}}}", paste(parts, collapse = " \\\\ ")),
       txt, fixed = TRUE)
}
tex_path <- file.path(TABLES_SI, "tab_strategy_coaching_shift.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
for (h in c("Share before", "Share after", "Share change", "Rating before", "Rating after", "Rating change")) {
  txt <- stack_header(txt, h, strsplit(h, " ")[[1]])
}
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
