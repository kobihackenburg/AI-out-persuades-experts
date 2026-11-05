# tab_accuracy_detection.R
# SI Table: can persuadees detect inaccurate claims? Conversation-level Pearson
# correlations (Studies 1-2, fact-checked conversations with at least one
# scored claim) of the "lied to me or made things up" rating with the share
# and number of inaccurate claims, and of the "I learned a lot" rating with
# the number of accurate and of inaccurate claims. Columns: all persuaders,
# AI persuaders, human persuaders.
# Reads RDS written by code/analysis/si_14_claim_accuracy.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_accuracy_detection.R ====")

detect <- load_rds("claim_accuracy_detection")

subset_labels <- c(all = "All", ai = "AI", human = "Human")
rating_labels <- c(rating_deception = "Lied to me or made things up",
                   rating_learning  = "I learned a lot")
measure_labels <- c(share_inaccurate = "Share of claims inaccurate",
                    n_inaccurate     = "Number of inaccurate claims",
                    n_accurate       = "Number of accurate claims")

n_by_subset <- detect |> dplyr::distinct(subset, n)
stopifnot(nrow(n_by_subset) == length(subset_labels))
col_labels <- sprintf("%s (n = %s)", subset_labels[n_by_subset$subset],
                      format(n_by_subset$n, big.mark = ","))
names(col_labels) <- n_by_subset$subset

wide <- detect |>
  dplyr::mutate(cell = sprintf("%.2f [%.2f, %.2f]", r, lower_ci, upper_ci)) |>
  dplyr::select(subset, x, y, cell) |>
  tidyr::pivot_wider(names_from = subset, values_from = cell) |>
  dplyr::transmute(
    Rating        = unname(rating_labels[y]),
    `Claim count` = unname(measure_labels[x]),
    all, ai, human
  )

tbl <- gt::gt(wide, groupname_col = "Rating") |>
  gt::cols_label(.list = as.list(col_labels)) |>
  style_table(
    title    = gt::md("**Persuadee ratings vs. the accuracy of persuaders' claims.**"),
    subtitle = paste0(
      "Pearson correlations (r [95% CI]) between per-conversation claim counts and ",
      "the persuadee's 0-100 partner ratings, Studies 1-2 fact-checked conversations ",
      "with at least one scored claim, for all persuaders and for AI and human ",
      "persuaders separately. Row groups give the rating item. A claim is ",
      "accurate if its web-search veracity score exceeds 50 and inaccurate otherwise; ",
      "share of claims inaccurate is the number of inaccurate claims divided by all ",
      "scored claims in the conversation."
    )
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_accuracy_detection", TABLES_SI,
           label  = "tab:accuracy_detection",
           layout = "shrink",
           new    = TRUE)

message("  done.")
