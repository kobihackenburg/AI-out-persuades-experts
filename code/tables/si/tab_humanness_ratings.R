# tab_humanness_ratings.R
# SI Table: persuadees' ratings of "My conversation partner interacted just
# like a human would" (0-100) by persuader type, Studies 1-3, unconstrained
# AI vs all human classes. Partner type was randomized, so this is a
# randomized comparison.
# Reads RDS written by code/analysis/si_17_humanlike.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_humanness_ratings.R ====")

sep <- load_rds("humanness_separation")

tbl_df <- sep |>
  dplyr::arrange(dplyr::desc(is_ai)) |>
  dplyr::transmute(
    Persuader            = dplyr::if_else(is_ai == 1L, "AI", "Human"),
    `n conv`             = format(n, big.mark = ","),
    Mean                 = sprintf("%.1f", mean),
    Median               = sprintf("%.0f", median),
    `Rated 75 or above`  = sprintf("%.0f%%", 100 * share_ge75),
    `Rated 25 or below`  = sprintf("%.0f%%", 100 * share_le25)
  )

subtitle <- sprintf(
  paste0("Ratings of the statement \"My conversation partner interacted just like a human would\" ",
         "(0-100), collected after each conversation, Studies 1-3. AI excludes Constrained AI; ",
         "Human pools all human persuader classes. Probability that a randomly chosen human ",
         "conversation was rated more human-like than a randomly chosen AI conversation (AUC): ",
         "%.2f. Share of AI conversations rated at or above the human median (%.0f): %.0f%%."),
  sep$auc_human[1], sep$human_median[1], 100 * sep$ai_ge_human_median[1]
)

tbl <- gt::gt(tbl_df) |>
  style_table(
    title    = gt::md("**Perceived humanness of persuaders, by persuader type.**"),
    subtitle = subtitle
  )

save_table(tbl, "tab_humanness_ratings", TABLES_SI,
           label  = "tab:humanness_ratings",
           layout = "auto",
           new    = TRUE)

message("  done.")
