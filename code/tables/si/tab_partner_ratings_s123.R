# tab_partner_ratings_s123.R
# SI Table: pooled S1-S3 AI minus per-human-class differences on the
# 7-item partner-rating battery. Long format -- one row per
# (item x human comparator class), grouped by item -- so the table
# stays within text width even with the longer "estimate / 95% CI / p"
# breakdown. Companion to the Study 4 AI vs Canvasser analogue.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_partner_ratings_s123.R ====")

pr <- load_rds("partner_ratings_s123")

ITEM_INFO <- tibble::tribble(
  ~item,              ~item_label,                                  ~item_order,
  "rating_arguments", "Made strong arguments",                      1L,
  "rating_learning",  "I learned a lot",                            2L,
  "rating_empathy",   "Felt understood by partner",                 3L,
  "rating_enjoyment", "Conversation was enjoyable",                 4L,
  "rating_anthropo",  "Partner interacted like a human",            5L,
  "rating_deception", "Partner lied or made things up",             6L,
  "rating_bias",      "Partner was biased toward one side",         7L
)

CLASS_INFO <- tibble::tribble(
  ~class,                  ~class_label,           ~class_order,
  "random_lay_person",     "Random Layperson",     1L,
  "elite_lay_person",      "Selected Layperson",   2L,
  "canvasser",             "Professional Canvasser", 3L,
  "elite_debater",         "Elite Debater",        4L,
  "coached_elite_debater", "Coached Elite Debater", 5L
)

fmt_p <- function(p) {
  ifelse(is.na(p), "--",
         ifelse(p < 0.001, "<.001", sprintf("%.3f", p)))
}

tbl_df <- pr |>
  dplyr::inner_join(ITEM_INFO,  by = "item") |>
  dplyr::inner_join(CLASS_INFO, by = "class") |>
  dplyr::arrange(item_order, class_order) |>
  dplyr::transmute(
    Item       = item_label,
    Comparator = class_label,
    Estimate   = sprintf("%+.2f", estimate),
    `95% CI`   = sprintf("[%+.2f, %+.2f]", lower_ci, upper_ci),
    p          = fmt_p(p_value)
  )

tbl <- gt::gt(tbl_df, groupname_col = "Item") |>
  style_table(
    title    = gt::md("**AI minus each human class on the 7-item partner-rating battery (pooled S1-S3).**"),
    subtitle = "Per-item LMM: rating ~ issue + study + group + (1|persuader) + (1|persuadee), pooled across Studies 1-3. Each block is one battery item; rows give the AI minus comparator-class contrast in 0-100 rating-scale points. Positive estimates indicate the AI was rated higher than the comparator on the item as worded; for the bottom three items (anthropomorphism, deception, bias), a higher rating is unfavourable for the rated partner. p-values are uncorrected."
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_partner_ratings_s123", TABLES_SI,
           label  = "tab:partner_ratings_s123",
           layout = "longtable")
message("  done.")
