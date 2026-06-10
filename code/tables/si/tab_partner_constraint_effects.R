# tab_partner_constraint_effects.R
# SI Table: per-item effect of constraining AI on persuadees' post-
# conversation partner ratings (Studies 1-2). Backs the main-text
# Figure 3a (fig:mechanism panel a). One row group per battery item;
# rows give the Constrained AI - Info-prompted AI and Human -
# Info-prompted AI contrasts (0-100 rating points) from the per-item
# LMMs. Source RDS: mechanism_constraint_effects.rds (produced by
# code/analysis/02_mechanism.R, section 4).

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_partner_constraint_effects.R ====")

ce <- load_rds("mechanism_constraint_effects")

ITEM_INFO <- tibble::tribble(
  ~item,              ~item_label,                            ~item_order,
  "rating_arguments", "Made strong arguments",                1L,
  "rating_learning",  "I learned a lot",                      2L,
  "rating_empathy",   "Felt understood by partner",           3L,
  "rating_enjoyment", "Conversation was enjoyable",           4L,
  "rating_anthropo",  "Partner interacted like a human",      5L,
  "rating_deception", "Partner lied or made things up",       6L,
  "rating_bias",      "Partner was biased toward one side",   7L
)

CONTRAST_INFO <- tibble::tribble(
  ~contrast_kind,     ~contrast_label,                       ~contrast_order,
  "Constrained - AI", "Constrained AI - Info-prompted AI",   1L,
  "Human - AI",       "Human - Info-prompted AI",            2L
)

fmt_p <- function(p) {
  ifelse(is.na(p), "--",
         ifelse(p < 0.001, "<.001", sprintf("%.3f", p)))
}

tbl_df <- ce |>
  dplyr::inner_join(ITEM_INFO,     by = "item") |>
  dplyr::inner_join(CONTRAST_INFO, by = "contrast_kind") |>
  dplyr::arrange(item_order, contrast_order) |>
  dplyr::transmute(
    Item       = item_label,
    Contrast   = contrast_label,
    Estimate   = sprintf("%+.2f", estimate),
    `95% CI`   = sprintf("[%+.2f, %+.2f]", lower_ci, upper_ci),
    p          = fmt_p(p_value)
  )

n_obs <- unique(ce$n_obs)[1]
subtitle <- sprintf(
  paste0(
    "Per-item LMM: rating ~ pre + issue + study + group + attempt + ",
    "(1|persuadee), fit on Studies 1-2 (n = %s persuadee-conversations ",
    "per item). The reference is the unconstrained Info-prompted AI; ",
    "rows give the Constrained AI and pooled Human contrasts in 0-100 ",
    "rating-scale points. Negative values indicate a lower rating than ",
    "unconstrained AI. For the bottom three items (anthropomorphism, ",
    "deception, bias) a higher rating is unfavourable for the rated ",
    "partner. p-values are uncorrected."),
  format(n_obs, big.mark = ",")
)

tbl <- gt::gt(tbl_df, groupname_col = "Item") |>
  style_table(
    title    = gt::md("**Effect of constraining AI on partner ratings (Studies 1-2).**"),
    subtitle = subtitle
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_partner_constraint_effects", TABLES_SI,
           label  = "tab:partner_constraint_effects",
           layout = "longtable")
message("  done.")
