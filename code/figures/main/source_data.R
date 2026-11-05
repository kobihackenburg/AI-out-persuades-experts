# source_data.R
# Nature Source Data: one CSV per main-text figure with the values each panel
# plots, read from the same output/results/*.rds the figure scripts use.
# Each row carries its panel and what it is; columns not used by a block are
# empty.
#
#   output/source_data/source_data_fig{1..4}.csv

source(here::here("code/analysis/_setup.R"))

message("\n==== code/figures/main/source_data.R ====")

SOURCE_DATA <- file.path(OUTPUT, "source_data")
dir.create(SOURCE_DATA, recursive = TRUE, showWarnings = FALSE)

block <- function(df, panel, element) {
  dplyr::mutate(tibble::as_tibble(df), panel = panel, element = element, .before = 1)
}

write_source <- function(n, ...) {
  out <- dplyr::bind_rows(...)
  path <- file.path(SOURCE_DATA, sprintf("source_data_fig%d.csv", n))
  readr::write_csv(out, path, na = "")
  message(sprintf("  wrote %s (%d rows)", path, nrow(out)))
}

write_source(1,
  block(load_rds("summary_class_effects"), "a", "Effect vs control (pp), by persuader class"),
  block(load_rds("summary_ai_overlays"),   "a", "Effect vs control (pp), by AI model")
)

write_source(2,
  block(load_rds("limits_contrasts"), "a", "AI minus comparator (pp)"),
  block(load_rds("limits_inset"),     "a", "Words per message and reply delay"),
  block(load_rds("limits_class_distributions"), "b",
        "Distribution of persuader-level effects (mean and model-estimated SD, pp)"),
  block(tibble::tibble(raw_class = "ai", group_te = load_rds("limits_ai_reference")), "b",
        "AI effect vs control (pp), reference line")
)

write_source(3,
  block(load_rds("mechanism_constraint_effects"), "a", "Partner-rating contrasts"),
  block(load_rds("mechanism_facts_vs_te"),        "b", "Effect vs control (pp) and mean claims per conversation"),
  block(load_rds("mechanism_facts_regression"),   "b", "Regression of effect on mean claims")
)

write_source(4,
  block(load_rds("realworld_te_vs_control"), "a", "Donation effect vs control (pp)"),
  block(load_rds("realworld_strategies"),    "b", "Mechanism ratings, AI minus canvassers"),
  block(load_rds("realworld_mechanism"),     "b", "Item ratings, AI minus canvassers")
)

message("  done.")
