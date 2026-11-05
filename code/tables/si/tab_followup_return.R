# tab_followup_return.R
# SI Table: follow-up return rates by assigned condition.
#   Studies 1-3 - each persuadee's first session (one row per persuadee)
#   Study 4     - preregistered analysis sample (one conversation each)
# Differences vs control are linear probability models with HC2 standard
# errors (pp; Studies 1-3 adjust for study); the Studies 1-3 joint test and
# AI minus human difference are in the caption.
# Reads RDS written by code/analysis/si_13_followup.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_followup_return.R ====")

CLASS_LABELS <- c(PERSUADER_CLASS_LABELS, ai_constrained = "Constrained AI")

resp <- load_rds("followup_response")

fmt_cell <- function(estimate, lower_ci, upper_ci) {
  ifelse(is.na(estimate), "",
         sprintf("%+.1f [%+.1f, %+.1f]", estimate, lower_ci, upper_ci))
}

rates_panel <- function(rates, tests, panel) {
  prefix <- "^response pp vs control, by class: "
  diffs <- tests |>
    dplyr::filter(grepl(prefix, spec)) |>
    dplyr::transmute(group = sub(prefix, "", spec),
                     diff = fmt_cell(estimate, lower_ci, upper_ci))
  rates |>
    dplyr::mutate(group = as.character(group)) |>
    dplyr::left_join(diffs, by = "group") |>
    dplyr::mutate(order = match(group, c(PERSUADER_CLASS_ORDER, "ai_constrained"))) |>
    dplyr::arrange(order) |>
    dplyr::transmute(panel = panel, Condition = unname(CLASS_LABELS[group]),
                     Persuadees = format(n, big.mark = ","),
                     `Returned (%)` = sprintf("%.1f", 100 * fu_rate),
                     `Difference vs control, pp` = dplyr::coalesce(diff, ""))
}

s123 <- rates_panel(resp$s123_rates, resp$s123_tests, "Studies 1--3 (first sessions)")
s4   <- rates_panel(resp$s4_rates, resp$s4_tests, "Study 4")

t123 <- resp$s123_tests
joint   <- t123[grepl("^joint Wald", t123$spec), ]
ai_hu   <- t123[t123$spec == "response pp: AI - human", ]

tbl <- gt::gt(dplyr::bind_rows(s123, s4), groupname_col = "panel") |>
  style_table(
    title    = gt::md("**Follow-up return rates by assigned condition.**"),
    subtitle = sprintf(paste0(
      "Share of persuadees who re-rated the issue at follow-up, and differences vs control (pp [95%% CI]) ",
      "from linear probability models with HC2 standard errors. Studies 1--3: each persuadee's first session ",
      "on an eligible issue (one row per persuadee), with study fixed effects; persuader class did not ",
      "predict return (joint Wald chi-squared(%d) = %.2f, p = %.2f), and AI and human conditions did not ",
      "differ (AI minus human %+.1f pp, 95%% CI [%+.1f, %+.1f]). Study 4: preregistered analysis sample, ",
      "share who made the follow-up donation decision."),
      joint$df, joint$estimate, joint$p_value, ai_hu$estimate, ai_hu$lower_ci, ai_hu$upper_ci)
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_followup_return", TABLES_SI,
           label  = "tab:followup_return",
           layout = "auto",
           new    = TRUE)

message("  done.")
