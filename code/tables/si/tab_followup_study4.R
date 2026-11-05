# tab_followup_study4.R
# SI Table: Study 4 donation effects immediately and at follow-up among the
# persuadees who made the follow-up donation decision (preregistered donation
# model, follow-up donation of a new GBP 1 bonus swapped in as outcome).
# Reads RDS written by code/analysis/si_13_followup.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_followup_study4.R ====")

fmt_cell <- function(estimate, lower_ci, upper_ci) {
  sprintf("%+.2f [%+.2f, %+.2f]", estimate, lower_ci, upper_ci)
}

res <- load_rds("followup_s4_results")

wide <- res |>
  dplyr::mutate(
    Contrast = dplyr::recode(spec,
                             `ai vs control`        = "AI vs control",
                             `canvasser vs control` = "Professional Canvassers vs control",
                             `AI - canvasser`       = "AI minus Professional Canvassers"),
    order = match(spec, c("ai vs control", "canvasser vs control", "AI - canvasser")),
    wave  = ifelse(wave == "immediate", "Immediate", "Follow-up"),
    cell  = fmt_cell(estimate, lower_ci, upper_ci)
  ) |>
  dplyr::select(order, Contrast, wave, cell) |>
  tidyr::pivot_wider(names_from = wave, values_from = cell) |>
  dplyr::arrange(order) |>
  dplyr::select(Contrast, Immediate, `Follow-up`)

tbl <- gt::gt(wide) |>
  style_table(
    title    = gt::md("**Study 4 donation effects immediately and at follow-up.**"),
    subtitle = sprintf(paste0(
      "Differences in the share of the bonus donated to Save the Children (pp [95%% CI]) among the %s ",
      "persuadees in the preregistered analysis sample who made the follow-up decision. Immediate: the ",
      "original donation decision; follow-up: the share of a new \u00a31 bonus donated about 4.5 months ",
      "later. Both from the preregistered donation model (pre-treatment support and donation willingness, ",
      "age, ideology and condition, with a persuader random intercept)."),
      format(res$n[1], big.mark = ","))
  )

save_table(tbl, "tab_followup_study4", TABLES_SI,
           label  = "tab:followup_study4",
           layout = "auto",
           new    = TRUE)

message("  done.")
