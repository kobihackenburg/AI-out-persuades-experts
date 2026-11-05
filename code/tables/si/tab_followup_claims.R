# tab_followup_claims.R
# SI Table: association between a conversation's claim count and attitudes,
# immediately and at follow-up, in the same re-surveyed persuader
# conversations (Studies 1-3; si_07 specification, Constrained AI counted as AI).
# Reads RDS written by code/analysis/si_13_followup.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_followup_claims.R ====")

cl <- load_rds("followup_claims")

wide <- cl |>
  dplyr::mutate(
    Term = dplyr::recode(term,
                         `per doubling of claims`     = "Per doubling of claims",
                         `human - AI at equal claims` = "Human minus AI, at equal claims"),
    wave = ifelse(wave == "immediate", "Immediate", "Follow-up"),
    cell = sprintf("%+.2f [%+.2f, %+.2f]", estimate, lower_ci, upper_ci)
  ) |>
  dplyr::select(Term, wave, cell) |>
  tidyr::pivot_wider(names_from = wave, values_from = cell)

per_doubling <- cl[cl$term == "per doubling of claims", ]
retained <- per_doubling$estimate[per_doubling$wave == "follow-up"] /
  per_doubling$estimate[per_doubling$wave == "immediate"]

tbl <- gt::gt(wide) |>
  style_table(
    title    = gt::md("**Claim count and attitudes, immediately and at follow-up (Studies 1--3).**"),
    subtitle = sprintf(paste0(
      "Linear mixed model of the immediate or follow-up attitude on log claim count, a human-persuader ",
      "indicator, pre-treatment attitude, issue and session number, with persuader and persuadee random ",
      "intercepts, in the %s re-surveyed AI (including Constrained AI) and human conversations with at least ",
      "one fact-checked claim (pp [95%% CI]). Claim count was not randomized, so these are associations. The ",
      "follow-up gradient ",
      "is %.0f%% of the immediate one."),
      format(cl$n[1], big.mark = ","), 100 * retained)
  )

save_table(tbl, "tab_followup_claims", TABLES_SI,
           label  = "tab:followup_claims",
           layout = "auto",
           new    = TRUE)

message("  done.")
