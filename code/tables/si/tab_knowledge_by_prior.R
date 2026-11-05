# tab_knowledge_by_prior.R
# SI Table: mean self-reported issue knowledge by pre-treatment attitude
# toward the assigned stance (Studies 1-3 complete cases).
# Reads RDS written by code/analysis/si_21_opponents.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_knowledge_by_prior.R ====")

kp <- load_rds("strong_opponents_knowledge")

tbl_df <- kp$bands |>
  dplyr::transmute(
    `Pre-treatment attitude` = sub("-", "--", as.character(prior_band), fixed = TRUE),
    Conversations            = format(n, big.mark = ",", trim = TRUE),
    `Mean issue knowledge`   = sprintf("%.1f", mean_knowledge)
  )

subtitle <- sprintf(paste0(
  "Studies 1-3 complete cases. Pre-treatment attitude toward the assigned stance and ",
  "self-reported issue knowledge are both on 0-100 scales. Mean knowledge is %.1f among ",
  "persuadees at 25 or below and %.1f among those above 25."),
  kp$strong$mean_knowledge, kp$rest$mean_knowledge)

tbl <- gt::gt(tbl_df) |>
  style_table(
    title    = gt::md("**Pre-treatment issue knowledge by prior attitude.**"),
    subtitle = subtitle
  )

save_table(tbl, "tab_knowledge_by_prior", TABLES_SI,
           label  = "tab:knowledge_by_prior",
           layout = "auto",
           new    = TRUE)

message("  done.")
