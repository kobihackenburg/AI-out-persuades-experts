# tab_matching_robustness.R
# SI Table: AI-minus-human persuasive advantage (pooled Studies 1-3 LMM) under
# specifications that address selection at matchmaking and the wait for a
# human partner. Reads output/results/matching_{selection,wait}_specs.rds
# (built by code/analysis/si_23_matching_order.R).

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_matching_robustness.R ====")

SPEC_LABELS <- c(
  "preregistered spec"                                      = "Preregistered model",
  "+ all pre-treatment covariates"                          = "+ nine further pre-treatment covariates",
  "hours with human persuaders online"                      = "Clock hours with a human conversation",
  "hours with human persuaders online + study-date-hour FE" = "Clock hours with a human conversation + study-date-hour FE",
  "human conversations with start gap <= AI median"         = "Human conversations with start gap at or below the AI median"
)

specs <- dplyr::bind_rows(
  load_rds("matching_selection_specs") |> dplyr::mutate(n_human = NA_integer_),
  load_rds("matching_wait_specs")
)
stopifnot(all(specs$spec %in% names(SPEC_LABELS)))

rows <- specs |>
  dplyr::transmute(
    Specification       = unname(SPEC_LABELS[spec]),
    `AI minus human (pp)` = sprintf("%+.2f [%+.2f, %+.2f]", estimate, lower_ci, upper_ci),
    p                   = ifelse(p_value < .001, "<.001", sprintf("%.3f", p_value)),
    `N conversations`   = format(n, big.mark = ",")
  )

n_short_human <- format(specs$n_human[!is.na(specs$n_human)], big.mark = ",")

subtitle <- paste0(
  "Estimate [95% CI] of the AI-minus-human contrast from the pooled Studies 1-3 model ",
  "(pre-treatment attitude, issue, condition and attempt fixed effects; persuader and persuadee random intercepts), ",
  "with human persuader classes pooled. The covariate row adds age, female, degree, ideology, political knowledge, ",
  "dogmatism, AI trust, issue knowledge and issue importance (missing values median-imputed). The clock-hour rows ",
  "keep conversations that started in a study-date-hour in which at least one human conversation started ",
  "(control sessions dated by session start), optionally with a fixed effect for each such hour. The last row keeps ",
  "human conversations whose start gap (session start to conversation start) was at most the median AI start gap ",
  "in the same study (", n_short_human, " human conversations), with all AI and control conversations."
)

tbl <- gt::gt(rows) |>
  style_table(
    title    = gt::md("**AI's advantage under selection and waiting-time checks (Studies 1--3).**"),
    subtitle = subtitle
  )

save_table(tbl, "tab_matching_robustness", TABLES_SI,
           label  = "tab:matching_robustness",
           layout = "fit",
           new    = TRUE)

message("  done.")
