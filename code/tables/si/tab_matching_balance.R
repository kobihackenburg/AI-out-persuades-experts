# tab_matching_balance.R
# SI Table: pre-treatment balance (Studies 1-3) of (i) matched vs unmatched
# persuadee sessions in human conditions and (ii) the analysed AI vs human
# samples. Cells are means and standardized mean differences; the joint
# likelihood-ratio tests are reported in the caption. Reads
# output/results/matching_balance{,_tests}.rds (built by
# code/analysis/si_23_matching_order.R).

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_matching_balance.R ====")

COVARIATE_LABELS <- c(
  pre_attitude          = "Pre-treatment attitude (0-100)",
  age                   = "Age (years)",
  female                = "Female (share)",
  degree                = "University degree (share)",
  ideology_coded        = "Ideology (0 = Left, 4 = Right)",
  political_knowledge_n = "Political knowledge (0-4 correct)",
  dogmatism             = "Dogmatism scale",
  ai_trust              = "AI trust (0-100)",
  pre_issue_knowledge   = "Pre-treatment issue knowledge (0-100)",
  pre_importance        = "Pre-treatment issue importance (0-100)"
)

bal <- load_rds("matching_balance")
tests <- load_rds("matching_balance_tests")

fmt_mean <- function(x) ifelse(abs(x) < 10, sprintf("%.2f", x), sprintf("%.1f", x))

panel <- function(label, prefix) {
  bal |>
    dplyr::filter(comparison == label) |>
    dplyr::transmute(covariate,
                     !!paste(prefix, "1") := fmt_mean(mean_1),
                     !!paste(prefix, "0") := fmt_mean(mean_0),
                     !!paste(prefix, "smd") := sprintf("%+.3f", smd))
}

wide <- panel("human conditions: matched vs unmatched sessions", "m") |>
  dplyr::left_join(panel("analysed sample: AI vs human conditions", "a"), by = "covariate") |>
  dplyr::mutate(covariate = unname(COVARIATE_LABELS[covariate])) |>
  dplyr::rename(
    Covariate             = covariate,
    Matched               = `m 1`,
    Unmatched             = `m 0`,
    `SMD (matched)`       = `m smd`,
    AI                    = `a 1`,
    Human                 = `a 0`,
    `SMD (AI)`            = `a smd`
  )

n_of <- function(label, col) {
  r <- dplyr::filter(bal, comparison == label, covariate == "pre_attitude")
  format(r[[col]], big.mark = ",")
}
test_of <- function(label) dplyr::filter(tests, test == label)
t_match <- test_of("human conditions: covariates predict matching")
t_ai    <- test_of("analysed sample: covariates predict AI vs human condition")

subtitle <- sprintf(
  paste0("Left: persuadee sessions assigned to a human condition, matched (n = %s) vs unmatched (n = %s). ",
         "Right: analyzed sample, AI (n = %s) vs human (n = %s) conversations. ",
         "SMD is the standardized mean difference (matched minus unmatched; AI minus human) in pooled-SD units. ",
         "Age, ideology, political knowledge, dogmatism and AI trust are missing for some sessions and are ",
         "summarized over non-missing values. Joint likelihood-ratio tests of all ten covariates in a logit ",
         "with study fixed effects (missing values median-imputed): matching, chi-squared(%d) = %.2f, p = %.2f; ",
         "AI vs human condition, chi-squared(%d) = %.2f, p = %.2f."),
  n_of("human conditions: matched vs unmatched sessions", "n_1"),
  n_of("human conditions: matched vs unmatched sessions", "n_0"),
  n_of("analysed sample: AI vs human conditions", "n_1"),
  n_of("analysed sample: AI vs human conditions", "n_0"),
  t_match$df, t_match$chisq, t_match$p_value,
  t_ai$df, t_ai$chisq, t_ai$p_value
)

tbl <- gt::gt(wide) |>
  style_table(
    title    = gt::md("**Pre-treatment balance at matchmaking and in the analyzed sample (Studies 1--3).**"),
    subtitle = subtitle
  )

save_table(tbl, "tab_matching_balance", TABLES_SI,
           label  = "tab:matching_balance",
           layout = "fit",
           new    = TRUE)

message("  done.")
