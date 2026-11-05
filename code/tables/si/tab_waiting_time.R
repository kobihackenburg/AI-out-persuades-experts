# tab_waiting_time.R
# SI Table: associations of waiting, pause and exchange times with attitude
# change, holding claims and message length constant (Studies 1-2,
# fact-checked conversations). Human persuaders: wait for the persuader's
# message and persuadee pause before replying. Constrained AI: time per
# exchange only, because the per-message drawn delay was not logged
# separately. All measures are post-treatment, so these are associations.
# Reads RDS written by code/analysis/si_15_length_timing.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_waiting_time.R ====")

fmt_cell <- function(estimate, lower_ci, upper_ci) {
  sprintf("%+.2f [%+.2f, %+.2f]", estimate, lower_ci, upper_ci)
}
fmt_p <- function(p) ifelse(p < 0.001, "<.001", sprintf("%.3f", p))

TERM_LABELS <- c(
  log_wait    = "Wait for persuader's message",
  log_pause   = "Persuadee pause before replying",
  log_cycle   = "Time per exchange",
  log_claims  = "Fact-checkable claims",
  log_msg_len = "Persuader message length"
)
MODEL_LABELS <- c(human = "Human persuaders", constrained_ai = "Constrained AI")

coefs <- load_rds("waiting_time_coefs")
desc  <- load_rds("waiting_time_desc")

tbl_df <- coefs |>
  dplyr::mutate(block = sprintf("%s (n = %s)", MODEL_LABELS[model],
                                formatC(n, format = "d", big.mark = ",")),
                model_order = match(model, names(MODEL_LABELS)),
                term_order  = match(term, names(TERM_LABELS))) |>
  dplyr::arrange(model_order, term_order) |>
  dplyr::transmute(
    block,
    Term                = unname(TERM_LABELS[term]),
    `Estimate [95% CI]` = fmt_cell(estimate, conf.low, conf.high),
    p                   = fmt_p(p.value)
  )

med <- function(g, col) desc[[col]][desc$group == g]
subtitle <- sprintf(
  paste0(
    "Attitude change in pp per doubling of each measure. LMMs of post-treatment attitude on ",
    "pre-treatment attitude, issue, attempt number, log2 claims, log2 mean persuader message ",
    "length and the log2 timing measures shown, with random intercepts for persuader and ",
    "persuadee; 95%% Wald CIs. Each measure is the conversation median. Wait: persuadee message ",
    "to the persuader's next message (median %.0f s). Pause: persuader message to the ",
    "persuadee's reply, including reading and writing time (median %.0f s). Time per exchange: ",
    "persuadee message to the persuadee's next message, which for Constrained AI comprises the ",
    "drawn delay, reading and reply (median %.0f s); the drawn delays were not logged separately. ",
    "Fact-checked Studies 1-2 conversations. These measures were not randomly assigned and were ",
    "recorded during the conversation, and pauses are chosen by persuadees, so the estimates ",
    "are associations, not causal effects."
  ),
  med("human", "median_wait_s"), med("human", "median_pause_s"),
  med("constrained_ai", "median_cycle_s")
)

tbl <- gt::gt(tbl_df, groupname_col = "block") |>
  gt::cols_label(Term = "") |>
  style_table(
    title    = gt::md("**Waiting, pause and exchange times and attitude change, holding claims and message length constant.**"),
    subtitle = subtitle
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_waiting_time", TABLES_SI,
           label  = "tab:waiting_time",
           layout = "auto",
           new    = TRUE)

message("  done.")
