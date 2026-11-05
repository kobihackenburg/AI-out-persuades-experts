# tab_persuader_order.R
# SI Table: human persuader workload (conversations and sittings) and
# within-persuader order effects on post-treatment attitude, by persuader
# class (Studies 1-3). Reads output/results/persuader_{load,order}.rds (built
# by code/analysis/si_23_matching_order.R).

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_persuader_order.R ====")

fmt_cell <- function(estimate, lower_ci, upper_ci) {
  ifelse(is.na(estimate), "",
         sprintf("%+.2f [%+.2f, %+.2f]", estimate, lower_ci, upper_ci))
}

load_df <- load_rds("persuader_load")
order_df <- load_rds("persuader_order")

effects <- order_df |>
  dplyr::filter(term %in% c("order10", "in_sitting_late")) |>
  dplyr::transmute(group, term, cell = fmt_cell(estimate, lower_ci, upper_ci)) |>
  tidyr::pivot_wider(names_from = term, values_from = cell)

n_all <- order_df |> dplyr::filter(group == "all_human") |> dplyr::pull(n) |> unique()
lq <- dplyr::filter(order_df, group == "all_human", term == "last_quartile")

rows <- load_df |>
  dplyr::mutate(group = as.character(group)) |>
  dplyr::bind_rows(tibble::tibble(group = "all_human", conversations = n_all)) |>
  dplyr::left_join(effects, by = "group") |>
  dplyr::mutate(g_order = match(group, c(PERSUADER_CLASS_ORDER, "all_human"))) |>
  dplyr::arrange(g_order) |>
  dplyr::transmute(
    Class                          = ifelse(group == "all_human", "All human persuaders",
                                            unname(PERSUADER_CLASS_LABELS[group])),
    Persuaders                     = ifelse(is.na(persuaders), "", as.character(persuaders)),
    Conversations                  = format(conversations, big.mark = ","),
    `Per persuader` =
      ifelse(is.na(convs_per_persuader_median), "",
             sprintf("%g (%d)", convs_per_persuader_median, as.integer(convs_per_persuader_max))),
    Sittings = ifelse(is.na(sittings_per_persuader_median), "", sprintf("%g", sittings_per_persuader_median)),
    `Per sitting` = ifelse(is.na(convs_per_sitting_median), "", sprintf("%g", convs_per_sitting_median)),
    `Per 10 conversations (pp)`  = order10,
    `3rd+ in sitting (pp)`       = in_sitting_late
  )

subtitle <- sprintf(
  paste0("Analyzed conversations with a recorded start time, ordered within each persuader by start time. ",
         "Per persuader is the median (maximum) number of conversations per persuader; Sittings is the median ",
         "number of sittings per persuader, where a new sitting begins after a break of more than 30 minutes since ",
         "the persuader's previous conversation ended; Per sitting is the median, over conversations, of the number ",
         "of conversations in the sitting. ",
         "Order effects are estimates [95%% CI] on post-treatment attitude from models with persuader fixed effects ",
         "(pre-treatment attitude, issue and attempt fixed effects; persuadee random intercept), so they compare a ",
         "persuader's later conversations with their own earlier ones. Per 10 conversations is the slope on position ",
         "in the persuader's own sequence; 3rd+ in sitting contrasts the third and later conversations of a sitting ",
         "with the first two. For all human persuaders, the last quarter of a persuader's sequence vs the first three ",
         "quarters: %+.2f pp (95%% CI [%+.2f, %+.2f])."),
  lq$estimate, lq$lower_ci, lq$upper_ci
)

tbl <- gt::gt(rows) |>
  style_table(
    title    = gt::md("**Human persuader workload and within-persuader order effects (Studies 1--3).**"),
    subtitle = subtitle
  )

save_table(tbl, "tab_persuader_order", TABLES_SI,
           label  = "tab:persuader_order",
           layout = "landscape_fit",
           new    = TRUE)

message("  done.")
