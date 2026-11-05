# tab_matching_start_gap.R
# SI Table: time from session start to conversation start (pre-survey plus,
# in human conditions, the wait for a partner) by study x condition, with the
# within-human association between this start gap and the outcomes in the
# caption. Reads output/results/matching_{start_gap,wait_models}.rds (built
# by code/analysis/si_23_matching_order.R).

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_matching_start_gap.R ====")

GROUP_LABELS <- c(ai = "AI", ai_constrained = "AI (Constrained)", human = "Human (all classes)")
GROUP_ORDER  <- names(GROUP_LABELS)

gap <- load_rds("matching_start_gap")
ai_median <- gap |> dplyr::filter(group3 == "ai") |> dplyr::select(study, ai_median = median_min)

rows <- gap |>
  dplyr::left_join(ai_median, by = "study") |>
  dplyr::mutate(g_order = match(group3, GROUP_ORDER)) |>
  dplyr::arrange(study, g_order) |>
  dplyr::transmute(
    Study                 = sub("study_", "Study ", study),
    Condition             = unname(GROUP_LABELS[group3]),
    `N conversations`     = format(n, big.mark = ","),
    `Median (min)`        = sprintf("%.1f", median_min),
    `IQR (min)`           = sprintf("%.1f-%.1f", p25, p75),
    `Difference from AI median (min)` = ifelse(group3 == "ai", "", sprintf("%+.1f", median_min - ai_median))
  )

wm <- load_rds("matching_wait_models")
w_att <- dplyr::filter(wm, outcome == "post")
w_enj <- dplyr::filter(wm, outcome == "rating_enjoyment")

subtitle <- sprintf(
  paste0("Start gap is the time from the start of the persuadee's session to the start of the conversation; ",
         "it includes the pre-treatment survey and, in human conditions, the wait for a free persuader. ",
         "Control conversations have no recorded start time. Analyzed conversations only. ",
         "Within human conditions (n = %s), each additional minute of start gap (winsorized at the 99th percentile) ",
         "was associated with %+.2f pp post-treatment attitude (95%% CI [%+.2f, %+.2f]) and %+.2f points ",
         "conversation enjoyment on the 0-100 scale (95%% CI [%+.2f, %+.2f]), adjusting for pre-treatment attitude, ",
         "issue, attempt and persuader class, with persuader and persuadee random intercepts."),
  format(w_att$n, big.mark = ","),
  w_att$estimate, w_att$lower_ci, w_att$upper_ci,
  w_enj$estimate, w_enj$lower_ci, w_enj$upper_ci
)

tbl <- gt::gt(rows, groupname_col = "Study") |>
  style_table(
    title    = gt::md("**Time from session start to conversation start (Studies 1--3).**"),
    subtitle = subtitle
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_matching_start_gap", TABLES_SI,
           label  = "tab:matching_start_gap",
           layout = "fit",
           new    = TRUE)

tex_path <- file.path(TABLES_SI, "tab_matching_start_gap.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
