# tab_strategy_info_led_share.R
# SI Table: share of human conversations that were non-information-led
# (some non-informational strategy rated above "facts and evidence"), by
# human persuader class, under the main definition and excluding open
# questions / reflective listening (Studies 1-3).
# Reads RDS written by code/analysis/si_19_persuader_strategies.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_strategy_info_led_share.R ====")

CLASS_LABELS <- c(random_lay_person     = "Random Laypeople",
                  elite_lay_person      = "Selected Laypeople",
                  elite_debater         = "Elite Debaters",
                  coached_elite_debater = "Coached Elite Debaters",
                  canvasser             = "Professional Canvassers",
                  all_humans            = "All human persuaders")

cov <- load_rds("strategy_info_led_coverage")

n_conv <- cov |>
  dplyr::filter(definition == "main", persuader_class != "all_humans") |>
  dplyr::mutate(n = human_info_led + human_non_info_led)
n_conv <- dplyr::bind_rows(n_conv, tibble::tibble(persuader_class = "all_humans", n = sum(n_conv$n)))

wide <- cov |>
  dplyr::transmute(persuader_class, definition, value = sprintf("%.1f", 100 * share_non_info_led)) |>
  tidyr::pivot_wider(names_from = definition, values_from = value) |>
  dplyr::left_join(dplyr::select(n_conv, persuader_class, n), by = "persuader_class") |>
  dplyr::arrange(match(persuader_class, names(CLASS_LABELS))) |>
  dplyr::transmute(Class = unname(CLASS_LABELS[persuader_class]),
                   Conversations = format(n, big.mark = ","),
                   `Main definition` = main,
                   `Excluding open questions, reflective listening` = no_deep)

subtitle <- paste0(
  "Percentage of human persuaders' conversations (Studies 1-3) that were non-information-led, i.e. in ",
  "which the persuader rated at least one non-informational strategy higher than \"I shared lots of new, ",
  "well-sourced facts and evidence\". Main definition: the non-informational strategies are telling a ",
  "story, framing the issue in terms of the partner's values, and beginning with open questions and ",
  "reflective listening. The last column drops open questions and reflective listening from that set. ",
  "Conversations: human conversations with a strategy report."
)

tbl <- gt::gt(wide) |>
  style_table(
    title    = gt::md("**Share of non-information-led human conversations by persuader class (%).**"),
    subtitle = subtitle
  )

save_table(tbl, "tab_strategy_info_led_share", TABLES_SI,
           label  = "tab:strategy_info_led_share",
           new    = TRUE)

tex_path <- file.path(TABLES_SI, "tab_strategy_info_led_share.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\textbf{Excluding open questions, reflective listening}",
            "\\textbf{\\raisebox{-0.5\\height}{\\shortstack{Excluding open questions, \\\\ reflective listening}}}",
            txt, fixed = TRUE)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
