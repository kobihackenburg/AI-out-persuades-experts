# tab_donation_partner_perception.R
# SI Table: per-item AI - Canvasser difference of means on the 7
# partner-rating items in Study 4.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_donation_partner_perception.R ====")

perc <- load_rds("realworld_perception")

ITEM_INFO <- tibble::tribble(
  ~item,              ~item_label,                                  ~item_order,
  "rating_arguments", "Made strong arguments",                      1L,
  "rating_learning",  "I learned a lot",                            2L,
  "rating_empathy",   "Felt understood by partner",                 3L,
  "rating_enjoyment", "Conversation was enjoyable",                 4L,
  "rating_anthropo",  "Partner interacted like a human",            5L,
  "rating_deception", "Partner lied or made things up",             6L,
  "rating_bias",      "Partner was biased toward one side",         7L
)
stopifnot(setequal(ITEM_INFO$item, perc$item))

tbl_df <- perc |>
  dplyr::inner_join(ITEM_INFO, by = "item") |>
  dplyr::arrange(item_order) |>
  dplyr::transmute(
    Item             = item_label,
    `AI - Canvasser` = sprintf("%+.2f", estimate),
    `95% CI`         = sprintf("[%+.2f, %+.2f]", lower_ci, upper_ci),
    p                = ifelse(p_value < 0.001, "<.001", sprintf("%.3f", p_value))
  )

tbl <- gt::gt(tbl_df) |>
  style_table(
    title    = gt::md("**Per-item AI - Canvasser differences on the Study 4 partner-rating battery.**"),
    subtitle = "Welch's t per item (0-100 sliders); higher = AI rated higher than canvassers on the item as worded. n_AI = 1,526, n_Canvasser = 1,019."
  )

save_table(tbl, "tab_donation_partner_perception", TABLES_SI,
           label  = "tab:donation_partner_perception",
           layout = "auto")

tex_path <- file.path(TABLES_SI, "tab_donation_partner_perception.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
txt <- gsub("\\\\textbf\\{AI - Canvasser\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{AI - \\\\\\\\ Canvasser}}}", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
