# tab_donation_mechanism_battery.R
# SI Table: per-item AI - Canvasser difference of means on the 14-item
# Study 4 donation-mechanism battery. One row per self-report item,
# grouped by the 7 preregistered strategies.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_donation_mechanism_battery.R ====")

mech <- load_rds("realworld_mechanism")

ITEM_INFO <- tibble::tribble(
  ~item,                 ~strategy,                       ~strategy_order, ~item_label,
  "mech_mental_image",   "Implementation Intentions",     1L, "Formed mental picture of donating",
  "mech_prior_decision", "Implementation Intentions",     1L, "Knew how/whether I would donate before page",
  "mech_commitment",     "Commitment Escalation",         2L, "Agreed to increasingly specific commitments",
  "mech_consistency",    "Commitment Escalation",         2L, "Inconsistent not to donate given commitments",
  "mech_advocacy",       "Information: Impact Efficacy",  3L, "Donating is impactful advocacy",
  "mech_impact",         "Information: Impact Efficacy",  3L, "My donation would make a real difference",
  "mech_learning",       "Information: Issue",            4L, "Learnt a lot from the conversation",
  "mech_knowledge",      "Information: Issue",            4L, "Feel more knowledgeable about the issue",
  "mech_regret",         "Anticipated Regret",            5L, "Not donating would be something I'd regret",
  "mech_disappointment", "Anticipated Regret",            5L, "Would be disappointed in myself if I didn't act",
  "mech_identity",       "Identity Labeling",             6L, "I am someone who takes action on their values",
  "mech_followthrough",  "Identity Labeling",             6L, "I am someone who follows through on beliefs",
  "mech_emotion",        "Emotional Activation",          7L, "Conversation made me feel strong emotions",
  "mech_empathy",        "Emotional Activation",          7L, "Felt empathy towards those affected"
)
stopifnot(setequal(ITEM_INFO$item, mech$item))

tbl_df <- mech |>
  dplyr::inner_join(ITEM_INFO, by = "item") |>
  dplyr::arrange(strategy_order, item) |>
  dplyr::transmute(
    Strategy         = strategy,
    Item             = item_label,
    `AI - Canvasser` = sprintf("%+.2f", estimate),
    `95% CI`         = sprintf("[%+.2f, %+.2f]", lower_ci, upper_ci),
    p                = ifelse(p_value < 0.001, "<.001", sprintf("%.3f", p_value))
  )

tbl <- gt::gt(tbl_df, groupname_col = "Strategy") |>
  style_table(
    title    = gt::md("**Per-item AI - Canvasser differences on the Study 4 donation-mechanism battery.**"),
    subtitle = "Welch's t per item (0-100 sliders); rows grouped by the 7 preregistered strategies plotted as composites in Fig. 4b. n_AI = 1,525, n_Canvasser = 1,019."
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_donation_mechanism_battery", TABLES_SI,
           label  = "tab:donation_mechanism_battery",
           layout = "auto")

tex_path <- file.path(TABLES_SI, "tab_donation_mechanism_battery.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
txt <- gsub("\\\\textbf\\{AI - Canvasser\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{AI - \\\\\\\\ Canvasser}}}", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
