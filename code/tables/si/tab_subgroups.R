# tab_subgroups.R
# SI Table: 14-subgroup x 48-level AI-vs-pooled-humans estimates,
# the table backing the subgroup figure.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_subgroups.R ====")

# Backs the main-text mechanism-section claim, so reads the Studies 1-2
# refit. Canvasser-inclusion analogue lives in
# Section~\ref{si:mechanism-with-canvasser}.
sg <- load_rds("subgroups_ai_vs_humans_s1_s2")

SUBGROUP_LABELS <- c(
  gender                  = "Gender",
  age_bucket              = "Age bucket",
  ethnicity               = "Ethnicity",
  education               = "Education",
  income                  = "Income",
  party                   = "Party affiliation",
  ideology_text           = "Ideology (5-pt)",
  political_knowledge_hi  = "Political knowledge (median split)",
  ai_trust_high           = "AI trust (median split)",
  mod_dogmatism           = "Dogmatism (median split)",
  mod_pre_agreement       = "Pre-treatment agreement (>=/< 50)",
  mod_ideology            = "Ideology (left/right)",
  mod_issue_knowledge     = "Issue knowledge (median split)",
  empathic_trust_high     = "Epistemic trust (median split)"
)

tbl_df <- sg |>
  dplyr::mutate(
    Subgroup_lbl = unname(ifelse(is.na(SUBGROUP_LABELS[as.character(subgroup)]),
                                 as.character(subgroup),
                                 SUBGROUP_LABELS[as.character(subgroup)])),
    sg_order = match(subgroup, names(SUBGROUP_LABELS))
  ) |>
  dplyr::arrange(sg_order, level) |>
  dplyr::transmute(
    Subgroup    = Subgroup_lbl,
    Level       = level,
    n           = format(n, big.mark = ","),
    Estimate    = sprintf("%+.2f", estimate),
    `95% CI`    = sprintf("[%+.2f, %+.2f]", lower_ci, upper_ci),
    p           = ifelse(p_value < 0.001, "<.001", sprintf("%.3f", p_value))
  )

n_sig <- sum(sg$p_value < 0.05, na.rm = TRUE)

tbl <- gt::gt(tbl_df, groupname_col = "Subgroup") |>
  style_table(
    title    = gt::md("**AI advantage over pooled humans across 14 persuadee subgroups.**"),
    subtitle = sprintf("Per-(subgroup x level) LMM contrast AI - mean(human classes). Significant at p < .05 in %d of %d levels.",
                       n_sig, nrow(sg))
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_subgroups", TABLES_SI,
           label  = "tab:subgroups",
           layout = "longtable")
message("  done.")
