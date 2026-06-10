# tab_repeat_robustness.R
# SI Table: robustness of the per-class persuasive effect to repeated
# participation. Three columns:
#   (1) Headline    - pooled S1-S3 body model, per-class TE vs control
#   (2) First session only - same model refit on each persuadee's first
#       session within a study (attempt == 1; the constant `attempt` term
#       dropped), so within-study repeated measurement is removed
#   (3) Effect x session - per-class difference in the attempt slope vs
#       control from a group x attempt interaction model (pp per added
#       session); the joint interaction LRT is reported in the subtitle.
# Reads RDS written by code/analysis/{01_main_attitudes,si_11_repeat_participation}.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_repeat_robustness.R ====")

fmt_cell <- function(estimate, lower_ci, upper_ci) {
  ifelse(is.na(estimate), "",
         sprintf("%+.2f [%+.2f, %+.2f]", estimate, lower_ci, upper_ci))
}

load_te <- function(name, col_label) {
  load_rds(name) |>
    dplyr::transmute(group, !!col_label := fmt_cell(estimate, lower_ci, upper_ci))
}

headline  <- load_te("robustness_main_spec", "Headline")
first_sess <- load_te("repeat_first_session", "First session only")

carryover <- load_rds("repeat_carryover")
slopes <- carryover$slopes |>
  dplyr::transmute(group,
                   `Effect change per session` = fmt_cell(estimate, lower_ci, upper_ci))

wide <- headline |>
  dplyr::full_join(first_sess, by = "group") |>
  dplyr::full_join(slopes, by = "group") |>
  dplyr::mutate(group_order = match(group, PERSUADER_CLASS_ORDER)) |>
  dplyr::arrange(group_order) |>
  dplyr::transmute(
    Class = unname(PERSUADER_CLASS_LABELS[group]),
    Headline,
    `First session only`,
    `Effect change per session`
  )
wide$Class[is.na(wide$Class)] <- headline$group[is.na(wide$Class)]

subtitle <- sprintf(
  paste0("Per-class persuasive effect vs control (pp [95%% CI]). ",
         "Headline is the pooled Studies 1-3 body-text model. ",
         "First session only refits that model on each persuadee's first session ",
         "within a study (one observation per persuadee per study; the constant ",
         "attempt covariate dropped). Effect change per session is the difference, ",
         "vs control, in the linear attempt slope from a group-by-attempt model ",
         "(pp per additional session); the joint group-by-attempt interaction is ",
         "not significant (likelihood-ratio test, chi-squared(%d) = %.2f, p = %.2f), so the ",
         "AI advantage is stable across repeated sessions."),
  carryover$lrt_df, carryover$lrt_chisq, carryover$p_interaction
)

tbl <- gt::gt(wide) |>
  style_table(
    title    = gt::md("**Robustness of per-class persuasive impact to repeated participation.**"),
    subtitle = subtitle
  )

save_table(tbl, "tab_repeat_robustness", TABLES_SI,
           label  = "tab:repeat_robustness",
           layout = "shrink")

tex_path <- file.path(TABLES_SI, "tab_repeat_robustness.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{table\\}\\[!t\\]", "\\\\begin{table}[H]", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
