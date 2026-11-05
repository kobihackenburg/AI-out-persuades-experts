# tab_repeat_robustness.R
# Two SI tables on robustness to repeated participation:
#   tab_repeat_robustness.tex - per-class persuasive effect vs control.
#     Three columns:
#     (1) Headline    - pooled S1-S3 body model, per-class TE vs control
#     (2) First session only - same model refit on each persuadee's first
#         session within a study (attempt == 1; the constant `attempt` term
#         dropped), so within-study repeated measurement is removed
#     (3) Effect x session - per-class difference in the attempt slope vs
#         control from a group x attempt interaction model (pp per added
#         session); the joint interaction LRT is reported in the subtitle.
#   tab_repeat_ai_minus_human.tex - AI minus all human persuaders pooled,
#     under cross-study definitions of repeated participation (same-issue
#     repeats, attempt and first session counted across studies) and by
#     first vs repeat conversation on an issue.
# Reads RDS written by code/analysis/{01_main_attitudes,si_11_repeat_participation}.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_repeat_robustness.R ====")

fmt_cell <- function(estimate, lower_ci, upper_ci) {
  ifelse(is.na(estimate), "",
         sprintf("%+.2f [%+.2f, %+.2f]", estimate, lower_ci, upper_ci))
}

force_here <- function(name) {
  tex_path <- file.path(TABLES_SI, paste0(name, ".tex"))
  txt <- readLines(tex_path, encoding = "UTF-8")
  txt <- gsub("\\\\begin\\{table\\}\\[!t\\]", "\\\\begin{table}[H]", txt)
  writeLines(txt, tex_path, useBytes = FALSE)
}

# ---- Per-class effects vs control -----------------------------------------
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
           layout = "fit")
force_here("tab_repeat_robustness")

# ---- AI minus human persuaders under cross-study definitions ---------------
SPEC_LABELS <- c(
  all                         = "All conversations",
  first_on_issue              = "First conversation on each issue",
  no_repeated_issues          = "Excluding all conversations on repeated issues",
  attempt_global              = "Attempt counted across studies",
  first_session_global        = "First session across all studies",
  exposure_first              = "First conversation on the issue",
  exposure_repeat             = "Repeat conversation on the issue",
  exposure_repeat_minus_first = "Repeat minus first"
)
PANEL_REFITS   <- "Refits of the preregistered model"
PANEL_EXPOSURE <- "By exposure to the issue (condition-by-exposure model)"

ai_minus <- load_rds("repeat_ai_minus_human") |>
  dplyr::mutate(
    Panel = ifelse(startsWith(spec, "exposure_"), PANEL_EXPOSURE, PANEL_REFITS),
    Specification = unname(SPEC_LABELS[spec]),
    `AI minus human` = fmt_cell(estimate, lower_ci, upper_ci),
    Conversations = ifelse(is.na(n), "", formatC(n, format = "d", big.mark = ","))
  ) |>
  dplyr::select(Panel, Specification, `AI minus human`, Conversations)

ai_minus_subtitle <- paste0(
  "Difference in persuasive effect between AI (unconstrained) and all human ",
  "persuaders pooled (pp [95% CI]), from the pooled Studies 1-3 preregistered ",
  "model with condition coded as control, AI, Constrained AI or human. Persuadees ",
  "who took part in more than one study could discuss the same issue again ",
  "(a repeated issue). First conversation on each issue drops second and third ",
  "conversations on a repeated issue; Excluding all conversations on repeated ",
  "issues drops every conversation on one; Attempt counted across studies numbers ",
  "each persuadee's sessions across Studies 1-3 rather than within each study; ",
  "First session across all studies keeps each persuadee's first session in any ",
  "study (one observation per persuadee; persuadee intercept and attempt ",
  "covariate dropped). The lower panel adds a condition-by-exposure interaction ",
  "to the all-conversations model; Conversations gives the number of first and ",
  "repeat conversations."
)

tbl_ai_minus <- gt::gt(ai_minus, groupname_col = "Panel") |>
  style_table(
    title    = gt::md("**AI's advantage over human persuaders, accounting for same-issue repeats across studies.**"),
    subtitle = ai_minus_subtitle
  )

save_table(tbl_ai_minus, "tab_repeat_ai_minus_human", TABLES_SI,
           label  = "tab:repeat_ai_minus_human",
           layout = "auto", new = TRUE)
force_here("tab_repeat_ai_minus_human")

message("  done.")
