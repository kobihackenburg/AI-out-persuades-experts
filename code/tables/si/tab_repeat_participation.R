# tab_repeat_participation.R
# Two SI tables documenting repeated persuadee participation across all five
# studies:
#   tab_repeat_participation.tex - sessions-per-persuadee distribution
#   tab_repeat_overlap.tex       - 5x5 cross-study persuadee-overlap matrix
# Both read RDS written by code/analysis/si_11_repeat_participation.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_repeat_participation.R ====")

STUDY_LABEL <- c(
  study_0 = "Study 0 (tournament)",
  study_1 = "Study 1",
  study_2 = "Study 2",
  study_3 = "Study 3",
  study_4 = "Study 4"
)

force_here <- function(name) {
  tex_path <- file.path(TABLES_SI, paste0(name, ".tex"))
  txt <- readLines(tex_path, encoding = "UTF-8")
  txt <- gsub("\\\\begin\\{table\\}\\[!t\\]", "\\\\begin{table}[H]", txt)
  writeLines(txt, tex_path, useBytes = FALSE)
}

# ---- Panel 1: sessions-per-persuadee distribution -----------------------
desc <- load_rds("repeat_descriptives")

sessions_df <- desc |>
  dplyr::mutate(Study = unname(STUDY_LABEL[study])) |>
  dplyr::transmute(
    Study,
    `1`            = format(`1`, big.mark = ","),
    `2`            = format(`2`, big.mark = ","),
    `3`            = format(`3`, big.mark = ","),
    `4`            = format(`4`, big.mark = ","),
    `5+`           = format(`5+`, big.mark = ","),
    Persuadees     = format(n_persuadees_total, big.mark = ","),
    Conversations  = format(n_sessions_total, big.mark = ","),
    Mean           = sprintf("%.1f", mean_sessions),
    Max            = format(max_sessions, big.mark = ",")
  )

tbl_sessions <- gt::gt(sessions_df) |>
  style_table(
    title    = gt::md("**Repeated participation: sessions per persuadee, by study.**"),
    subtitle = "Columns 1-5+ count unique persuadees in each study's analytic sample who completed exactly that many conversations (5+ pools five or more). Persuadees gives the total unique persuadees, Conversations the total retained conversations, and Mean / Max the per-persuadee session count. Studies 1-3 assigned a new, previously-unseen issue at each session; Study 4 was a single-session donation design."
  )

save_table(tbl_sessions, "tab_repeat_participation", TABLES_SI,
           label  = "tab:repeat_participation",
           layout = "auto")
force_here("tab_repeat_participation")

# ---- Panel 2: cross-study persuadee-overlap matrix ----------------------
overlap <- load_rds("repeat_overlap")
mat <- overlap$overlap_matrix

overlap_df <- mat |>
  dplyr::mutate(Study = unname(STUDY_LABEL[study])) |>
  dplyr::transmute(
    Study,
    S0 = format(study_0, big.mark = ","),
    S1 = format(study_1, big.mark = ","),
    S2 = format(study_2, big.mark = ","),
    S3 = format(study_3, big.mark = ","),
    S4 = format(study_4, big.mark = ",")
  )

mc <- overlap$membership_counts
n_one    <- mc$n_persuadees[mc$n_pools == 1L]
n_multi  <- sum(mc$n_persuadees[mc$n_pools > 1L])

overlap_subtitle <- sprintf(
  paste0("Counts of unique persuadees shared between each pair of study pools. ",
         "The diagonal is each study's number of unique persuadees; off-diagonal ",
         "cells count persuadees appearing in both studies. Across all five pools, ",
         "%s persuadees participated in only one study and %s in more than one; ",
         "the main attitude-study pool (Studies 1-3) is nearly disjoint from both ",
         "the Study 0 tournament pool and the Study 4 donation pool."),
  format(n_one, big.mark = ","), format(n_multi, big.mark = ",")
)

tbl_overlap <- gt::gt(overlap_df) |>
  style_table(
    title    = gt::md("**Cross-study persuadee overlap.**"),
    subtitle = overlap_subtitle
  )

save_table(tbl_overlap, "tab_repeat_overlap", TABLES_SI,
           label  = "tab:repeat_overlap",
           layout = "auto")
force_here("tab_repeat_overlap")

message("  done.")
