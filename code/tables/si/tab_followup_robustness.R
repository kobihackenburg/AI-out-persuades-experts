# tab_followup_robustness.R
# SI Table: selection into the follow-up. AI's advantage (Studies 1-3: over
# pooled human persuaders; Study 4: over Professional Canvassers)
#   Panel 1 - immediately, among persuadees who later returned vs did not
#   Panel 2 - at follow-up, under the preregistered model, inverse-probability-
#             of-return weighting, a study fixed effect, first-session and
#             single-conversation subsamples, Study 4 without the age and
#             ideology covariates, and Lee (2009) bounds.
# Reads RDS written by code/analysis/si_13_followup.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_followup_robustness.R ====")

fmt_cell <- function(estimate, lower_ci, upper_ci) {
  sprintf("%+.2f [%+.2f, %+.2f]", estimate, lower_ci, upper_ci)
}
fmt_n <- function(n) format(n, big.mark = ",")

sel <- load_rds("followup_selection_gap")
rob <- load_rds("followup_robustness")
s4r <- load_rds("followup_s4_robustness")
n_s123 <- rob$s123$n[rob$s123$spec == "main (preregistered spec)"]
n_s4   <- s4r$n[s4r$spec == "main (preregistered spec)"]

SPECS <- tibble::tribble(
  ~key,                                                  ~panel,     ~Specification,
  "immediate, responder",                                "immediate", "Persuadees who returned",
  "immediate, nonresponder",                             "immediate", "Persuadees who did not return",
  "difference (responders - non-responders)",            "immediate", "Difference (returned minus did not)",
  "main (preregistered spec)",                           "follow-up", "Preregistered model",
  "inverse-probability-of-response weighted",            "follow-up", "Inverse-probability-of-return weighted",
  "+ study fixed effect",                                "follow-up", "With study fixed effect",
  "first session only",                                  "follow-up", "First session only",
  "persuadees with one conversation in total",           "follow-up", "Persuadees with one conversation in total",
  "Lee bounds",                                          "follow-up", "Lee bounds"
)

tidy_rows <- function(gap, robust, n_immediate) {
  immediate <- gap |>
    dplyr::mutate(key = sub("^immediate AI - (human|canvasser), ", "immediate, ", spec),
                  cell = fmt_cell(estimate, lower_ci, upper_ci), n = n_immediate)
  follow_up <- robust |>
    dplyr::mutate(key = spec,
                  cell = ifelse(spec == "Lee bounds", sprintf("[%+.2f, %+.2f]", lower_ci, upper_ci),
                                fmt_cell(estimate, lower_ci, upper_ci)),
                  n = ifelse(spec == "Lee bounds", NA_integer_, n))
  dplyr::bind_rows(immediate, follow_up) |> dplyr::select(key, cell, n)
}

n_elig_s123 <- load_rds("followup_sample") |> dplyr::filter(study == "pooled S1-3") |> dplyr::pull(eligible_rows)
n_elig_s4   <- load_rds("followup_s4_sample")$rows_prereg_covariates
s123 <- tidy_rows(sel$s123, rob$s123, n_elig_s123) |>
  dplyr::rename(s123 = cell, n_s123 = n)
s4 <- tidy_rows(sel$s4, s4r, n_elig_s4) |>
  dplyr::rename(s4 = cell, n_s4 = n)

wide <- SPECS |>
  dplyr::left_join(s123, by = "key") |>
  dplyr::left_join(s4, by = "key") |>
  dplyr::mutate(
    panel = ifelse(panel == "immediate", "Immediate advantage, by later return",
                   "Advantage at follow-up"),
    `Studies 1--3: AI minus human` = dplyr::coalesce(s123, "--"),
    `n (S1--3)` = ifelse(is.na(n_s123), "", fmt_n(n_s123)),
    `Study 4: AI minus canvassers` = dplyr::coalesce(s4, "--"),
    `n (S4)` = ifelse(is.na(n_s4), "", fmt_n(n_s4))
  ) |>
  dplyr::select(panel, Specification, `Studies 1--3: AI minus human`, `n (S1--3)`,
                `Study 4: AI minus canvassers`, `n (S4)`)

lee <- rob$s123_lee
tbl <- gt::gt(wide, groupname_col = "panel") |>
  style_table(
    title    = gt::md("**Selection into the follow-up survey.**"),
    subtitle = sprintf(paste0(
      "AI minus pooled human persuaders (Studies 1--3, attitude in pp) and AI minus Professional ",
      "Canvassers (Study 4, share of bonus donated in pp), 95%% CI in brackets. Immediate advantage: ",
      "preregistered model on all eligible conversations (Studies 1--3) or the preregistered analysis ",
      "sample (Study 4) with a condition-by-return interaction. Advantage at follow-up: preregistered ",
      "model with the follow-up outcome (n = %s and %s); inverse-probability-of-return weights from a ",
      "logistic model of return on condition, study, issue, pre- and post-conversation attitude, session ",
      "number and demographics (Studies 1--3) or on condition, pre-treatment support and willingness, ",
      "immediate donation, age and ideology (Study 4), trimmed at the 99th percentile; first-session and ",
      "one-conversation subsamples drop the persuadee intercept and session term. Lee bounds trim the arm ",
      "with the higher return rate by the return-rate gap after residualising the follow-up outcome on ",
      "pre-treatment covariates (Studies 1--3: return %.1f%% AI vs %.1f%% human; Study 4: %.1f%% of the ",
      "canvasser arm trimmed). -- : not applicable."),
      fmt_n(n_s123), fmt_n(n_s4), 100 * lee$rate_first, 100 * lee$rate_second,
      100 * s4r$trim_share[s4r$spec == "Lee bounds"])
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_followup_robustness", TABLES_SI,
           label  = "tab:followup_robustness",
           layout = "shrink",
           new    = TRUE)

# Plain tabular so \resizebox can scale the table to the text width.
tex_path <- file.path(TABLES_SI, "tab_followup_robustness.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
stack_header <- function(txt, header, top, bottom) {
  gsub(sprintf("\\\\textbf\\{%s\\}", header),
       sprintf("\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{%s \\\\\\\\ %s}}}", top, bottom),
       txt)
}
txt <- stack_header(txt, "Studies 1--3: AI minus human", "Studies 1--3:", "AI minus human")
txt <- stack_header(txt, "Study 4: AI minus canvassers", "Study 4:", "AI minus canvassers")
con <- file(tex_path, open = "w", encoding = "UTF-8")
writeLines(txt, con)
close(con)

message("  done.")
