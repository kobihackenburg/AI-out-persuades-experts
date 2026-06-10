# tab_pooled_lmm_coefs.R
# SI Table: full fixed-effects coefficient table for the pooled S1-S3
# body-text LMM (post ~ pre + issue + group + attempt + RE).

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_pooled_lmm_coefs.R ====")

m  <- load_rds("pooled_s1_s3_lmm")
fe <- summary(m)$coefficients
p_col <- if ("Pr(>|t|)" %in% colnames(fe)) "Pr(>|t|)" else NA_character_

fmt_p <- function(p) {
  ifelse(is.na(p), "--",
         ifelse(p < 0.001, "<.001", sprintf("%.3f", p)))
}

# Issue and group labels mirror the manuscript's display names so the
# coefficient table reads naturally; anything we don't recognise falls
# through to a humanised "Title case" version of the raw term name.
ISSUE_LABELS <- c(
  assisted_suicide      = "physician-assisted suicide",
  benefit_cap           = "two-child benefit cap",
  controversial_speech  = "controversial speech at universities",
  historic_objects      = "return historic objects",
  immigration           = "allow more immigrants",
  monarchy              = "keep the monarchy",
  pension_age           = "raise the state pension age",
  protester_penalties   = "tougher protester penalties",
  social_media_ban      = "social-media ban for under-16s",
  ukraine_peace_deal    = "Ukraine peace deal"
)
GROUP_LABELS <- c(
  ai                    = "AI",
  canvasser             = "professional canvasser",
  control               = "control",
  coached_elite_debater = "coached elite debater",
  elite_debater         = "elite debater",
  elite_lay_person      = "tournament-selected layperson",
  random_lay_person     = "random layperson"
)

pretty_term <- function(term) {
  out <- term
  out <- ifelse(out == "(Intercept)",                "Intercept", out)
  out <- ifelse(out %in% c("pre", "pre_attitude"),   "Pre-treatment attitude", out)
  out <- ifelse(out == "attempt",                    "Attempt", out)

  is_issue <- startsWith(out, "issue")
  is_group <- startsWith(out, "group")
  issue_key <- sub("^issue", "", out)
  group_key <- sub("^group", "", out)

  out <- ifelse(is_issue,
                paste0("Issue: ", ifelse(issue_key %in% names(ISSUE_LABELS),
                                         ISSUE_LABELS[issue_key], issue_key)),
                out)
  out <- ifelse(is_group,
                paste0("Group: ", ifelse(group_key %in% names(GROUP_LABELS),
                                         GROUP_LABELS[group_key], group_key)),
                out)
  out
}

raw_tbl <- tibble::as_tibble(fe, rownames = "term")
tbl_df <- raw_tbl |>
  dplyr::transmute(
    Term     = pretty_term(term),
    Estimate = sprintf("%+0.3f", Estimate),
    SE       = sprintf("%.3f",   `Std. Error`),
    `t`      = sprintf("%.2f",   `t value`),
    p        = if (!is.na(p_col)) fmt_p(.data[[p_col]]) else "--"
  )

vc <- as.data.frame(lme4::VarCorr(m))
n_per <- lme4::ngrps(m)[["persuader"]]
n_pee <- lme4::ngrps(m)[["persuadee"]]

tbl <- gt::gt(tbl_df) |>
  style_table(
    title    = gt::md("**Pooled S1-S3 LMM -- full fixed-effects coefficient table.**"),
    subtitle = sprintf("Fixed effects from a linear mixed-effects model regressing post-treatment attitude on pre-treatment attitude, policy issue, persuader-class group, and attempt number, with crossed random intercepts for persuader and persuadee. Reference levels are Control for Group (so each Group coefficient is the per-class average treatment effect vs. Control) and the alphabetically first issue, physician-assisted suicide, for Issue; each non-reference coefficient is the mean difference vs. that reference, holding all other covariates fixed. N obs = %d; n_persuader = %d; n_persuadee = %d. Random-effects standard deviations are reported in the SI prose above.",
                       nobs(m), n_per, n_pee)
  )

save_table(tbl, "tab_pooled_lmm_coefs", TABLES_SI,
           label  = "tab:pooled_lmm_coefs",
           layout = "auto")

tex_path <- file.path(TABLES_SI, "tab_pooled_lmm_coefs.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{table\\}\\[!t\\]", "\\\\begin{table}[H]", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
