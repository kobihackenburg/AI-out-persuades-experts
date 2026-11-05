# tab_study4_lmm_coefs.R
# SI Table: full fixed-effects coefficient table for the Study 4
# preregistered donation LMM
# (donation ~ pre_support + pre_willingness + age + ideology + group +
#             (1|persuader)).

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_study4_lmm_coefs.R ====")

m  <- load_rds("study4_donation_lmm")
fe <- summary(m)$coefficients
p_col <- if ("Pr(>|t|)" %in% colnames(fe)) "Pr(>|t|)" else NA_character_

fmt_p <- function(p) {
  ifelse(is.na(p), "--",
         ifelse(p < 0.001, "<.001", sprintf("%.3f", p)))
}

tbl_df <- tibble::as_tibble(fe, rownames = "term") |>
  dplyr::transmute(
    Term     = term,
    Estimate = sprintf("%+0.3f", Estimate),
    SE       = sprintf("%.3f",   `Std. Error`),
    `t`      = sprintf("%.2f",   `t value`),
    p        = if (!is.na(p_col)) fmt_p(.data[[p_col]]) else "--"
  )

n_per <- lme4::ngrps(m)[["persuader"]]
vc <- as.data.frame(lme4::VarCorr(m))
re_sd <- setNames(vc$sdcor, vc$grp)
re_sentence <- sprintf("Random-intercept standard deviation for persuader: %.2f; residual standard deviation: %.2f.",
                       re_sd[["persuader"]], re_sd[["Residual"]])

tbl <- gt::gt(tbl_df) |>
  style_table(
    title    = gt::md("**Study 4 donation LMM -- full fixed-effects coefficient table.**"),
    subtitle = sprintf("donation ~ pre_support + pre_willingness + age + ideology + group + (1|persuader). N = %d; n_persuader = %d. Reference: group = control. %s",
                       nobs(m), n_per, re_sentence)
  )

save_table(tbl, "tab_study4_lmm_coefs", TABLES_SI,
           label  = "tab:study4_lmm_coefs",
           layout = "auto")

# Force [H] float placement
local({
  path <- file.path(TABLES_SI, "tab_study4_lmm_coefs.tex")
  txt  <- readLines(path)
  txt  <- sub("\\\\begin\\{table\\}\\[!t\\]", "\\\\begin{table}[H]", txt)
  if (!any(grepl(re_sentence, txt, fixed = TRUE))) stop("random-effect sentence not found in caption")
  txt  <- sub(re_sentence, paste0("\\added{", re_sentence, "}"), txt, fixed = TRUE)
  writeLines(txt, path)
})
message("  done.")
