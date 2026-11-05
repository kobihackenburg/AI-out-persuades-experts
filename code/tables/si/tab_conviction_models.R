# tab_conviction_models.R
# SI Table: AI's advantage over human persuaders with no adjustment, adjusting
# for the three conviction markers, for log claim count, and for both
# (fact-checked Studies 1-2 conversations, unconstrained AI vs all humans).
# Reads RDS written by code/analysis/si_19_conviction_markers.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_conviction_models.R ====")

SPEC_COLS <- c(none       = "No adjustment",
               conviction = "Conviction markers",
               facts      = "Claim count",
               both       = "Both")
TERM_ROWS <- c(ai_minus_human = "AI minus human persuaders (pp)",
               maximizers     = "Maximizers (per 100 words)",
               boosters       = "Boosters (per 100 words)",
               hedges         = "Hedges (per 100 words)",
               log_n_facts    = "Log claim count")

gm <- load_rds("conviction_gap_models")

# is_human is human minus AI; flip so positive means AI more persuasive
cells <- gm |>
  dplyr::mutate(
    flip      = term == "is_human",
    row       = ifelse(flip, "ai_minus_human", term),
    est       = ifelse(flip, -estimate, estimate),
    lo        = ifelse(flip, -conf.high, conf.low),
    hi        = ifelse(flip, -conf.low, conf.high),
    value     = sprintf("%+.2f [%+.2f, %+.2f]", est, lo, hi)
  ) |>
  dplyr::select(row, spec, value)

wide <- cells |>
  tidyr::pivot_wider(names_from = spec, values_from = value, values_fill = "") |>
  dplyr::arrange(match(row, names(TERM_ROWS))) |>
  dplyr::mutate(row = unname(TERM_ROWS[row])) |>
  dplyr::select(row, dplyr::all_of(names(SPEC_COLS)))
names(wide) <- c("Term", unname(SPEC_COLS))

subtitle <- sprintf(
  paste0("LMM post ~ pre + issue + attempt + [adjustors] + is_human + (1 | persuader) + (1 | persuadee) ",
         "on fact-checked Studies 1-2 conversations with at least one claim, unconstrained AI vs all ",
         "human classes (n = %s). The first row is AI's advantage over human persuaders (pp [95%% CI]); ",
         "other rows are the adjustors' coefficients (pp per marker per 100 words, or per unit of log ",
         "claim count). The claim-count column reproduces the unconstrained-AI specification of the ",
         "fact-density analysis."),
  format(gm$n_obs[1], big.mark = ",")
)

tbl <- gt::gt(wide) |>
  style_table(
    title    = gt::md("**AI's advantage over human persuaders adjusting for conviction markers and claim count.**"),
    subtitle = subtitle
  )

save_table(tbl, "tab_conviction_models", TABLES_SI,
           label  = "tab:conviction_models",
           layout = "shrink",
           new    = TRUE)

tex_path <- file.path(TABLES_SI, "tab_conviction_models.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
