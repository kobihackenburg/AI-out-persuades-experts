# tab_donation_margins.R
# SI Table: extensive + intensive donation margin decomposition for
# Study 4. Reads output/results/realworld_margins.rds, a 2-element
# list:
#   * per_group: per-condition Wilson-CI share of donors and Welch-CI
#                mean donation among donors.
#   * deltas:    closed-form AI - Canvasser deltas for both margins.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_donation_margins.R ====")

m <- load_rds("realworld_margins")

GROUP_LABELS <- c(control = "Control", canvasser = "Canvasser", ai = "AI")
GROUP_ORDER  <- c("control", "canvasser", "ai")

desc_df <- m$per_group |>
  dplyr::mutate(group = factor(as.character(group), levels = GROUP_ORDER)) |>
  dplyr::arrange(group) |>
  dplyr::transmute(
    Condition = unname(GROUP_LABELS[as.character(group)]),
    n         = format(n, big.mark = ","),
    `Donors / total`              = sprintf("%s / %s",
                                             format(n_donate, big.mark = ","),
                                             format(n,        big.mark = ",")),
    `% donating (95% CI)`         = sprintf("%.1f [%.1f, %.1f]",
                                             p_donate_pct, lo_ext, hi_ext),
    `Mean donation | donor (95% CI), pence` = sprintf("%.1f [%.1f, %.1f]",
                                                       mean_donor, lo_int, hi_int)
  )

delta_df <- m$deltas |>
  dplyr::mutate(
    Margin = dplyr::recode(margin,
                           extensive = "Extensive (share donating, pp)",
                           intensive = "Intensive (mean donation | donor, pence)")
  ) |>
  dplyr::select(Margin, estimate, lower_ci, upper_ci) |>
  dplyr::transmute(
    Margin,
    `AI - Canvasser` = sprintf("%+.2f", estimate),
    `95% CI`         = sprintf("[%+.2f, %+.2f]", lower_ci, upper_ci)
  )

desc_tbl <- gt::gt(desc_df) |>
  style_table(
    title    = gt::md("**Donation extensive + intensive margin descriptives by condition (Study 4).**"),
    subtitle = "Wilson 95% CI on the share donating; Welch t 95% CI on the mean donation among donors. Pence of a 100-pence (GBP 1) bonus."
  )

delta_tbl <- gt::gt(delta_df) |>
  style_table(
    title    = gt::md("**AI - Canvasser margin deltas (Study 4).**"),
    subtitle = "Closed-form Wald 95% CI on the extensive margin; Welch t 95% CI on the intensive margin."
  )

save_table(desc_tbl,  "tab_donation_margins",        TABLES_SI,
           label = "tab:donation_margins",
           layout = "auto")

tex_path <- file.path(TABLES_SI, "tab_donation_margins.tex")
local({
  txt <- readLines(tex_path)
  txt <- sub("\\\\begin\\{table\\}\\[!t\\]", "\\\\begin{table}[H]", txt)
  writeLines(txt, tex_path)
})
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
txt <- gsub("\\\\textbf\\{Donors / total\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Donors / \\\\\\\\ total}}}", txt)
txt <- gsub("\\\\textbf\\{\\\\% donating \\(95\\\\% CI\\)\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{\\\\% donating \\\\\\\\ (95\\\\% CI)}}}", txt)
txt <- gsub("\\\\textbf\\{Mean donation \\| donor \\(95\\\\% CI\\), pence\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Mean donation | donor \\\\\\\\ (95\\\\% CI), pence}}}", txt)
writeLines(txt, tex_path, useBytes = FALSE)

save_table(delta_tbl, "tab_donation_margins_deltas", TABLES_SI,
           label = "tab:donation_margins_deltas",
           layout = "auto")

tex_path2 <- file.path(TABLES_SI, "tab_donation_margins_deltas.tex")
local({
  txt <- readLines(tex_path2)
  txt <- sub("\\\\begin\\{table\\}\\[!t\\]", "\\\\begin{table}[H]", txt)
  writeLines(txt, tex_path2)
})
txt2 <- readLines(tex_path2, encoding = "UTF-8")
txt2 <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
             "\\\\begin{tabular}{", txt2)
txt2 <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt2)
txt2 <- gsub("\\\\textbf\\{AI - Canvasser\\}",
             "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{AI - \\\\\\\\ Canvasser}}}", txt2)
writeLines(txt2, tex_path2, useBytes = FALSE)

message("  done.")
