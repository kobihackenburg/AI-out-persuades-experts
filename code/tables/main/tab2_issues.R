# tab2_issues.R
# Main-text Table 2: the 10 prespecified UK policy stances used as
# treatment topics across Studies 1--3. Fully static -- no data-driven
# cells -- but it lives here (rather than as an inline table block in
# manuscript.tex) so that:
#   * manuscript.tex \input's it just like tab1_human_conditions.tex,
#     keeping all main-text tables in output/tables/main/.
#   * `make clean && make tables` reproduces the file: `make clean`
#     wipes everything under output/, and the `make tables` target
#     globs code/tables/main/*.R, so this script must exist for the
#     pipeline to round-trip.
#
# If the stance list ever changes, edit the template below directly.
#
# Output: output/tables/main/tab2_issues.tex
# Hook-up: the manuscript \input's this file with a bare
# `\input{tab2_issues.tex}` (no path prefix), matching tab1.

source(here::here("code/analysis/_setup.R"))

message("\n==== code/tables/main/tab2_issues.R ====")

template <- '\\begin{table}[!htbp]
\\centering
\\small
\\caption{\\textbf{Policy stances used in Studies 1--3.}}
\\label{tab:issues}

\\renewcommand{\\arraystretch}{1.35}
\\setlength{\\tabcolsep}{4pt}

\\begin{tabular}{@{} c p{0.88\\textwidth} @{}}
\\toprule
& \\textbf{Stance} \\\\
\\midrule
\\rowcolor{gray!6}
1 & The UK should return historic objects taken from other countries (e.g., Parthenon Marbles, Benin Bronzes), even if UK museums lose exhibits and tourism income. \\\\
2 & The UK should abolish the two-child benefit cap, even if it increases welfare spending. \\\\
\\rowcolor{gray!6}
3 & The UK should allow more immigrants, even if it puts a strain on public services. \\\\
4 & The UK should keep the monarchy, even if it costs taxpayers and keeps an unelected head of state. \\\\
\\rowcolor{gray!6}
5 & The UK should legalise physician-assisted suicide, even if some ill or disabled people feel pressure to choose it. \\\\
6 & The UK should ban all forms of social media for under 16s, even if that goes against their wishes. \\\\
\\rowcolor{gray!6}
7 & The UK should back a peace deal where Ukraine gives up some territory, even if Russia keeps that land. \\\\
8 & The UK should impose tougher penalties on peaceful protesters who block roads, rail or energy sites, even if they are more frequently arrested and get longer sentences. \\\\
\\rowcolor{gray!6}
9 & The UK should protect controversial speech at universities, even if many consider it racist or harmful. \\\\
10 & The UK should raise the state pension age, even if more people in demanding jobs work into their late 60s. \\\\
\\bottomrule
\\end{tabular}

\\end{table}
'

if (!dir.exists(TABLES_MAIN)) {
  dir.create(TABLES_MAIN, recursive = TRUE, showWarnings = FALSE)
}
path <- file.path(TABLES_MAIN, "tab2_issues.tex")
writeLines(template, path)
message(sprintf("  wrote %s", path))
message("  done.")
