# tab1_human_conditions.R
# Main-text Table 1: incentives, compensation, preparation, and sample
# sizes across the five human-persuader conditions. The hand-stylised
# LaTeX (alternating \rowcolor + multi-line \shortstack headers and
# cells, all notes consolidated into the caption) is too custom for
# gt's LaTeX backend to render cleanly, so this script emits the table
# from a sprintf template with only the data-driven sample-size cells
# substituted in.
#
# Note on \shortstack vs \makecell: multi-line cells use \shortstack
# (LaTeX core, \vbox-based) rather than \makecell (sub-tabular based)
# because \rowcolor{} from colortbl leaves visible white breaks at
# column boundaries when crossing a \makecell cell -- the inner
# tabular's own \tabcolsep gutters land on top of the outer row
# coloring. \shortstack has no inner tabular, so the row strip stays
# unbroken across multi-line rows.
#
# Note on \multirow in multi-line rows: \shortstack typesets a 2-line
# \vbox whose baseline sits at its bottom line, so when sharing a
# tabular row with single-line cells (a bold row label, or an \xmark
# in a no-bonus column), the single-line cells baseline-align to the
# bottom of the stack and read as visually bottom-aligned. A per-cell
# \raisebox{0.5\baselineskip}{...} does not resolve this: with
# \arraystretch{1.35} inflating the row strut, the lift is swallowed by
# the strut's depth. Instead, each multi-line row is restructured as two
# physical tabular rows, with
# \multirow{...}{*}{...} wrapping the row label (and the no-bonus
# \xmark cells in Per-conv Bonus / Live Practice). \multirow centers
# its content vertically across the N spanned rows by construction,
# so labels and \xmark cells now sit at the visual midpoint without
# depending on \baselineskip arithmetic.
#
# Note on \multirow + \rowcolor z-order: positive-count multirow
# (\multirow{2}{*}{...} placed in the FIRST spanned row) renders the
# multirow content during processing of row 1, then \rowcolor for row
# 2 is drawn AFTER, painting its gray rectangle on top of the bottom
# half of the multirow content -- the label visually clips at the row
# boundary. The colortbl-documented fix is to use NEGATIVE row count
# placed in the LAST spanned row: \multirow{-2}{*}{...} in row 2 (with
# row 1's first cell empty). Both rowcolor rectangles then draw first
# during their respective row processing, and the multirow content is
# rendered last on top of the gray, so nothing clips. This pattern is
# applied to the gray-striped rows only (Top-Performer Bonus and Live
# Practice). Per-conv Bonus has no \rowcolor and so keeps the simpler
# positive-count form (\multirow{2}{*}{...} in row 1).
#
# Both halves of each striped multi-line row carry \rowcolor{gray!6}
# so the gray strip remains continuous across both physical rows.
# \multirow has no inner tabular (unlike \makecell), so it does not
# reintroduce the column-gutter break that motivated the \shortstack
# switch.
#
# What's data-driven:
#   * N Persuaders per class -- dplyr::n_distinct(persuader_id) on the
#     post-complete-case-filter rows that the matching analysis script
#     uses for that class's outcome.
#   * N Conversations per class -- nrow() on the same filtered rows.
#   * Professional Canvassers contributed to Studies 3 (attitude) and
#     4 (donation); the script computes each study's counts separately
#     and renders them as "S3 / S4" (matching the Study row's "3 / 4").
#
# Everything else (incentives, base salaries, prep hours, practice-
# conversation counts, issue selection / coaching flags) is static
# study-design metadata; if any of those need to change, edit the
# template below directly.
#
# Output: output/tables/main/tab1_human_conditions.tex
# Hook-up: the manuscript \input's this file with a bare
# `\input{tab1_human_conditions.tex}` (no path prefix). The canonical
# pipeline artefact lives at output/tables/main/tab1_human_conditions.tex
# (matching the rest of the code/tables -> output/tables/ flow).

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))

message("\n==== code/tables/main/tab1_human_conditions.R ====")

# =============================================================================
# 1. Per-class N Persuaders + N Conversations from the data
# =============================================================================
# Mirrors the per-study analysis-script filters so the cell counts
# always match the denominators feeding Fig 1 / Fig 3 / SI tables:
#   * S1, S2, S3 -> complete_case_filter(require_attitudes = TRUE)
#                   (same as 01_main_attitudes.R)
#   * S4         -> complete_case_filter(require_attitudes = FALSE) +
#                   explicit !is.na(donation_amount) (same as
#                   03_donation.R::donation_df).
counts_for <- function(study_id, class) {
  s <- load_study(study_id) |>
    complete_case_filter(
      require_attitudes = (study_id != "study_4"),
      label             = paste0("tab1/", study_id, "/", class)
    )
  if (study_id == "study_4") {
    s <- s |> dplyr::filter(!is.na(donation_amount))
  }
  rows <- s |> dplyr::filter(persuader_class == class)
  list(
    n_persuaders   = dplyr::n_distinct(rows$persuader_id),
    n_conversations = nrow(rows)
  )
}

rl  <- counts_for("study_1", "random_lay_person")
sl  <- counts_for("study_1", "elite_lay_person")
ed  <- counts_for("study_1", "elite_debater")
ced <- counts_for("study_2", "coached_elite_debater")
c3  <- counts_for("study_3", "canvasser")
c4  <- counts_for("study_4", "canvasser")

fmt_n <- function(x) formatC(x, format = "d", big.mark = ",")

# Slash-separated cells for the Professional Canvassers column.
canv_persuaders   <- sprintf("%s / %s", fmt_n(c3$n_persuaders),
                                        fmt_n(c4$n_persuaders))
canv_conversations <- sprintf("%s / %s", fmt_n(c3$n_conversations),
                                         fmt_n(c4$n_conversations))

message(sprintf(
  "  cells: Random=%s/%s, Selected=%s/%s, Canvassers=%s/%s, Elite=%s/%s, Coached=%s/%s",
  fmt_n(rl$n_persuaders),  fmt_n(rl$n_conversations),
  fmt_n(sl$n_persuaders),  fmt_n(sl$n_conversations),
  canv_persuaders,         canv_conversations,
  fmt_n(ed$n_persuaders),  fmt_n(ed$n_conversations),
  fmt_n(ced$n_persuaders), fmt_n(ced$n_conversations)
))

# =============================================================================
# 2. LaTeX template
# =============================================================================
# Row order (top -> bottom) and \rowcolor alternation chosen to give a
# clean gray/white/gray/... cadence after the new "N Conversations" row
# is inserted under "N Persuaders" (the legacy table didn't have a
# consistent alternation through midrules; we tighten it here).
# Structure: plain \begin{table} + \caption + \label + \begin{tabular}
# (no threeparttable wrapper, no tablenotes block). All notes are
# consolidated into the caption, which follows a consistent
# "\textbf{Column:} definition; caveats." pattern in table-row order,
# with the Coached Elite Debaters class-level caveat deferred to a
# single end-of-caption sentence rather than wedged between column
# definitions:
#   * Bold title sentence (Nature style: \textbf{...} on the opening
#       descriptive sentence, mirroring the figure captions' bolded
#       title-sentence convention; descriptive rather than finding-
#       style since the table presents study-design metadata, not a
#       result). Then a body block: Professional Canvassers two-
#       study clarification, then \cmark/\xmark legend.
#   * \textbf{Top-Performer Bonus:} performance-prize definition +
#       explicit tiered amounts (\pounds1{,}000/750/500/250 for
#       1st--4th place; \pounds10 flat for Random Laypeople top
#       10\%). Cells abbreviate the tier as \pounds1k--\pounds250 on
#       physical row 1 with the qualifier on row 2; the bold row
#       label is vertically centered via \multirow.
#   * \textbf{Per-conv Bonus (vs.\ AI):} formula \pounds15 $\times$
#       persuader effectiveness relative to AI. Cells span two
#       physical rows: formula on row 1, class-mean realized per-
#       conversation payout in parentheses on row 2. Per-persuader
#       totals are reported in the main text rather than the cells
#       to keep the row uncluttered.
#   * \textbf{Advance Notice:} calendar-days definition with the
#       unpaid-own-time prep clarification.
#   * \textbf{Paid Preparation:} hours-of-paid-research definition.
#   * \textbf{Live Practice:} active-conversation-time scope plus
#       Selected Laypeople's selection-tournament-counts-as-practice
#       caveat and Coached Elite Debaters' Study~1 + AI-practice
#       composition (~20 + ~5 = ~25 convos).
#   * \textbf{Coached Elite Debaters} class-level note (END OF
#       CAPTION): identity (= Elite Debaters returning for Study~2
#       with ~8h of additional coaching) plus cumulative-through-
#       Study~2 interpretation of their Advance Notice and Paid
#       Preparation cells. Methodological granularity (e.g., that
#       the Study~2 coaching was delivered in two 4-hour blocks) is
#       deferred to \textit{Methods}.
#   * \textit{Methods} pointer at the end.
#
# Five %s slots, in order:
#   1-5: N Persuaders     for RL, SL, Canv (slash), ED, CED
#   6-10: N Conversations for RL, SL, Canv (slash), ED, CED
template <- '\\begin{table}[!htbp]
\\centering
\\small
\\caption{\\textbf{Incentives, compensation, preparation, and sample sizes across the five human-persuader conditions.} Professional Canvassers contributed to both Study~3 (attitude) and Study~4 (donation); slash-separated values in their column give Study~3~/~Study~4 counts. \\cmark\\ = yes; \\xmark\\ = no. \\textbf{Top-Performer Bonus:} performance prize awarded to the top-ranked persuaders in each class -- the four highest-ranked received \\pounds1{,}000~/~\\pounds750~/~\\pounds500~/~\\pounds250 (1st--4th); for Random Laypeople, the top 10\\%% each received \\pounds10. \\textbf{Per-conv Bonus (vs.\\ AI):} per-conversation bonus computed as \\pounds15 $\\times$ persuader effectiveness relative to AI; parenthetical values are class-mean realized per-conversation bonuses (per-persuader totals in the main text). \\textbf{Advance Notice:} calendar days between briefing and session start; persuaders could prepare unpaid on their own time. \\textbf{Paid Preparation:} self-reported hours of paid issue research. \\textbf{Live Practice:} hours of active conversation time; for Selected Laypeople, selection-tournament conversations count as live practice; for Coached Elite Debaters, includes Study~1 conversations ($\\sim$20) plus $\\sim$5 AI-practice conversations. \\textbf{Coached Elite Debaters} are the same persuaders as Elite Debaters, returning for Study~2 with $\\sim$8 hours of additional coaching; their Advance Notice and Paid Preparation cells are cumulative across both studies. See \\textit{Methods} for full details.}
\\label{tab:human-conditions}

\\renewcommand{\\arraystretch}{1.35}
\\setlength{\\tabcolsep}{4pt}

\\begin{tabular}{@{} l @{\\hspace{6pt}} c c c c c @{}}
\\toprule
& \\shortstack{\\textbf{Random}\\\\\\textbf{Laypeople}}
& \\shortstack{\\textbf{Selected}\\\\\\textbf{Laypeople}}
& \\shortstack{\\textbf{Professional}\\\\\\textbf{Canvassers}}
& \\shortstack{\\textbf{Elite}\\\\\\textbf{Debaters}}
& \\shortstack{\\textbf{Coached Elite}\\\\\\textbf{Debaters}} \\\\
\\midrule

\\rowcolor{gray!6}
\\textbf{Study} & 1 & 1 & 3 / 4 & 1 & 2 \\\\

\\textbf{N Persuaders} & %s & %s & %s & %s & %s \\\\

\\rowcolor{gray!6}
\\textbf{N Conversations} & %s & %s & %s & %s & %s \\\\

\\midrule

\\textbf{Base Salary} & \\pounds12/hr & \\pounds24/hr & \\pounds140/hr & \\pounds30/hr & \\pounds30/hr \\\\

\\rowcolor{gray!6}
& \\pounds10 & \\pounds1k--\\pounds250 & \\pounds1k--\\pounds250 & \\pounds1k--\\pounds250 & \\pounds1k--\\pounds250 \\\\
\\rowcolor{gray!6}
\\multirow{-2}{*}{\\textbf{Top-Performer Bonus}} & (top 10\\%%) & (1st--4th) & (1st--4th) & (1st--4th) & (1st--4th) \\\\

\\multirow{2}{*}{\\textbf{Per-conv Bonus (vs.\\ AI)}} & \\multirow{2}{*}{\\xmark} & \\pounds15 $\\times$ rel.\\ eff. & \\multirow{2}{*}{\\xmark} & \\pounds15 $\\times$ rel.\\ eff. & \\pounds15 $\\times$ rel.\\ eff. \\\\
& & ($\\sim$\\pounds8/conv) & & ($\\sim$\\pounds8.50/conv) & ($\\sim$\\pounds10/conv) \\\\

\\midrule

\\rowcolor{gray!6}
\\textbf{Advance Notice} & \\xmark & 4 days & 7 days & 21 days & $\\sim$6 weeks \\\\

\\textbf{Paid Preparation} & \\xmark & \\xmark & \\xmark & $\\sim$8 hrs & $\\sim$16 hrs \\\\

\\rowcolor{gray!6}
& & 3 hrs & & 1.5 hrs & 7 hrs \\\\
\\rowcolor{gray!6}
\\multirow{-2}{*}{\\textbf{Live Practice}} & \\multirow{-2}{*}{\\xmark} & (13 convos) & \\multirow{-2}{*}{\\xmark} & ($\\sim$5 convos) & ($\\sim$25 convos) \\\\

\\textbf{Issue Selection} & \\xmark & \\xmark & \\xmark & \\cmark & \\cmark \\\\

\\rowcolor{gray!6}
\\textbf{Coaching} & \\xmark & \\xmark & \\xmark & \\xmark & \\cmark \\\\

\\bottomrule
\\end{tabular}

\\end{table}
'

tex <- sprintf(
  template,
  # N Persuaders row
  fmt_n(rl$n_persuaders),  fmt_n(sl$n_persuaders),  canv_persuaders,
  fmt_n(ed$n_persuaders),  fmt_n(ced$n_persuaders),
  # N Conversations row
  fmt_n(rl$n_conversations), fmt_n(sl$n_conversations), canv_conversations,
  fmt_n(ed$n_conversations), fmt_n(ced$n_conversations)
)

# =============================================================================
# 3. Write
# =============================================================================
if (!dir.exists(TABLES_MAIN)) {
  dir.create(TABLES_MAIN, recursive = TRUE, showWarnings = FALSE)
}
path <- file.path(TABLES_MAIN, "tab1_human_conditions.tex")
writeLines(tex, path)
message(sprintf("  wrote %s", path))
message("  done.")
