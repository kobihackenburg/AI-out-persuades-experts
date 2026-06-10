# table_style.R
# Unified table style + write pipeline for every SI table. As with theme.R
# for figures, every table script should call `style_table()` instead of
# repeating gt configuration locally, and then call `save_table()` (never
# gt::gtsave) so the on-disk LaTeX file is journal-ready.

if (!requireNamespace("gt", quietly = TRUE)) {
  stop("gt is required. Run `Rscript -e 'install.packages(\"gt\")'`.")
}

# style_table(): apply project-standard formatting to a gt object.
#   * compact body font; caption uses document body size (no \small etc.).
#   * light, readable padding; bold column headers with thin top/bottom rule.
#   * sentence-case column labels are the caller's responsibility (we don't
#     auto-rewrite, just style).
style_table <- function(gt_tbl, title = NULL, subtitle = NULL) {
  out <- gt_tbl |>
    gt::tab_options(
      table.font.size                   = "small",
      heading.title.font.size           = NULL,
      heading.subtitle.font.size        = NULL,
      heading.align                     = "left",
      data_row.padding                  = gt::px(3),
      column_labels.padding             = gt::px(4),
      column_labels.font.weight         = "bold",
      column_labels.border.top.style    = "solid",
      column_labels.border.bottom.style = "solid",
      table.border.top.style            = "none",
      table.border.bottom.style         = "none",
      heading.border.bottom.style       = "none",
      row.striping.include_table_body   = TRUE,
      row.striping.background_color     = "#ECECEC"
    )

  if (!is.null(title) || !is.null(subtitle)) {
    out <- out |> gt::tab_header(title = title, subtitle = subtitle)
  }

  out
}

# save_table(): render a gt object to a journal-ready LaTeX file. gt's raw
# output is unsuitable for direct \input because it uses \caption* (so the
# table doesn't auto-number) and lacks a \label, and because the title /
# subtitle are wrapped in fixed-size {\small ...} / {\scriptsize ...} groups
# that visually clash with body prose. We post-process the file so each
# generated .tex is fully self-contained:
#
#   * \caption*  -> \caption        (auto-numbers as Table S1, S2, ...)
#   * strip the {\small ...\small} / {\scriptsize ...\small} wrappers gt
#     injects inside the caption (caption inherits document body size)
#   * inject \label{<label>} immediately before \end{table} (so \ref{} works)
#
# `layout` selects an outer wrapper:
#   "auto"      - no wrapper; table floats at its natural size.
#   "shrink"    - wrap \begin{tabular*}...\end{tabular*} in
#                 \resizebox{\linewidth}{!}{...}: tables wider than the text
#                 block are auto-scaled down (font shrinks).
#   "landscape" - wrap the whole \begin{table}...\end{table} in
#                 \begin{landscape}...\end{landscape} (requires pdflscape,
#                 already loaded in SI.tex).
#   "longtable" - convert the float to a longtable env so a tall table can
#                 break across pages. Use sparingly; longtable doesn't float
#                 and ignores \begin{table}[t] placement.
save_table <- function(gt_tbl, name, dir, label = NULL,
                       layout = c("auto", "shrink", "landscape", "longtable")) {
  layout <- match.arg(layout)

  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, paste0(name, ".tex"))
  gt::gtsave(gt_tbl, path)

  txt <- readLines(path, warn = FALSE, encoding = "UTF-8")
  txt <- .inject_centering(txt)
  txt <- .center_data_columns(txt)
  txt <- .clean_gt_caption(txt)
  txt <- .decode_gt_unicode(txt)
  txt <- .bold_headers(txt)
  txt <- .stripe_rows(txt, layout)
  if (!is.null(label)) txt <- .inject_label(txt, label, path)

  txt <- switch(layout,
    auto      = txt,
    shrink    = .wrap_shrink(txt),
    landscape = .wrap_landscape(txt),
    longtable = .wrap_longtable(txt, label)
  )
  txt <- .left_align_captions(txt)

  # writeLines() on macOS will silently escape non-ASCII as <U+XXXX>
  # unless the output connection is explicitly UTF-8. Pound signs,
  # em-dashes, and similar characters in column labels need to survive
  # the round-trip into the LaTeX file.
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(txt, con, useBytes = FALSE)
  message(sprintf("  wrote %s [layout=%s]", path, layout))
  invisible(path)
}

# -- caption + label helpers -----------------------------------------------

.inject_centering <- function(txt) {
  idx <- grep("^\\\\begin\\{table\\}", txt)
  if (length(idx) == 0L) return(txt)
  append(txt, "\\centering", after = idx[1])
}

# Left-align caption text (title + subtitle).  \centering only centres the
# tabular; captionsetup overrides the caption package default (centering).
.left_align_captions <- function(txt) {
  cap_idx <- grep("^\\\\caption\\{", txt)
  if (length(cap_idx) == 0L) return(txt)
  setup <- "\\captionsetup{justification=raggedright,singlelinecheck=false}"
  for (i in rev(cap_idx)) {
    txt <- append(txt, setup, after = i - 1L)
  }
  txt
}

# Keep the first column left-aligned; centre all remaining data columns.
.center_data_columns <- function(txt) {
  idx <- grep("\\\\begin\\{tabular", txt)
  if (length(idx) == 0L) return(txt)
  line <- txt[idx[1]]
  m <- regexec("([lrc])([lrc]+)\\}\\s*$", line)
  parts <- regmatches(line, m)[[1]]
  if (length(parts) == 0L) return(txt)
  first    <- parts[2]
  rest     <- parts[3]
  new_rest <- gsub("[lr]", "c", rest)
  prefix   <- substr(line, 1L, m[[1]][1] - 1L)
  txt[idx[1]] <- paste0(prefix, first, new_rest, "}")
  txt
}

.clean_gt_caption <- function(txt) {
  blob <- paste(txt, collapse = "\n")

  # \caption*{...}  ->  \caption{...}
  blob <- sub("\\\\caption\\*\\{", "\\\\caption{", blob, perl = TRUE)

  # Strip the font-sizing groups gt wraps the heading title and subtitle
  # in. gt emits two flavours depending on its version / options: a
  # named-size form ({\small ...\small}) and an explicit-fontsize form
  # ({\fontsize{20}{25}\selectfont ...\small}). Match either flavour at
  # the start and a named size at the close; everything between is the
  # caption text we want to keep.
  size_alt  <- "tiny|scriptsize|footnotesize|small|normalsize|large|Large|LARGE|huge|Huge"
  open_alt  <- sprintf(
    "(?:\\\\(?:%s)|\\\\fontsize\\{[^}]*\\}\\{[^}]*\\}\\\\selectfont)",
    size_alt
  )
  blob <- gsub(
    sprintf("\\{%s\\s+(.*?)\\\\(?:%s)\\s*\\}", open_alt, size_alt),
    "\\1", blob, perl = TRUE
  )

  # gt 1.3.0 also emits a simpler form without a closing size command:
  # {\large \textbf{Title}} and {\small subtitle text}.  The pattern
  # above misses these because it requires a trailing \small / \large.
  blob <- gsub(
    sprintf("\\{\\\\(?:%s)\\s+((?:[^{}]|\\{[^{}]*\\})*)\\}", size_alt),
    "\\1", blob, perl = TRUE
  )

  strsplit(blob, "\n", fixed = TRUE)[[1]]
}

# Rewrite the <U+XXXX> escapes gtsave inserts for non-ASCII characters
# into LaTeX-friendly forms. gt does this regardless of the R session
# encoding, so we always need to clean them up post-write.
.decode_gt_unicode <- function(txt) {
  replacements <- c(
    "<U\\+00A3>" = "\\\\pounds{}",  # GBP
    "<U\\+00A0>" = "~",              # non-breaking space
    "<U\\+2013>" = "--",             # en-dash
    "<U\\+2014>" = "---",            # em-dash
    "<U\\+2212>" = "-",              # minus sign
    "<U\\+2018>" = "`",              # left single quote
    "<U\\+2019>" = "'",              # right single quote
    "<U\\+201C>" = "``",             # left double quote
    "<U\\+201D>" = "''",             # right double quote
    "<U\\+2026>" = "\\\\ldots{}",   # ellipsis
    "<U\\+00D7>" = "$\\\\times$"     # times
  )
  for (pat in names(replacements)) {
    txt <- gsub(pat, replacements[[pat]], txt, perl = TRUE)
  }
  txt
}

# gt emits tabular*{\linewidth}{@{\extracolsep{\fill}}...} which spreads
# columns with invisible inter-column glue.  Neither \rowcolor nor
# \cellcolor can cover that glue, so shaded rows show white gaps.
# Convert to plain tabular (no \extracolsep) so colouring is seamless.
# Tables that need full-width layout use the "shrink" wrapper, which
# applies \resizebox{\linewidth}{!}{...} around the tabular.
.convert_tabular_star <- function(txt) {
  txt <- gsub(
    "\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
    "\\\\begin{tabular}{",
    txt, perl = TRUE
  )
  txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt, perl = TRUE)
  txt
}

.inject_label <- function(txt, label, path) {
  end_idx <- grep("^\\\\end\\{table\\}\\s*$", txt)
  if (length(end_idx) == 0L) {
    warning(sprintf("save_table: no \\end{table} in %s; label not injected", path))
    return(txt)
  }
  append(txt, sprintf("\\label{%s}", label), after = end_idx[1] - 1L)
}

# gt 1.3.0's LaTeX backend ignores column_labels.font.weight; the emitted
# header row contains plain text.  Wrap each cell in \textbf{} so headers
# render bold in the compiled PDF.
.bold_headers <- function(txt) {
  top_idx <- grep("\\\\toprule", txt)
  mid_idx <- grep("\\\\midrule", txt)
  if (length(top_idx) == 0L || length(mid_idx) == 0L) return(txt)
  if (mid_idx[1] <= top_idx[1] + 1L) return(txt)

  for (i in seq.int(top_idx[1] + 1L, mid_idx[1] - 1L)) {
    line <- txt[i]
    suffix <- ""
    if (grepl("\\\\\\\\\\s*$", line)) {
      suffix <- " \\\\ "
      line <- sub("\\s*\\\\\\\\\\s*$", "", line)
    }
    cells <- strsplit(line, "&", fixed = TRUE)[[1]]
    cells <- vapply(cells, function(cell) {
      content <- trimws(cell)
      if (nchar(content) == 0L) return(cell)
      if (grepl("^\\\\multicolumn\\{1\\}\\{c\\}", content)) return(cell)
      if (!grepl("^\\\\textbf\\{", content))
        content <- sprintf("\\textbf{%s}", content)
      sprintf("\\multicolumn{1}{c}{%s}", content)
    }, character(1), USE.NAMES = FALSE)
    txt[i] <- paste0(paste(cells, collapse = " & "), suffix)
  }

  # When multi-line headers (\shortstack) are present, vertically center
  # them so single-line cells in the same row aren't bottom-aligned.
  header_lines <- seq.int(top_idx[1] + 1L, mid_idx[1] - 1L)
  header_block <- paste(txt[header_lines], collapse = " ")
  if (grepl("\\\\shortstack", header_block)) {
    for (i in header_lines) {
      txt[i] <- gsub(
        "(\\\\shortstack\\{[^}]*\\\\\\\\[^}]*\\})",
        "\\\\raisebox{-0.5\\\\height}{\\1}",
        txt[i], perl = TRUE
      )
    }
  }

  txt
}

# gt 1.3.0's LaTeX backend ignores row.striping options.  Two
# strategies are used depending on the target layout:
#
# tabular* (auto/shrink/landscape): a full-width \hrule via \noalign
#   spans the entire tabular* width including @{\extracolsep{\fill}}
#   inter-column glue, giving continuous shading.  A \vskip of equal
#   magnitude backs up so the row content is typeset on top.
#
# longtable: \cellcolor{gray!15} on every cell.  longtable strips
#   @{\extracolsep{\fill}} so there are no inter-column gaps, and
#   \cellcolor confines shading to the column area rather than
#   extending to \textwidth (which would overshoot the content).
#
# The counter resets at each row-group boundary so the first data row
# in every group is always shaded.
.stripe_rows <- function(txt, layout = "auto") {
  mid_idx <- grep("\\\\midrule", txt)
  bot_idx <- grep("\\\\bottomrule", txt)
  if (length(mid_idx) == 0L || length(bot_idx) == 0L) return(txt)

  start <- mid_idx[1] + 1L
  end   <- bot_idx[1] - 1L
  if (end < start) return(txt)

  use_cellcolor <- identical(layout, "longtable")

  shade_line <- paste0(
    "\\noalign{{\\color{gray!15}\\hrule height ",
    "\\dimexpr\\ht\\strutbox+\\dp\\strutbox\\relax depth 0pt}",
    "\\nointerlineskip\\vskip ",
    "-\\dimexpr\\ht\\strutbox+\\dp\\strutbox\\relax}"
  )

  result <- character(0)
  row_count <- 0L

  for (i in seq.int(start, end)) {
    line <- trimws(txt[i])
    if (nchar(line) == 0L) { result <- c(result, txt[i]); next }
    if (grepl("^\\\\(addlinespace|midrule|cmidrule|toprule|bottomrule)", line)) {
      result <- c(result, txt[i])
      next
    }
    if (grepl("^\\\\multicolumn", line)) {
      row_count <- 0L
      result <- c(result, txt[i])
      next
    }

    row_count <- row_count + 1L
    if (row_count %% 2L == 1L) {
      if (use_cellcolor) {
        raw <- txt[i]
        suffix <- ""
        if (grepl("\\\\\\\\\\s*$", raw)) {
          suffix <- " \\\\ "
          raw <- sub("\\s*\\\\\\\\\\s*$", "", raw)
        }
        cells <- strsplit(raw, "&", fixed = TRUE)[[1]]
        cells <- vapply(cells, function(cell) {
          content <- trimws(cell)
          sprintf("\\cellcolor{gray!15}{%s}", content)
        }, character(1), USE.NAMES = FALSE)
        result <- c(result, paste0(paste(cells, collapse = " & "), suffix))
      } else {
        result <- c(result, shade_line, txt[i])
      }
    } else {
      result <- c(result, txt[i])
    }
  }

  txt <- c(txt[seq_len(start - 1L)], result, txt[seq.int(end + 1L, length(txt))])
  txt
}

# -- layout wrappers -------------------------------------------------------

.wrap_shrink <- function(txt) {
  begin_idx <- grep("^\\\\begin\\{tabular\\*?\\}", txt)
  end_idx   <- grep("^\\\\end\\{tabular\\*?\\}",   txt)
  if (length(begin_idx) == 0L || length(end_idx) == 0L) return(txt)
  bi <- begin_idx[1]; ei <- end_idx[1]
  c(
    txt[seq_len(bi - 1L)],
    "\\resizebox{\\linewidth}{!}{%",
    txt[bi:ei],
    "}",
    txt[seq.int(ei + 1L, length(txt))]
  )
}

.wrap_landscape <- function(txt) {
  tab_begin <- grep("^\\\\begin\\{table\\}", txt)
  tab_end   <- grep("^\\\\end\\{table\\}",   txt)
  if (length(tab_begin) == 0L || length(tab_end) == 0L) return(txt)
  c(
    "\\begin{landscape}",
    txt,
    "\\end{landscape}"
  )
}

.wrap_longtable <- function(txt, label) {
  # Convert  \begin{table}[t] ... \begin{tabular*}{...}{spec} ... \end{tabular*} ... \end{table}
  # to       \begin{longtable}{spec} \caption{...}\label{...}\\ \toprule ... \endfirsthead ...
  # Minimal version: lift the tabular content into a longtable, prepending
  # the caption from the original table env.
  tab_begin <- grep("^\\\\begin\\{tabular\\*?\\}", txt)
  tab_end   <- grep("^\\\\end\\{tabular\\*?\\}",   txt)
  cap_begin <- grep("^\\\\caption\\{", txt)
  if (length(tab_begin) == 0L || length(tab_end) == 0L) return(txt)

  # Extract column spec from the tabular line, e.g.
  #   \begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lrrr}  -> lrrr
  # The naive last-{...} group regex fails because the spec is nested
  # inside an @{...} group that itself contains braces. Strip the
  # @{...} prefix first, then take the final {...} block.
  spec_line <- txt[tab_begin[1]]
  spec_line <- sub("\\{@\\{[^}]*\\{[^}]*\\}\\}", "{", spec_line, perl = TRUE)
  spec <- sub(".*\\{([a-zA-Z|\\s]+)\\}\\s*$", "\\1", spec_line, perl = TRUE)

  # Caption block may span multiple lines until a balancing close-brace.
  cap_text <- NULL
  if (length(cap_begin) > 0L) {
    # Find matching closing brace by counting depth.
    start <- cap_begin[1]
    depth <- 0L; end <- start
    for (i in seq.int(start, length(txt))) {
      line <- txt[i]
      depth <- depth + nchar(gsub("[^{]", "", line)) - nchar(gsub("[^}]", "", line))
      if (depth <= 0L) { end <- i; break }
    }
    cap_text <- paste(txt[start:end], collapse = "\n")
    cap_text <- sub("^\\\\caption\\{", "", cap_text)
    cap_text <- sub("\\}\\s*$", "", cap_text)
  }
  if (is.null(cap_text)) cap_text <- ""
  label_tex <- if (!is.null(label)) sprintf("\\label{%s}", label) else ""

  body <- txt[(tab_begin[1] + 1L):(tab_end[1] - 1L)]
  c(
    sprintf("\\begin{longtable}{%s}", spec),
    sprintf("\\caption{%s}%s\\\\", cap_text, label_tex),
    body,
    "\\end{longtable}"
  )
}
