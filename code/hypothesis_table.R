# hypothesis_table(): build a hypothesis summary table from a CSV.
#
# CSV layout: first column = indicators; each other column header is a
# hypothesis reference plus its short name in parentheses, e.g.
#   Indicator,@hyp-demand-number (Number),@hyp-demand-empowered (Empowered),...
#
# Output: a pandoc grid table with
#   - a spanning header over the hypothesis columns (span_label), and
#   - each hypothesis name on its own line under the hypothesis label.
# Works in both PDF and HTML, and @hyp- references are numbered by
# assets/hypotheses.lua. Use in a chunk with `results: asis`.

hypothesis_table <- function(file, span_label, widths = NULL) {
  tbl <- readr::read_csv(file, col_types = readr::cols(.default = readr::col_character())) |>
    dplyr::mutate(dplyr::across(everything(), ~ tidyr::replace_na(.x, "")))
  heads <- names(tbl)
  n <- length(heads)
  if (is.null(widths)) widths <- c(26, rep(24, n - 1))  # characters per column
  stopifnot(length(widths) == n)

  wrap <- function(x, w) {
    out <- strwrap(x, width = w)
    if (length(out) == 0) "" else out
  }
  pad <- function(x, w) paste0(x, strrep(" ", pmax(0, w - nchar(x))))

  rule <- function(ch = "-", align = FALSE) {
    segs <- vapply(seq_len(n), function(j) {
      s <- strrep(ch, widths[j] + 2)
      if (align) {
        # first column left-aligned, the rest centered
        s <- if (j == 1) paste0(":", substring(s, 2)) else paste0(":", substring(s, 2, nchar(s) - 1), ":")
      }
      s
    }, character(1))
    paste0("+", paste(segs, collapse = "+"), "+")
  }

  row_lines <- function(cells) {
    h <- max(lengths(cells))
    vapply(seq_len(h), function(i) {
      parts <- vapply(seq_len(n), function(j) {
        line <- if (i <= length(cells[[j]])) cells[[j]][i] else ""
        if (nchar(line) > widths[j]) {
          stop("'", line, "' is wider than column ", j, " (", widths[j],
               " characters). Increase `widths`.")
        }
        pad(line, widths[j])
      }, character(1))
      paste0("| ", paste(parts, collapse = " | "), " |")
    }, character(1))
  }

  # spanning header row over the hypothesis columns
  span_w <- sum(widths[-1]) + 3 * (n - 2)
  if (nchar(span_label) > span_w) stop("span_label is too long for the table width.")
  span <- paste0("| ", pad("", widths[1]), " | ", pad(span_label, span_w), " |")
  partial <- paste0("|", strrep(" ", widths[1] + 2), "+",
                    paste(strrep("-", widths[-1] + 2), collapse = "+"), "+")

  # header: "@hyp-x (Name)" becomes two lines, with a hard line break
  head_cells <- lapply(seq_len(n), function(j) {
    m <- regmatches(heads[j], regexec("^(\\S+) (\\(.*\\))$", heads[j]))[[1]]
    if (length(m) == 3) c(paste0(m[2], "\\"), m[3]) else wrap(heads[j], widths[j])
  })

  body <- unlist(lapply(seq_len(nrow(tbl)), function(r) {
    cells <- lapply(seq_len(n), function(j) wrap(tbl[[j]][r], widths[j]))
    c(row_lines(cells), rule("-"))
  }))

  cat(c("", rule("-"), span, partial, row_lines(head_cells),
        rule("=", align = TRUE), body, ""), sep = "\n")
}
