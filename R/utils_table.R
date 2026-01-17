# Saqrmisc Package: Table Formatting Utilities
#
# General-purpose table conversion functions that can be applied to any table.

#' @importFrom stats setNames
NULL

# =============================================================================
# EXPORTED GENERAL-PURPOSE CONVERSION FUNCTIONS
# =============================================================================

#' Convert Table to Markdown Format
#'
#' @description
#' Converts a data frame or gt table to markdown format. Works with any
#' tabular data structure.
#'
#' @param x A data frame, matrix, or gt table to convert.
#' @param title Optional title for the table.
#' @param subtitle Optional subtitle for the table.
#' @param digits Number of decimal places for numeric columns. Default 2.
#' @param align Character vector of column alignments ("l", "c", "r") or
#'   single character to apply to all columns. Default "l" (left).
#' @param caption_style Style for title: "header" (## Title), "bold" (**Title**),
#'   or "plain". Default "header".
#'
#' @return A character string containing the markdown table with class
#'   "markdown_table" for proper printing.
#'
#' @examples
#' \dontrun{
#' # Convert a data frame to markdown
#' df <- data.frame(
#'   Name = c("Alice", "Bob", "Carol"),
#'   Score = c(85.5, 92.3, 78.9),
#'   Grade = c("B", "A", "C")
#' )
#' to_markdown(df)
#' to_markdown(df, title = "Student Scores", digits = 1)
#'
#' # Works with gt tables too
#' library(gt)
#' gt_table <- gt(df)
#' to_markdown(gt_table)
#' }
#'
#' @export
to_markdown <- function(x,
                        title = NULL,
                        subtitle = NULL,
                        digits = 2,
                        align = "l",
                        caption_style = c("header", "bold", "plain")) {


  caption_style <- match.arg(caption_style)


  # Convert input to data frame
 df <- .to_dataframe(x, digits = digits)

  # Build alignment row
  n_cols <- ncol(df)
  if (length(align) == 1) {
    align <- rep(align, n_cols)
  }
  align_markers <- sapply(align, function(a) {
    switch(a,
           "l" = ":---",
           "c" = ":---:",
           "r" = "---:",
           "---")
  })

  # Build markdown table
  header <- paste("|", paste(names(df), collapse = " | "), "|")
  separator <- paste("|", paste(align_markers, collapse = " | "), "|")

  rows <- apply(df, 1, function(row) {
    # Escape pipe characters in content
    row <- gsub("\\|", "\\\\|", as.character(row))
    paste("|", paste(row, collapse = " | "), "|")
  })

  # Assemble parts
  md_parts <- c()

  if (!is.null(title)) {
    if (caption_style == "header") {
      md_parts <- c(md_parts, paste0("## ", title))
    } else if (caption_style == "bold") {
      md_parts <- c(md_parts, paste0("**", title, "**"))
    } else {
      md_parts <- c(md_parts, title)
    }
    if (!is.null(subtitle)) {
      md_parts <- c(md_parts, paste0("*", subtitle, "*"))
    }
    md_parts <- c(md_parts, "")
  }

  md_parts <- c(md_parts, header, separator, rows)
  md_table <- paste(md_parts, collapse = "\n")

  class(md_table) <- c("markdown_table", "character")
  return(md_table)
}


#' Convert Table to LaTeX Format
#'
#' @description
#' Converts a data frame or gt table to LaTeX tabular format. Works with any
#' tabular data structure.
#'
#' @param x A data frame, matrix, or gt table to convert.
#' @param title Optional title (added as caption).
#' @param label Optional LaTeX label for cross-referencing.
#' @param digits Number of decimal places for numeric columns. Default 2.
#' @param align Character vector of column alignments ("l", "c", "r") or
#'   single character to apply to all columns. Default "l" (left).
#' @param booktabs Logical. Use booktabs style (toprule, midrule, bottomrule)?
#'   Default TRUE for cleaner tables.
#' @param float Logical. Wrap in table environment with caption? Default TRUE
#'   if title is provided.
#' @param position Table float position (e.g., "htbp"). Default "htbp".
#'
#' @return A character string containing the LaTeX code with class
#'   "latex_table" for proper printing.
#'
#' @examples
#' \dontrun{
#' # Convert a data frame to LaTeX
#' df <- data.frame(
#'   Variable = c("Age", "Score", "Income"),
#'   Mean = c(35.2, 78.5, 52000),
#'   SD = c(10.1, 12.3, 15000)
#' )
#' to_latex(df)
#' to_latex(df, title = "Descriptive Statistics", booktabs = TRUE)
#'
#' # With custom alignment
#' to_latex(df, align = c("l", "r", "r"))
#' }
#'
#' @export
to_latex <- function(x,
                     title = NULL,
                     label = NULL,
                     digits = 2,
                     align = "l",
                     booktabs = TRUE,
                     float = NULL,
                     position = "htbp") {

  # Convert input to data frame
  df <- .to_dataframe(x, digits = digits)

  # Set float default
  if (is.null(float)) {
    float <- !is.null(title)
  }

  # Build alignment string
  n_cols <- ncol(df)
  if (length(align) == 1) {
    align <- rep(align, n_cols)
  }
  col_align <- paste(align, collapse = "")

  # Escape special LaTeX characters in content
  escape_latex <- function(text) {
    text <- gsub("\\\\", "\\\\textbackslash{}", text)
    text <- gsub("&", "\\\\&", text)
    text <- gsub("%", "\\\\%", text)
    text <- gsub("\\$", "\\\\$", text)
    text <- gsub("#", "\\\\#", text)
    text <- gsub("_", "\\\\_", text)
    text <- gsub("\\{", "\\\\{", text)
    text <- gsub("\\}", "\\\\}", text)
    text <- gsub("~", "\\\\textasciitilde{}", text)
    text <- gsub("\\^", "\\\\textasciicircum{}", text)
    text
  }

  # Escape column names and content
  col_names <- sapply(names(df), escape_latex)
  header <- paste(col_names, collapse = " & ")

  rows <- apply(df, 1, function(row) {
    escaped_row <- sapply(as.character(row), escape_latex)
    paste(escaped_row, collapse = " & ")
  })

  # Build LaTeX table
  latex_parts <- c()

  if (float) {
    latex_parts <- c(latex_parts,
                     paste0("\\begin{table}[", position, "]"),
                     "\\centering")
    if (!is.null(title)) {
      latex_parts <- c(latex_parts, paste0("\\caption{", escape_latex(title), "}"))
    }
    if (!is.null(label)) {
      latex_parts <- c(latex_parts, paste0("\\label{", label, "}"))
    }
  }

  latex_parts <- c(latex_parts, paste0("\\begin{tabular}{", col_align, "}"))

  if (booktabs) {
    latex_parts <- c(latex_parts,
                     "\\toprule",
                     paste0(header, " \\\\"),
                     "\\midrule",
                     paste0(rows, " \\\\"),
                     "\\bottomrule")
  } else {
    latex_parts <- c(latex_parts,
                     "\\hline",
                     paste0(header, " \\\\"),
                     "\\hline",
                     paste0(rows, " \\\\"),
                     "\\hline")
  }

  latex_parts <- c(latex_parts, "\\end{tabular}")

  if (float) {
    latex_parts <- c(latex_parts, "\\end{table}")
  }

  latex_table <- paste(latex_parts, collapse = "\n")
  class(latex_table) <- c("latex_table", "character")
  return(latex_table)
}


#' Convert Table to HTML Format
#'
#' @description
#' Converts a data frame or gt table to HTML table format. Works with any
#' tabular data structure.
#'
#' @param x A data frame, matrix, or gt table to convert.
#' @param title Optional title (added as caption).
#' @param digits Number of decimal places for numeric columns. Default 2.
#' @param class CSS class(es) for the table element. Default "table".
#' @param id Optional HTML id attribute for the table.
#' @param border Table border width. Default 1.
#' @param style Additional inline CSS style for the table.
#' @param header_style CSS style for header cells. Default bold.
#' @param stripe Logical. Add striped rows? Default FALSE.
#'
#' @return A character string containing the HTML code with class
#'   "html_table" for proper printing.
#'
#' @examples
#' \dontrun{
#' # Convert a data frame to HTML
#' df <- data.frame(
#'   Name = c("Alice", "Bob"),
#'   Score = c(85, 92)
#' )
#' to_html(df)
#' to_html(df, title = "Scores", class = "table table-striped")
#' }
#'
#' @export
to_html <- function(x,
                    title = NULL,
                    digits = 2,
                    class = "table",
                    id = NULL,
                    border = 1,
                    style = NULL,
                    header_style = "font-weight: bold;",
                    stripe = FALSE) {

  # Convert input to data frame
  df <- .to_dataframe(x, digits = digits)

  # HTML escape function
  escape_html <- function(text) {
    text <- gsub("&", "&amp;", text)
    text <- gsub("<", "&lt;", text)
    text <- gsub(">", "&gt;", text)
    text <- gsub("\"", "&quot;", text)
    text <- gsub("'", "&#39;", text)
    text
  }

  # Build table attributes
  attrs <- c(paste0('class="', class, '"'))
  if (!is.null(id)) attrs <- c(attrs, paste0('id="', id, '"'))
  if (border > 0) attrs <- c(attrs, paste0('border="', border, '"'))
  if (!is.null(style)) attrs <- c(attrs, paste0('style="', style, '"'))

  # Build header row
  header_cells <- sapply(names(df), function(col) {
    if (!is.null(header_style)) {
      paste0('<th style="', header_style, '">', escape_html(col), '</th>')
    } else {
      paste0('<th>', escape_html(col), '</th>')
    }
  })
  header_row <- paste0('  <tr>', paste(header_cells, collapse = ''), '</tr>')

  # Build data rows
  data_rows <- sapply(seq_len(nrow(df)), function(i) {
    row_style <- if (stripe && i %% 2 == 0) ' style="background-color: #f9f9f9;"' else ''
    cells <- sapply(as.character(df[i, ]), function(cell) {
      paste0('<td>', escape_html(cell), '</td>')
    })
    paste0('  <tr', row_style, '>', paste(cells, collapse = ''), '</tr>')
  })

  # Assemble HTML
  html_parts <- c()

  html_parts <- c(html_parts, paste0('<table ', paste(attrs, collapse = ' '), '>'))

  if (!is.null(title)) {
    html_parts <- c(html_parts, paste0('  <caption>', escape_html(title), '</caption>'))
  }

  html_parts <- c(html_parts,
                  '  <thead>',
                  header_row,
                  '  </thead>',
                  '  <tbody>',
                  data_rows,
                  '  </tbody>',
                  '</table>')

  html_table <- paste(html_parts, collapse = '\n')
  class(html_table) <- c("html_table", "character")
  return(html_table)
}


#' Convert Table to Kable Format
#'
#' @description
#' Converts a data frame or gt table to knitr::kable format for use in
#' R Markdown documents.
#'
#' @param x A data frame, matrix, or gt table to convert.
#' @param format Output format: "markdown", "html", "latex", "rst", "pipe".
#'   Default "pipe" for GitHub-flavored markdown.
#' @param digits Number of decimal places for numeric columns. Default 2.
#' @param caption Optional table caption.
#' @param col.names Optional column names (overrides data frame names).
#' @param row.names Logical. Include row names? Default FALSE.
#' @param align Column alignment: "l" (left), "c" (center), "r" (right),
#'   or a vector of alignments.
#' @param ... Additional arguments passed to knitr::kable.
#'
#' @return A knitr_kable object.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(A = 1:3, B = c("x", "y", "z"))
#' to_kable(df)
#' to_kable(df, format = "html", caption = "My Table")
#' }
#'
#' @export
to_kable <- function(x,
                     format = c("pipe", "markdown", "html", "latex", "rst"),
                     digits = 2,
                     caption = NULL,
                     col.names = NULL,
                     row.names = FALSE,
                     align = NULL,
                     ...) {

  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is required for to_kable(). Install with: install.packages('knitr')")
  }

  format <- match.arg(format)

  # Convert input to data frame
  df <- .to_dataframe(x, digits = digits)

  # Use knitr::kable
  knitr::kable(df,
               format = format,
               digits = digits,
               caption = caption,
               col.names = col.names,
               row.names = row.names,
               align = align,
               ...)
}


#' Convert Any Table to Data Frame
#'
#' @description
#' Extracts the underlying data frame from various table objects including
#' gt tables, kable objects, and matrices.
#'
#' @param x A table object (gt, kable, matrix, or data frame).
#' @param digits Number of decimal places for formatting numeric columns.
#'   Set to NULL to skip formatting. Default NULL.
#'
#' @return A data frame.
#'
#' @examples
#' \dontrun{
#' # Extract data from a gt table
#' library(gt)
#' gt_table <- gt(mtcars[1:5, 1:3])
#' df <- to_dataframe(gt_table)
#'
#' # Works with matrices too
#' mat <- matrix(1:6, nrow = 2)
#' to_dataframe(mat)
#' }
#'
#' @export
to_dataframe <- function(x, digits = NULL) {
  .to_dataframe(x, digits = digits)
}


# =============================================================================
# INTERNAL HELPER FUNCTIONS
# =============================================================================

#' Internal: Convert to Data Frame
#' @noRd
.to_dataframe <- function(x, digits = 2) {
  # Already a data frame
  if (is.data.frame(x)) {
    df <- x
  }
  # Matrix
  else if (is.matrix(x)) {
    df <- as.data.frame(x)
  }
  # gt table - extract data
 else if (inherits(x, "gt_tbl")) {
    # gt stores data in _data
    df <- x[["_data"]]
    if (is.null(df)) {
      # Fallback: try to extract from the body
      df <- as.data.frame(x)
    }
  }
  # kable object - try to extract
  else if (inherits(x, "knitr_kable")) {
    # kable stores original data in attributes
    df <- attr(x, "data")
    if (is.null(df)) {
      stop("Cannot extract data frame from this kable object")
    }
  }
  # tibble
  else if (inherits(x, "tbl_df")) {
    df <- as.data.frame(x)
  }
  else {
    stop("Cannot convert object of class '", class(x)[1], "' to data frame")
  }

  # Format numeric columns
  if (!is.null(digits)) {
    for (col in names(df)) {
      if (is.numeric(df[[col]])) {
        df[[col]] <- round(df[[col]], digits)
      }
    }
  }

  return(df)
}


# =============================================================================
# PRINT METHODS
# =============================================================================

#' Print method for markdown tables
#' @param x A markdown_table object
#' @param ... Additional arguments (ignored)
#' @export
print.markdown_table <- function(x, ...) {
  cat(x, "\n")
  invisible(x)
}

#' Print method for latex tables
#' @param x A latex_table object
#' @param ... Additional arguments (ignored)
#' @export
print.latex_table <- function(x, ...) {
  cat(x, "\n")
  invisible(x)
}

#' Print method for html tables
#' @param x An html_table object
#' @param ... Additional arguments (ignored)
#' @export
print.html_table <- function(x, ...) {
  cat(x, "\n")
  invisible(x)
}


# =============================================================================
# INTERNAL FORMATTING FUNCTION (for backward compatibility)
# =============================================================================

#' Format Table Output (Internal)
#'
#' @description
#' Internal utility function used by Saqrmisc table-producing functions.
#' For general use, prefer the exported to_markdown(), to_latex(), etc.
#'
#' @param df A data frame to format.
#' @param format Character. Output format: "gt", "plain", "markdown", "latex", "kable".
#' @param title Optional title for the table.
#' @param subtitle Optional subtitle for the table.
#' @param show_header Logical. Show title/subtitle? Default TRUE.
#' @param digits Number of decimal places for numeric columns. Default 2.
#' @param footnote Optional footnote text.
#' @param source_notes Character vector of source notes (for gt tables).
#' @param bold_cols Character vector of column names to bold.
#' @param align_left Character vector of column names to left-align.
#'
#' @return Formatted output based on the specified format.
#'
#' @keywords internal
#' @noRd
format_table <- function(df,
                         format = c("gt", "plain", "markdown", "latex", "kable"),
                         title = NULL,
                         subtitle = NULL,
                         show_header = TRUE,
                         digits = 2,
                         footnote = NULL,
                         source_notes = NULL,
                         bold_cols = NULL,
                         align_left = NULL) {

  format <- match.arg(format)

  # Plain format - return data frame as-is
  if (format == "plain") {
    return(df)
  }

  # Use the new exported functions
  if (format == "markdown") {
    return(to_markdown(df,
                       title = if (show_header) title else NULL,
                       subtitle = if (show_header) subtitle else NULL,
                       digits = digits))
  }

  if (format == "latex") {
    return(to_latex(df,
                    title = if (show_header) title else NULL,
                    digits = digits))
  }

  if (format == "kable") {
    return(to_kable(df,
                    digits = digits,
                    caption = if (show_header) title else NULL))
  }

  # GT format (default)
  if (format == "gt") {
    if (!requireNamespace("gt", quietly = TRUE)) {
      warning("gt package not available, returning plain format")
      return(df)
    }

    gt_table <- gt::gt(df)

    # Align columns
    if (!is.null(align_left)) {
      gt_table <- gt_table |>
        gt::cols_align(align = "left", columns = dplyr::any_of(align_left))
    }

    # Bold columns
    if (!is.null(bold_cols)) {
      for (col in bold_cols) {
        if (col %in% names(df)) {
          gt_table <- gt_table |>
            gt::tab_style(
              style = gt::cell_text(weight = "bold"),
              locations = gt::cells_body(columns = dplyr::all_of(col))
            )
        }
      }
    }

    # Add header
    if (show_header && !is.null(title)) {
      gt_table <- gt_table |>
        gt::tab_header(
          title = title,
          subtitle = subtitle
        )
    }

    # Add source notes
    if (!is.null(source_notes)) {
      for (note in source_notes) {
        gt_table <- gt_table |>
          gt::tab_source_note(source_note = note)
      }
    }

    return(gt_table)
  }
}


#' Print formatted table (Internal)
#'
#' @description
#' Internal helper to print and return formatted table results consistently.
#'
#' @keywords internal
#' @noRd
return_formatted_table <- function(table_output,
                                   data = NULL,
                                   display = NULL,
                                   format = "gt",
                                   additional_results = list(),
                                   print_output = TRUE) {

  result <- list(table = table_output)

  if (!is.null(data)) {
    result$data <- data
  }

  if (!is.null(display)) {
    result$display <- display
  }

  result <- c(result, additional_results)

  if (print_output) {
    if (format == "plain") {
      print(if (!is.null(display)) display else data)
    } else if (format %in% c("markdown", "latex", "kable")) {
      print(table_output)
    } else if (format == "gt") {
      print(table_output)
    }
  }

  invisible(result)
}
