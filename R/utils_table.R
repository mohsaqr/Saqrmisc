# Saqrmisc Package: Table Formatting Utilities
#
# Shared utility functions for table output formatting across all Saqrmisc functions.

#' Format Table Output
#'
#' @description
#' Internal utility function to format data frames into various output formats.
#' Used by descriptive_table, categorical_table, correlations, correlation_matrix,
#' compare_groups, and other table-producing functions.
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

  # =========================================================================
  # Plain format - return data frame as-is
  # =========================================================================
  if (format == "plain") {
    return(df)
  }

  # =========================================================================
  # Markdown format
  # =========================================================================
  if (format == "markdown") {
    # Build markdown table
    header <- paste("|", paste(names(df), collapse = " | "), "|")
    separator <- paste("|", paste(rep("---", ncol(df)), collapse = " | "), "|")

    rows <- apply(df, 1, function(row) {
      paste("|", paste(row, collapse = " | "), "|")
    })

    if (show_header && !is.null(title)) {
      md_parts <- c(
        paste0("## ", title),
        if (!is.null(subtitle)) paste0("*", subtitle, "*") else NULL,
        "",
        header, separator, rows
      )
      if (!is.null(footnote)) {
        md_parts <- c(md_parts, "", footnote)
      }
      md_table <- paste(md_parts, collapse = "\n")
    } else {
      md_table <- paste(c(header, separator, rows), collapse = "\n")
    }

    class(md_table) <- c("markdown_table", "character")
    return(md_table)
  }

  # =========================================================================
  # LaTeX format
  # =========================================================================
  if (format == "latex") {
    col_align <- paste(rep("l", ncol(df)), collapse = "")
    header <- paste(names(df), collapse = " & ")

    rows <- apply(df, 1, function(row) {
      paste(row, collapse = " & ")
    })

    if (show_header && !is.null(title)) {
      latex_parts <- c(
        paste0("% ", title),
        if (!is.null(subtitle)) paste0("% ", subtitle) else NULL,
        paste0("\\begin{tabular}{", col_align, "}"),
        "\\hline",
        paste0(header, " \\\\"),
        "\\hline",
        paste0(rows, " \\\\"),
        "\\hline",
        "\\end{tabular}"
      )
    } else {
      latex_parts <- c(
        paste0("\\begin{tabular}{", col_align, "}"),
        "\\hline",
        paste0(header, " \\\\"),
        "\\hline",
        paste0(rows, " \\\\"),
        "\\hline",
        "\\end{tabular}"
      )
    }

    latex_table <- paste(latex_parts, collapse = "\n")
    class(latex_table) <- c("latex_table", "character")
    return(latex_table)
  }

  # =========================================================================
  # Kable format
  # =========================================================================
  if (format == "kable") {
    if (requireNamespace("knitr", quietly = TRUE)) {
      kable_out <- knitr::kable(df, format = "markdown", digits = digits)
      if (show_header && !is.null(title)) {
        kable_out <- c(paste0("## ", title), "", kable_out)
      }
      return(kable_out)
    } else {
      warning("knitr not available, returning plain format")
      return(df)
    }
  }

  # =========================================================================
  # GT format (default)
  # =========================================================================
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


#' Print formatted table
#'
#' @description
#' Internal helper to print and return formatted table results consistently.
#'
#' @param table_output The formatted table output.
#' @param data Original data frame (for returning in result list).
#' @param display Display data frame (formatted version).
#' @param format The format used.
#' @param additional_results List of additional results to include.
#' @param print_output Logical. Print the output? Default TRUE.
#'
#' @return A list with table, data, and any additional results.
#'
#' @keywords internal
#' @noRd
return_formatted_table <- function(table_output,
                                   data = NULL,
                                   display = NULL,
                                   format = "gt",
                                   additional_results = list(),
                                   print_output = TRUE) {

  # Build result list

  result <- list(table = table_output)

  if (!is.null(data)) {
    result$data <- data
  }

  if (!is.null(display)) {
    result$display <- display
  }

  # Add any additional results
  result <- c(result, additional_results)

  # Print output
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
