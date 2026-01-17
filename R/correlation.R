#' Create a Publication-Ready Correlation Matrix
#'
#' @description
#' Creates a formatted correlation matrix with significance stars, optional
#' confidence intervals, and heatmap visualization. Supports bivariate (zero-order),
#' partial, and semi-partial correlations.
#'
#' @param data A data frame containing the variables to correlate.
#' @param Vars Character vector of variable names to include in the correlation matrix.
#'   If NULL (default), all numeric variables in the data frame are used.
#' @param type Type of correlation: "bivariate" (default, zero-order), "partial", or "semi-partial".
#'   For partial/semi-partial, each pair is controlled for all other variables in Vars.
#' @param method Correlation method: "pearson" (default), "spearman", or "kendall".
#' @param triangle Which triangle to display: "lower" (default), "upper", or "full".
#' @param diagonal What to show on the diagonal: "dash" (default), "one", or "names".
#' @param show_n Logical. Show pairwise sample sizes? Default FALSE.
#' @param show_ci Logical. Show 95% confidence intervals? Default FALSE. Only available for bivariate Pearson.
#' @param show_p Logical. Show p-values below correlations? Default FALSE.
#' @param p_adjust Method for p-value adjustment: "none" (default), "bonferroni", "holm", "fdr".
#' @param stars Logical. Show significance stars? Default TRUE.
#' @param heatmap Logical. Create a heatmap visualization? Default FALSE.
#' @param digits Number of decimal places. Default 2.
#' @param title Optional title for the table.
#' @param use Method for handling missing data: "pairwise" (default) or "complete".
#'   Note: partial and semi-partial correlations always use complete cases.
#' @param format Character. Output format:
#'   \describe{
#'     \item{`"gt"` (default)}{Publication-ready gt table with formatting}
#'     \item{`"plain"`}{Plain data frame (the correlation matrix)}
#'     \item{`"markdown"`}{Markdown-formatted table}
#'     \item{`"latex"`}{LaTeX tabular format}
#'     \item{`"kable"`}{knitr::kable format}
#'   }
#' @param show_header Logical. Show title header? Default: `TRUE`.
#'   Set to `FALSE` to hide the table header.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{table}: A gt table with formatted correlations
#'   \item \code{correlation_matrix}: Numeric correlation matrix
#'   \item \code{p_matrix}: Matrix of p-values
#'   \item \code{n_matrix}: Matrix of pairwise sample sizes
#'   \item \code{heatmap}: ggplot heatmap (if requested)
#' }
#'
#' @examples
#' \dontrun{
#' # Basic correlation matrix
#' correlation_matrix(
#'   data = mtcars,
#'   Vars = c("mpg", "cyl", "disp", "hp")
#' )
#'
#' # Partial correlations (controlling for other variables)
#' correlation_matrix(
#'   data = mtcars,
#'   Vars = c("mpg", "cyl", "disp", "hp"),
#'   type = "partial",
#'   title = "Partial Correlations"
#' )
#'
#' # Semi-partial correlations
#' correlation_matrix(
#'   data = mtcars,
#'   Vars = c("mpg", "cyl", "disp", "hp"),
#'   type = "semi-partial"
#' )
#'
#' # With confidence intervals and heatmap
#' correlation_matrix(
#'   data = mtcars,
#'   Vars = c("mpg", "cyl", "disp", "hp"),
#'   show_ci = TRUE,
#'   heatmap = TRUE,
#'   title = "Motor Trend Car Correlations"
#' )
#'
#' # Spearman correlations with Bonferroni correction
#' correlation_matrix(
#'   data = mtcars,
#'   Vars = c("mpg", "cyl", "disp"),
#'   method = "spearman",
#'   p_adjust = "bonferroni"
#' )
#' }
#'
#' @importFrom stats cor cor.test p.adjust complete.cases qt lm residuals pt
#' @importFrom gt gt tab_header tab_source_note cols_label cols_align tab_style cell_fill cell_text cells_body
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient2 theme_minimal theme labs element_blank element_text
#' @export
correlation_matrix <- function(data,
                                Vars = NULL,
                                type = c("bivariate", "partial", "semi-partial"),
                                method = c("pearson", "spearman", "kendall"),
                                triangle = c("lower", "upper", "full"),
                                diagonal = c("dash", "one", "names"),
                                show_n = FALSE,
                                show_ci = FALSE,
                                show_p = FALSE,
                                p_adjust = c("none", "bonferroni", "holm", "fdr"),
                                stars = TRUE,
                                heatmap = FALSE,
                                digits = 2,
                                title = NULL,
                                use = c("pairwise", "complete"),
                                format = c("gt", "plain", "markdown", "latex", "kable"),
                                show_header = TRUE) {

  # Match arguments
  type <- match.arg(type)
  method <- match.arg(method)
  triangle <- match.arg(triangle)
  diagonal <- match.arg(diagonal)
  p_adjust <- match.arg(p_adjust)
  use <- match.arg(use)
  format <- match.arg(format)

  # Validate inputs
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  # If Vars is NULL, use all numeric variables
  if (is.null(Vars)) {
    Vars <- names(data)[sapply(data, is.numeric)]
    if (length(Vars) < 2) {
      stop("Data must contain at least 2 numeric variables")
    }
    cat(sprintf("Using all %d numeric variables: %s\n\n",
                length(Vars), paste(Vars, collapse = ", ")))
  }

  if (length(Vars) < 2) {
    stop("'Vars' must contain at least 2 variable names")
  }

  # Partial/semi-partial require at least 3 variables
  if (type %in% c("partial", "semi-partial") && length(Vars) < 3) {
    stop("Partial and semi-partial correlations require at least 3 variables")
  }

  # Check that all variables exist
  missing_vars <- setdiff(Vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }

  # Subset data to requested variables
  data_subset <- data[, Vars, drop = FALSE]

  # Check that all variables are numeric
  non_numeric <- names(data_subset)[!sapply(data_subset, is.numeric)]
  if (length(non_numeric) > 0) {
    stop("All variables must be numeric. Non-numeric variables: ", paste(non_numeric, collapse = ", "))
  }

  # Handle missing data
  # Partial/semi-partial always require complete cases
  if (use == "complete" || type %in% c("partial", "semi-partial")) {
    data_subset <- data_subset[complete.cases(data_subset), ]
    if (nrow(data_subset) < 3) {
      stop("Fewer than 3 complete cases available")
    }
  }

  # CI only available for bivariate Pearson
  if (show_ci && type != "bivariate") {
    warning("Confidence intervals only available for bivariate correlations. Ignoring show_ci.")
    show_ci <- FALSE
  }

  n_vars <- length(Vars)
  n_obs <- nrow(data_subset)

  # Initialize matrices
  cor_matrix <- matrix(NA, nrow = n_vars, ncol = n_vars)
  p_matrix <- matrix(NA, nrow = n_vars, ncol = n_vars)
  n_matrix <- matrix(NA, nrow = n_vars, ncol = n_vars)
  ci_lower <- matrix(NA, nrow = n_vars, ncol = n_vars)
  ci_upper <- matrix(NA, nrow = n_vars, ncol = n_vars)

  rownames(cor_matrix) <- colnames(cor_matrix) <- Vars
  rownames(p_matrix) <- colnames(p_matrix) <- Vars
  rownames(n_matrix) <- colnames(n_matrix) <- Vars
  rownames(ci_lower) <- colnames(ci_lower) <- Vars
  rownames(ci_upper) <- colnames(ci_upper) <- Vars

  # Helper function to calculate partial correlation
  calc_partial_cor <- function(x, y, z_data, method) {
    # Regress x on z and get residuals
    x_resid <- residuals(lm(x ~ ., data = z_data))
    # Regress y on z and get residuals
    y_resid <- residuals(lm(y ~ ., data = z_data))
    # Correlate residuals
    if (method == "pearson") {
      cor(x_resid, y_resid)
    } else {
      cor(x_resid, y_resid, method = method)
    }
  }

  # Helper function to calculate semi-partial correlation
  calc_semipartial_cor <- function(x, y, z_data, method) {
    # Keep x as is, regress y on z and get residuals
    y_resid <- residuals(lm(y ~ ., data = z_data))
    # Correlate x with residuals of y
    if (method == "pearson") {
      cor(x, y_resid)
    } else {
      cor(x, y_resid, method = method)
    }
  }

  # Calculate correlations
  for (i in 1:n_vars) {
    for (j in 1:n_vars) {
      if (i == j) {
        cor_matrix[i, j] <- 1
        p_matrix[i, j] <- 0
        n_matrix[i, j] <- if (type == "bivariate" && use == "pairwise") {
          sum(!is.na(data_subset[[Vars[i]]]))
        } else {
          n_obs
        }
        ci_lower[i, j] <- 1
        ci_upper[i, j] <- 1
      } else {
        if (type == "bivariate") {
          # Standard bivariate correlation
          x <- data_subset[[Vars[i]]]
          y <- data_subset[[Vars[j]]]
          complete_pairs <- complete.cases(x, y)
          n_pairs <- sum(complete_pairs)
          n_matrix[i, j] <- n_pairs

          if (n_pairs >= 3) {
            tryCatch({
              test_result <- cor.test(x, y, method = method, use = "complete.obs")
              cor_matrix[i, j] <- test_result$estimate
              p_matrix[i, j] <- test_result$p.value

              # Confidence intervals (only for Pearson)
              if (method == "pearson" && n_pairs >= 4) {
                if (!is.null(test_result$conf.int)) {
                  ci_lower[i, j] <- test_result$conf.int[1]
                  ci_upper[i, j] <- test_result$conf.int[2]
                } else {
                  # Calculate CI manually using Fisher's z transformation
                  r <- test_result$estimate
                  z <- atanh(r)
                  se <- 1 / sqrt(n_pairs - 3)
                  z_crit <- qnorm(0.975)
                  ci_lower[i, j] <- tanh(z - z_crit * se)
                  ci_upper[i, j] <- tanh(z + z_crit * se)
                }
              }
            }, error = function(e) {
              warning("Could not calculate correlation for ", Vars[i], " and ", Vars[j], ": ", e$message)
            })
          }
        } else {
          # Partial or semi-partial correlation
          n_matrix[i, j] <- n_obs

          # Get control variables (all other variables except i and j)
          control_vars <- setdiff(Vars, c(Vars[i], Vars[j]))
          z_data <- data_subset[, control_vars, drop = FALSE]

          x <- data_subset[[Vars[i]]]
          y <- data_subset[[Vars[j]]]

          tryCatch({
            if (type == "partial") {
              r <- calc_partial_cor(x, y, z_data, method)
            } else {
              # semi-partial
              r <- calc_semipartial_cor(x, y, z_data, method)
            }
            cor_matrix[i, j] <- r

            # Calculate p-value for partial/semi-partial correlation
            # df = n - k - 2, where k is number of control variables
            k <- length(control_vars)
            df <- n_obs - k - 2

            if (df > 0 && !is.na(r) && abs(r) < 1) {
              t_stat <- r * sqrt(df / (1 - r^2))
              p_matrix[i, j] <- 2 * pt(-abs(t_stat), df = df)
            } else {
              p_matrix[i, j] <- NA
            }
          }, error = function(e) {
            warning("Could not calculate ", type, " correlation for ", Vars[i], " and ", Vars[j], ": ", e$message)
          })
        }
      }
    }
  }

  # Adjust p-values if requested
  if (p_adjust != "none") {
    # Get lower triangle p-values (excluding diagonal)
    lower_tri_indices <- which(lower.tri(p_matrix), arr.ind = TRUE)
    p_values_lower <- p_matrix[lower.tri(p_matrix)]

    # Adjust
    p_adjusted <- p.adjust(p_values_lower, method = p_adjust)

    # Put back into matrix (symmetric)
    for (k in seq_along(p_adjusted)) {
      i <- lower_tri_indices[k, 1]
      j <- lower_tri_indices[k, 2]
      p_matrix[i, j] <- p_adjusted[k]
      p_matrix[j, i] <- p_adjusted[k]
    }
  }

  # Helper function to format correlation with stars
  format_cor <- function(r, p, ci_l = NULL, ci_u = NULL, n = NULL) {
    if (is.na(r)) return("")

    # Format correlation
    r_fmt <- formatC(r, format = "f", digits = digits)
    # Remove leading zero
    r_fmt <- sub("^0\\.", ".", r_fmt)
    r_fmt <- sub("^-0\\.", "-.", r_fmt)

    # Add stars
    star_str <- ""
    if (stars && !is.na(p)) {
      if (p < 0.001) star_str <- "***"
      else if (p < 0.01) star_str <- "**"
      else if (p < 0.05) star_str <- "*"
    }

    result <- paste0(r_fmt, star_str)

    # Add CI if requested
    if (show_ci && !is.na(ci_l) && !is.na(ci_u)) {
      ci_l_fmt <- formatC(ci_l, format = "f", digits = digits)
      ci_u_fmt <- formatC(ci_u, format = "f", digits = digits)
      ci_l_fmt <- sub("^0\\.", ".", ci_l_fmt)
      ci_l_fmt <- sub("^-0\\.", "-.", ci_l_fmt)
      ci_u_fmt <- sub("^0\\.", ".", ci_u_fmt)
      ci_u_fmt <- sub("^-0\\.", "-.", ci_u_fmt)
      result <- paste0(result, " [", ci_l_fmt, ", ", ci_u_fmt, "]")
    }

    # Add p-value if requested
    if (show_p && !is.na(p)) {
      if (p < 0.001) {
        p_fmt <- "p < .001"
      } else {
        p_fmt <- paste0("p = ", sub("^0\\.", ".", formatC(p, format = "f", digits = 3)))
      }
      result <- paste0(result, "\n", p_fmt)
    }

    # Add n if requested
    if (show_n && !is.na(n)) {
      result <- paste0(result, "\n(n = ", n, ")")
    }

    return(result)
  }

  # Build display matrix
  display_matrix <- matrix("", nrow = n_vars, ncol = n_vars)
  rownames(display_matrix) <- Vars
  colnames(display_matrix) <- Vars

  for (i in 1:n_vars) {
    for (j in 1:n_vars) {
      if (i == j) {
        # Diagonal
        display_matrix[i, j] <- switch(diagonal,
                                        "dash" = "—",
                                        "one" = "1",
                                        "names" = Vars[i])
      } else {
        # Check triangle
        show_cell <- switch(triangle,
                            "lower" = i > j,
                            "upper" = i < j,
                            "full" = TRUE)

        if (show_cell) {
          display_matrix[i, j] <- format_cor(
            cor_matrix[i, j],
            p_matrix[i, j],
            ci_lower[i, j],
            ci_upper[i, j],
            n_matrix[i, j]
          )
        }
      }
    }
  }

  # Convert to data frame for gt
  display_df <- as.data.frame(display_matrix)
  display_df <- cbind(Variable = rownames(display_df), display_df)
  rownames(display_df) <- NULL

  # Build default title
  type_label <- switch(type,
                       "bivariate" = "",
                       "partial" = "Partial ",
                       "semi-partial" = "Semi-partial ")
  default_title <- paste0(type_label, "Correlation Matrix")
  final_title <- if (!is.null(title)) title else default_title

  # ===========================================================================
  # Return Non-GT Formats
  # ===========================================================================
  if (format %in% c("plain", "markdown", "latex", "kable")) {
    # Build base result
    base_result <- list(
      correlation_matrix = cor_matrix,
      p_matrix = p_matrix,
      n_matrix = n_matrix,
      type = type,
      method = method
    )

    if (show_ci && method == "pearson") {
      base_result$ci_lower <- ci_lower
      base_result$ci_upper <- ci_upper
    }

    # Plain format - return data frame
    if (format == "plain") {
      base_result$display = display_df
      print(display_df)
      return(invisible(base_result))
    }

    # Markdown format
    if (format == "markdown") {
      header <- paste("|", paste(names(display_df), collapse = " | "), "|")
      separator <- paste("|", paste(rep("---", ncol(display_df)), collapse = " | "), "|")

      rows <- apply(display_df, 1, function(row) {
        paste("|", paste(row, collapse = " | "), "|")
      })

      if (show_header) {
        md_table <- paste(c(
          paste0("## ", final_title),
          "",
          header, separator, rows,
          "",
          if (stars) "\\* p < .05, \\*\\* p < .01, \\*\\*\\* p < .001" else NULL
        ), collapse = "\n")
      } else {
        md_table <- paste(c(header, separator, rows), collapse = "\n")
      }

      class(md_table) <- c("markdown_table", "character")
      base_result$table <- md_table
      base_result$display <- display_df
      print(md_table)
      return(invisible(base_result))
    }

    # LaTeX format
    if (format == "latex") {
      col_align <- paste(rep("l", ncol(display_df)), collapse = "")
      header <- paste(names(display_df), collapse = " & ")

      rows <- apply(display_df, 1, function(row) {
        paste(row, collapse = " & ")
      })

      if (show_header) {
        latex_table <- paste(c(
          paste0("% ", final_title),
          paste0("\\begin{tabular}{", col_align, "}"),
          "\\hline",
          paste0(header, " \\\\"),
          "\\hline",
          paste0(rows, " \\\\"),
          "\\hline",
          "\\end{tabular}"
        ), collapse = "\n")
      } else {
        latex_table <- paste(c(
          paste0("\\begin{tabular}{", col_align, "}"),
          "\\hline",
          paste0(header, " \\\\"),
          "\\hline",
          paste0(rows, " \\\\"),
          "\\hline",
          "\\end{tabular}"
        ), collapse = "\n")
      }

      class(latex_table) <- c("latex_table", "character")
      base_result$table <- latex_table
      base_result$display <- display_df
      print(latex_table)
      return(invisible(base_result))
    }

    # Kable format
    if (format == "kable") {
      if (requireNamespace("knitr", quietly = TRUE)) {
        kable_out <- knitr::kable(display_df, format = "markdown", digits = digits)
        if (show_header) {
          kable_out <- c(paste0("## ", final_title), "", kable_out)
        }
        base_result$table <- kable_out
        base_result$display <- display_df
        print(kable_out)
        return(invisible(base_result))
      } else {
        warning("knitr not available, returning plain format")
        base_result$display <- display_df
        print(display_df)
        return(invisible(base_result))
      }
    }
  }

  # ===========================================================================
  # Create gt table
  # ===========================================================================
  gt_table <- gt::gt(display_df) |>
    gt::cols_align(align = "center", columns = -1) |>
    gt::cols_align(align = "left", columns = 1) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = 1)
    )

  # Add title if provided (respecting show_header)
  if (show_header && !is.null(final_title)) {
    gt_table <- gt_table |>
      gt::tab_header(title = final_title)
  }

  # Add footnote for stars
  if (stars) {
    footnote <- paste0("* p < .05, ** p < .01, *** p < .001",
                       if (p_adjust != "none") paste0(" (", p_adjust, " adjusted)") else "")
    gt_table <- gt_table |>
      gt::tab_source_note(source_note = footnote)
  }

  # Add method note
  method_note <- paste0(type_label, "Correlation method: ",
                        switch(method,
                               "pearson" = "Pearson",
                               "spearman" = "Spearman",
                               "kendall" = "Kendall"))
  if (type != "bivariate") {
    method_note <- paste0(method_note, " (controlling for other variables)")
  }
  gt_table <- gt_table |>
    gt::tab_source_note(source_note = method_note)

  # Apply heatmap coloring if requested
  if (heatmap) {
    # Add background colors based on correlation values
    for (i in 1:n_vars) {
      for (j in 1:n_vars) {
        if (i != j) {
          show_cell <- switch(triangle,
                              "lower" = i > j,
                              "upper" = i < j,
                              "full" = TRUE)
          if (show_cell && !is.na(cor_matrix[i, j])) {
            r <- cor_matrix[i, j]
            # Color scale: blue (positive) to white (zero) to red (negative)
            if (r > 0) {
              intensity <- min(abs(r), 1)
              color <- rgb(1 - intensity * 0.6, 1 - intensity * 0.4, 1)
            } else {
              intensity <- min(abs(r), 1)
              color <- rgb(1, 1 - intensity * 0.4, 1 - intensity * 0.6)
            }

            gt_table <- gt_table |>
              gt::tab_style(
                style = gt::cell_fill(color = color),
                locations = gt::cells_body(
                  columns = j + 1,  # +1 for Variable column
                  rows = i
                )
              )
          }
        }
      }
    }
  }

  # Create heatmap plot if requested
  heatmap_plot <- NULL
  if (heatmap) {
    # Prepare data for ggplot
    plot_data <- expand.grid(Var1 = Vars, Var2 = Vars)
    plot_data$correlation <- as.vector(cor_matrix)
    plot_data$p_value <- as.vector(p_matrix)

    # Filter based on triangle
    if (triangle == "lower") {
      plot_data$correlation[upper.tri(cor_matrix)] <- NA
    } else if (triangle == "upper") {
      plot_data$correlation[lower.tri(cor_matrix)] <- NA
    }

    # Format correlation labels
    plot_data$label <- ifelse(is.na(plot_data$correlation), "",
                              formatC(plot_data$correlation, format = "f", digits = digits))
    plot_data$label <- sub("^0\\.", ".", plot_data$label)
    plot_data$label <- sub("^-0\\.", "-.", plot_data$label)

    # Add stars
    if (stars) {
      plot_data$label <- ifelse(
        is.na(plot_data$p_value), plot_data$label,
        paste0(plot_data$label,
               ifelse(plot_data$p_value < 0.001, "***",
                      ifelse(plot_data$p_value < 0.01, "**",
                             ifelse(plot_data$p_value < 0.05, "*", ""))))
      )
    }

    # Reverse factor levels for proper display
    plot_data$Var1 <- factor(plot_data$Var1, levels = rev(Vars))
    plot_data$Var2 <- factor(plot_data$Var2, levels = Vars)

    heatmap_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Var2, y = Var1, fill = correlation)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.5) +
      ggplot2::geom_text(ggplot2::aes(label = label), size = 3.5, color = "black") +
      ggplot2::scale_fill_gradient2(
        low = "#E74C3C",
        mid = "white",
        high = "#3498DB",
        midpoint = 0,
        limits = c(-1, 1),
        na.value = "grey95",
        name = "r"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        legend.position = "right"
      ) +
      ggplot2::labs(title = if (!is.null(title)) title else "Correlation Matrix")
  }

  # Print the table
  print(gt_table)

  # Return results
  result <- list(
    table = gt_table,
    correlation_matrix = cor_matrix,
    p_matrix = p_matrix,
    n_matrix = n_matrix,
    type = type,
    method = method
  )

  if (show_ci && method == "pearson") {
    result$ci_lower <- ci_lower
    result$ci_upper <- ci_upper
  }

  if (heatmap) {
    result$heatmap <- heatmap_plot
  }

  invisible(result)
}


#' Full Pairwise Correlation Table
#'
#' @description
#' Creates a comprehensive long-format correlation table showing all pairwise
#' correlations with full statistics including r, confidence intervals, test
#' statistics, degrees of freedom, p-values, and sample sizes. Supports
#' multilevel/repeated measures correlations for nested data.
#'
#' @param data A data frame containing the variables to correlate.
#' @param Vars Character vector of variable names to include. If NULL (default),
#'   all numeric variables in the data frame are used.
#' @param type Type of correlation: "bivariate" (default), "partial", or "semi-partial".
#' @param method Correlation method: "pearson" (default), "spearman", or "kendall".
#' @param p_adjust Method for p-value adjustment: "none" (default), "bonferroni", "holm", "fdr".
#' @param ci_level Confidence level for intervals. Default 0.95.
#' @param min_r Numeric. Only show correlations with |r| >= this value. Default NULL (show all).
#' @param sig_only Logical. Only show significant correlations (p < .05)? Default FALSE.
#' @param multilevel Logical. Calculate multilevel (within-cluster) correlations? Default FALSE.
#'   When TRUE, removes between-cluster variance to estimate within-cluster associations.
#' @param id Character. Name of the clustering/ID variable for multilevel correlations.
#'   Required when multilevel = TRUE.
#' @param between Logical. Also report between-cluster correlations? Default FALSE.
#'   Only used when multilevel = TRUE.
#' @param group_by Character. Name of grouping variable to compute correlations separately per group.
#'   Results are combined into one table with a "Group" column.
#' @param include Character vector. Statistics to include in the table.
#'   Options: "r", "ci", "stat" (t/S/z), "df", "p", "n", "sig".
#'   Default NULL includes all. Use this OR exclude, not both.
#' @param exclude Character vector. Statistics to exclude from the table.
#'   Options: "r", "ci", "stat" (t/S/z), "df", "p", "n", "sig".
#'   Default NULL excludes none. Use this OR include, not both.
#' @param auto_consolidate Logical. If TRUE (default), when df or n are constant
#'   across all pairs, they are moved to the subtitle instead of shown as columns.
#' @param digits Number of decimal places. Default 3.
#' @param title Optional title for the table.
#' @param format Character. Output format:
#'   \describe{
#'     \item{`"gt"` (default)}{Publication-ready gt table with formatting}
#'     \item{`"plain"`}{Plain data frame}
#'     \item{`"markdown"`}{Markdown-formatted table}
#'     \item{`"latex"`}{LaTeX tabular format}
#'     \item{`"kable"`}{knitr::kable format}
#'   }
#' @param show_header Logical. Show title/subtitle header? Default: `TRUE`.
#'   Set to `FALSE` to hide the table header.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{table}: A gt table with full correlation statistics
#'   \item \code{data}: Data frame with all statistics
#'   \item \code{display}: Data frame as displayed (after include/exclude filtering)
#'   \item \code{n_pairs}: Number of variable pairs
#'   \item \code{n_significant}: Number of significant correlations
#'   \item \code{consolidated}: List of values moved to subtitle (df, n if constant)
#'   \item \code{between_data}: Between-cluster correlations (if multilevel & between = TRUE)
#' }
#'
#' @details
#' When \code{multilevel = TRUE}, the function calculates within-cluster correlations
#' by group-mean centering variables before computing correlations. This removes
#' between-cluster variance and estimates the pooled within-cluster association,
#' appropriate for repeated measures or hierarchically nested data.
#'
#' The degrees of freedom for multilevel correlations are adjusted as:
#' df = n_observations - n_clusters - 1
#'
#' @examples
#' \dontrun{
#' # Correlate all numeric variables in data frame
#' correlations(mtcars)
#'
#' # Full correlation table with specific variables
#' correlations(mtcars, Vars = c("mpg", "cyl", "disp", "hp", "wt"))
#'
#' # Only strong correlations
#' correlations(mtcars, Vars = c("mpg", "cyl", "disp", "hp"), min_r = 0.5)
#'
#' # Partial correlations with Bonferroni correction
#' correlations(mtcars, Vars = c("mpg", "cyl", "disp", "hp"),
#'              type = "partial", p_adjust = "bonferroni")
#'
#' # Only significant correlations
#' correlations(mtcars, Vars = c("mpg", "cyl", "disp", "hp", "wt"),
#'              sig_only = TRUE)
#'
#' # Multilevel correlations (within-person)
#' correlations(longitudinal_data, Vars = c("anxiety", "depression", "stress"),
#'              multilevel = TRUE, id = "participant_id")
#'
#' # Multilevel with between-cluster correlations
#' correlations(longitudinal_data, Vars = c("anxiety", "depression"),
#'              multilevel = TRUE, id = "participant_id", between = TRUE)
#'
#' # Correlations by group (stratified)
#' correlations(mtcars, Vars = c("mpg", "hp", "wt"), group_by = "cyl")
#'
#' # Correlations by group with significance filter
#' correlations(mtcars, Vars = c("mpg", "hp", "wt", "disp"),
#'              group_by = "am", sig_only = TRUE)
#'
#' # Include only specific statistics
#' correlations(mtcars, Vars = c("mpg", "hp", "wt"),
#'              include = c("r", "ci", "p", "sig"))
#'
#' # Exclude statistics you don't need
#' correlations(mtcars, Vars = c("mpg", "hp", "wt"),
#'              exclude = c("stat", "df"))
#'
#' # Disable auto-consolidation (always show df and n columns)
#' correlations(mtcars, Vars = c("mpg", "hp", "wt"),
#'              auto_consolidate = FALSE)
#' }
#'
#' @importFrom stats cor.test p.adjust complete.cases qt pt qnorm lm residuals ave
#' @importFrom gt gt tab_header tab_source_note cols_align tab_style cell_text cells_body tab_row_group
#' @export
correlations <- function(data,
                         Vars = NULL,
                         type = c("bivariate", "partial", "semi-partial"),
                         method = c("pearson", "spearman", "kendall"),
                         p_adjust = c("none", "bonferroni", "holm", "fdr"),
                         ci_level = 0.95,
                         min_r = NULL,
                         sig_only = FALSE,
                         multilevel = FALSE,
                         id = NULL,
                         between = FALSE,
                         group_by = NULL,
                         include = NULL,
                         exclude = NULL,
                         auto_consolidate = TRUE,
                         digits = 3,
                         title = NULL,
                         format = c("gt", "plain", "markdown", "latex", "kable"),
                         show_header = TRUE) {

  # Match arguments
  type <- match.arg(type)
  method <- match.arg(method)
  p_adjust <- match.arg(p_adjust)
  format <- match.arg(format)

  # Validate include/exclude
  valid_stats <- c("r", "ci", "stat", "df", "p", "n", "sig")
  if (!is.null(include) && !is.null(exclude)) {
    stop("Use 'include' OR 'exclude', not both")
  }
  if (!is.null(include)) {
    invalid <- setdiff(include, valid_stats)
    if (length(invalid) > 0) {
      stop("Invalid include values: ", paste(invalid, collapse = ", "),
           ". Valid options: ", paste(valid_stats, collapse = ", "))
    }
  }
  if (!is.null(exclude)) {
    invalid <- setdiff(exclude, valid_stats)
    if (length(invalid) > 0) {
      stop("Invalid exclude values: ", paste(invalid, collapse = ", "),
           ". Valid options: ", paste(valid_stats, collapse = ", "))
    }
  }

  # Validate inputs
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  # If Vars is NULL, use all numeric variables
  if (is.null(Vars)) {
    Vars <- names(data)[sapply(data, is.numeric)]
    if (length(Vars) < 2) {
      stop("Data must contain at least 2 numeric variables")
    }
    cat(sprintf("Using all %d numeric variables: %s\n\n",
                length(Vars), paste(Vars, collapse = ", ")))
  }

  if (length(Vars) < 2) {
    stop("'Vars' must contain at least 2 variable names")
  }

  if (type %in% c("partial", "semi-partial") && length(Vars) < 3) {
    stop("Partial and semi-partial correlations require at least 3 variables")
  }

  # Validate group_by (now expects quoted string)
  if (!is.null(group_by)) {
    if (!is.character(group_by) || length(group_by) != 1) {
      stop("'group_by' must be a character string (variable name)")
    }
    if (!group_by %in% names(data)) {
      stop("Group variable '", group_by, "' not found in data")
    }
  }

 # Handle group_by: compute correlations for each group and combine
  if (!is.null(group_by)) {
    group_levels <- unique(data[[group_by]])
    group_levels <- group_levels[!is.na(group_levels)]

    cat(sprintf("Computing correlations for %d groups: %s\n\n",
                length(group_levels), paste(group_levels, collapse = ", ")))

    all_results <- list()
    all_display <- list()

    for (g in group_levels) {
      group_data <- data[data[[group_by]] == g & !is.na(data[[group_by]]), ]

      if (nrow(group_data) < 3) {
        warning("Group '", g, "' has fewer than 3 observations, skipping")
        next
      }

      # Recursively call without group_by
      group_result <- tryCatch({
        suppressMessages(
          correlations(
            data = group_data,
            Vars = Vars,
            type = type,
            method = method,
            p_adjust = p_adjust,
            ci_level = ci_level,
            min_r = min_r,
            sig_only = sig_only,
            multilevel = multilevel,
            id = if (multilevel) id else NULL,
            between = between,
            group_by = NULL,  # Don't recurse further
            include = include,
            exclude = exclude,
            auto_consolidate = auto_consolidate,
            digits = digits,
            title = NULL
          )
        )
      }, error = function(e) {
        warning("Could not compute correlations for group '", g, "': ", e$message)
        NULL
      })

      if (!is.null(group_result) && !is.null(group_result$data)) {
        group_result$data$Group <- g
        group_result$display$Group <- g
        all_results[[as.character(g)]] <- group_result$data
        all_display[[as.character(g)]] <- group_result$display
      }
    }

    if (length(all_results) == 0) {
      stop("Could not compute correlations for any group")
    }

    # Combine results
    results_df <- do.call(rbind, all_results)
    display_df <- do.call(rbind, all_display)

    # Reorder columns to put Group first
    results_df <- results_df[, c("Group", setdiff(names(results_df), "Group"))]
    display_df <- display_df[, c("Group", setdiff(names(display_df), "Group"))]

    # Sort by Group, then by absolute r
    results_df <- results_df[order(results_df$Group, -abs(results_df$r)), ]
    display_df <- display_df[order(display_df$Group, -abs(results_df$r)), ]

    rownames(results_df) <- NULL
    rownames(display_df) <- NULL

    n_significant <- sum(results_df$p < 0.05)

    # Create gt table with row groups
    gt_table <- gt::gt(display_df) |>
      gt::cols_align(align = "left", columns = c("Group", "Variable1", "Variable2")) |>
      gt::cols_align(align = "center", columns = setdiff(names(display_df), c("Group", "Variable1", "Variable2"))) |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(columns = c("Variable1", "Variable2"))
      )

    # Title
    type_label <- switch(type,
                         "bivariate" = "",
                         "partial" = "Partial ",
                         "semi-partial" = "Semi-partial ")
    method_label <- switch(method,
                           "pearson" = "Pearson",
                           "spearman" = "Spearman",
                           "kendall" = "Kendall")

    default_title <- paste0(type_label, method_label, " Correlations by ", group_by)
    subtitle <- paste0(length(group_levels), " groups, ", n_significant, " significant (p < .05)")

    gt_table <- gt_table |>
      gt::tab_header(
        title = if (!is.null(title)) title else default_title,
        subtitle = subtitle
      )

    # Footnotes
    footnote <- "* p < .05, ** p < .01, *** p < .001"
    if (p_adjust != "none") {
      footnote <- paste0(footnote, " (", p_adjust, " adjusted per group)")
    }
    gt_table <- gt_table |>
      gt::tab_source_note(source_note = footnote)

    # Color significant
    sig_rows <- which(results_df$p < 0.05)
    if (length(sig_rows) > 0) {
      gt_table <- gt_table |>
        gt::tab_style(
          style = gt::cell_text(color = "#27AE60"),
          locations = gt::cells_body(columns = "Sig", rows = sig_rows)
        )
    }

    # Highlight strong correlations
    strong_pos <- which(results_df$r >= 0.5)
    strong_neg <- which(results_df$r <= -0.5)

    if (length(strong_pos) > 0) {
      gt_table <- gt_table |>
        gt::tab_style(
          style = gt::cell_text(color = "#2980B9", weight = "bold"),
          locations = gt::cells_body(columns = "r", rows = strong_pos)
        )
    }
    if (length(strong_neg) > 0) {
      gt_table <- gt_table |>
        gt::tab_style(
          style = gt::cell_text(color = "#C0392B", weight = "bold"),
          locations = gt::cells_body(columns = "r", rows = strong_neg)
        )
    }

    print(gt_table)

    return(invisible(list(
      table = gt_table,
      data = results_df,
      display = display_df,
      n_pairs = nrow(results_df),
      n_significant = n_significant,
      n_groups = length(group_levels),
      group_by = group_by,
      type = type,
      method = method,
      p_adjust = p_adjust
    )))
  }

  # Validate id variable for multilevel (expects quoted string)
  id_var <- NULL
  if (multilevel) {
    if (is.null(id)) {
      stop("'id' must be specified when multilevel = TRUE")
    }
    if (!is.character(id) || length(id) != 1) {
      stop("'id' must be a character string (variable name)")
    }
    id_var <- id

    if (!id_var %in% names(data)) {
      stop("ID variable '", id_var, "' not found in data")
    }
  }

  # Check that all variables exist
  missing_vars <- setdiff(Vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }

  # Subset data
  if (multilevel) {
    data_subset <- data[, c(id_var, Vars), drop = FALSE]
  } else {
    data_subset <- data[, Vars, drop = FALSE]
  }

  # Check numeric (excluding id variable)
  check_vars <- if (multilevel) Vars else names(data_subset)
  non_numeric <- check_vars[!sapply(data_subset[check_vars], is.numeric)]
  if (length(non_numeric) > 0) {
    stop("All variables must be numeric. Non-numeric: ", paste(non_numeric, collapse = ", "))
  }

  # For partial/semi-partial or multilevel, use complete cases
  if (type %in% c("partial", "semi-partial") || multilevel) {
    data_subset <- data_subset[complete.cases(data_subset), ]
    if (nrow(data_subset) < 3) {
      stop("Fewer than 3 complete cases available")
    }
  }

  # Multilevel: group-mean center variables
  n_clusters <- NULL
  if (multilevel) {
    cluster_ids <- data_subset[[id_var]]
    n_clusters <- length(unique(cluster_ids))

    if (n_clusters < 2) {
      stop("Multilevel correlations require at least 2 clusters/IDs")
    }

    # Group-mean center each variable
    for (var_name in Vars) {
      group_means <- ave(data_subset[[var_name]], cluster_ids, FUN = mean)
      data_subset[[paste0(var_name, "_within")]] <- data_subset[[var_name]] - group_means
      data_subset[[paste0(var_name, "_between")]] <- group_means
    }

    cat(sprintf("Multilevel correlations: %d observations in %d clusters\n",
                nrow(data_subset), n_clusters))
  }

  n_vars <- length(Vars)
  n_total <- nrow(data_subset)

  # Generate all unique pairs
  pairs <- combn(Vars, 2)
  n_pairs <- ncol(pairs)

  # Initialize results storage
  results <- vector("list", n_pairs)

  # Calculate correlations for each pair
  for (k in 1:n_pairs) {
    var1 <- pairs[1, k]
    var2 <- pairs[2, k]

    if (type == "bivariate") {
      # Bivariate correlation (or multilevel within-cluster)
      if (multilevel) {
        # Use within-cluster centered variables
        x <- data_subset[[paste0(var1, "_within")]]
        y <- data_subset[[paste0(var2, "_within")]]
      } else {
        x <- data_subset[[var1]]
        y <- data_subset[[var2]]
      }
      complete_mask <- complete.cases(x, y)
      n <- sum(complete_mask)

      if (n >= 3) {
        test_result <- tryCatch({
          cor.test(x, y, method = method, conf.level = ci_level)
        }, error = function(e) NULL)

        if (!is.null(test_result)) {
          r <- as.numeric(test_result$estimate)
          p_raw <- test_result$p.value
          stat <- as.numeric(test_result$statistic)

          # Adjust df for multilevel
          if (multilevel) {
            # df = n - k - 1 where k is number of clusters
            df <- n - n_clusters - 1
            if (df > 0 && !is.na(r) && abs(r) < 1) {
              # Recalculate t and p with adjusted df
              stat <- r * sqrt(df / (1 - r^2))
              p_raw <- 2 * pt(-abs(stat), df = df)
            }
          } else {
            df <- if (!is.null(test_result$parameter)) as.numeric(test_result$parameter) else n - 2
          }

          # CI for Pearson
          if (method == "pearson" && n >= 4) {
            if (!multilevel && !is.null(test_result$conf.int)) {
              ci_low <- test_result$conf.int[1]
              ci_high <- test_result$conf.int[2]
            } else {
              # Fisher's z (use adjusted df for multilevel)
              z <- atanh(r)
              se <- 1 / sqrt(max(df - 1, 1))
              z_crit <- qnorm(1 - (1 - ci_level) / 2)
              ci_low <- tanh(z - z_crit * se)
              ci_high <- tanh(z + z_crit * se)
            }
          } else {
            ci_low <- NA
            ci_high <- NA
          }

          results[[k]] <- data.frame(
            Variable1 = var1,
            Variable2 = var2,
            r = r,
            CI_low = ci_low,
            CI_high = ci_high,
            statistic = stat,
            df = df,
            p = p_raw,
            n = n,
            stringsAsFactors = FALSE
          )
        }
      }
    } else {
      # Partial or semi-partial correlation
      control_vars <- setdiff(Vars, c(var1, var2))
      z_data <- data_subset[, control_vars, drop = FALSE]

      x <- data_subset[[var1]]
      y <- data_subset[[var2]]
      n <- n_total

      tryCatch({
        if (type == "partial") {
          # Regress both on controls
          x_resid <- residuals(lm(x ~ ., data = z_data))
          y_resid <- residuals(lm(y ~ ., data = z_data))
          r <- cor(x_resid, y_resid, method = method)
        } else {
          # Semi-partial: only regress y on controls
          y_resid <- residuals(lm(y ~ ., data = z_data))
          r <- cor(x, y_resid, method = method)
        }

        # Calculate test statistic and p-value
        k_control <- length(control_vars)
        df <- n - k_control - 2

        if (df > 0 && !is.na(r) && abs(r) < 1) {
          t_stat <- r * sqrt(df / (1 - r^2))
          p <- 2 * pt(-abs(t_stat), df = df)

          # CI using Fisher's z
          if (method == "pearson" && n >= 4) {
            z <- atanh(r)
            se <- 1 / sqrt(df)
            z_crit <- qnorm(1 - (1 - ci_level) / 2)
            ci_low <- tanh(z - z_crit * se)
            ci_high <- tanh(z + z_crit * se)
          } else {
            ci_low <- NA
            ci_high <- NA
          }

          results[[k]] <- data.frame(
            Variable1 = var1,
            Variable2 = var2,
            r = r,
            CI_low = ci_low,
            CI_high = ci_high,
            statistic = t_stat,
            df = df,
            p = p,
            n = n,
            stringsAsFactors = FALSE
          )
        }
      }, error = function(e) {
        warning("Could not calculate correlation for ", var1, " and ", var2, ": ", e$message)
      })
    }
  }

  # Combine results
  results_df <- do.call(rbind, results[!sapply(results, is.null)])

  if (is.null(results_df) || nrow(results_df) == 0) {
    stop("Could not calculate any correlations")
  }

  # Add level indicator for multilevel
  if (multilevel) {
    results_df$Level <- "Within"
  }

  # Calculate between-cluster correlations if requested
  between_df <- NULL
  if (multilevel && between) {
    between_results <- vector("list", n_pairs)

    # Get unique cluster means
    cluster_ids <- data_subset[[id_var]]
    unique_clusters <- unique(cluster_ids)
    n_between <- length(unique_clusters)

    for (k in 1:n_pairs) {
      var1 <- pairs[1, k]
      var2 <- pairs[2, k]

      # Get cluster means
      x_means <- tapply(data_subset[[var1]], cluster_ids, mean)
      y_means <- tapply(data_subset[[var2]], cluster_ids, mean)

      if (length(x_means) >= 3) {
        test_result <- tryCatch({
          cor.test(x_means, y_means, method = method, conf.level = ci_level)
        }, error = function(e) NULL)

        if (!is.null(test_result)) {
          r <- as.numeric(test_result$estimate)
          p_raw <- test_result$p.value
          stat <- as.numeric(test_result$statistic)
          df <- if (!is.null(test_result$parameter)) as.numeric(test_result$parameter) else n_between - 2

          # CI for Pearson
          if (method == "pearson" && n_between >= 4) {
            if (!is.null(test_result$conf.int)) {
              ci_low <- test_result$conf.int[1]
              ci_high <- test_result$conf.int[2]
            } else {
              z <- atanh(r)
              se <- 1 / sqrt(n_between - 3)
              z_crit <- qnorm(1 - (1 - ci_level) / 2)
              ci_low <- tanh(z - z_crit * se)
              ci_high <- tanh(z + z_crit * se)
            }
          } else {
            ci_low <- NA
            ci_high <- NA
          }

          between_results[[k]] <- data.frame(
            Variable1 = var1,
            Variable2 = var2,
            r = r,
            CI_low = ci_low,
            CI_high = ci_high,
            statistic = stat,
            df = df,
            p = p_raw,
            n = n_between,
            Level = "Between",
            stringsAsFactors = FALSE
          )
        }
      }
    }

    between_df <- do.call(rbind, between_results[!sapply(between_results, is.null)])

    # Combine within and between
    if (!is.null(between_df) && nrow(between_df) > 0) {
      results_df <- rbind(results_df, between_df)
    }
  }

  # Adjust p-values
  if (p_adjust != "none") {
    results_df$p_original <- results_df$p
    results_df$p <- p.adjust(results_df$p, method = p_adjust)
  }

  # Add significance stars
  results_df$sig <- ifelse(results_df$p < 0.001, "***",
                           ifelse(results_df$p < 0.01, "**",
                                  ifelse(results_df$p < 0.05, "*", "")))

  # Filter by min_r
  if (!is.null(min_r)) {
    results_df <- results_df[abs(results_df$r) >= min_r, ]
  }

  # Filter by significance
  if (sig_only) {
    results_df <- results_df[results_df$p < 0.05, ]
  }

  if (nrow(results_df) == 0) {
    cat("No correlations match the specified criteria.\n")
    return(invisible(list(table = NULL, data = results_df, n_pairs = 0, n_significant = 0)))
  }

  # Sort by absolute r (descending)
  results_df <- results_df[order(-abs(results_df$r)), ]

  # Count significant
  n_significant <- sum(results_df$p < 0.05)

  # Format for display
  display_df <- data.frame(
    Variable1 = results_df$Variable1,
    Variable2 = results_df$Variable2,
    stringsAsFactors = FALSE
  )

  # Add Level column for multilevel
  if (multilevel) {
    display_df$Level <- results_df$Level
  }

  # Format r
  display_df$r <- sapply(results_df$r, function(r) {
    fmt <- formatC(r, format = "f", digits = digits)
    sub("^0\\.", ".", sub("^-0\\.", "-.", fmt))
  })

  # Format CI
  if (method == "pearson") {
    display_df$`95% CI` <- mapply(function(low, high) {
      if (is.na(low) || is.na(high)) return("—")
      low_fmt <- sub("^0\\.", ".", sub("^-0\\.", "-.", formatC(low, format = "f", digits = digits)))
      high_fmt <- sub("^0\\.", ".", sub("^-0\\.", "-.", formatC(high, format = "f", digits = digits)))
      paste0("[", low_fmt, ", ", high_fmt, "]")
    }, results_df$CI_low, results_df$CI_high)
  }

  # Format statistic
  stat_name <- switch(method,
                      "pearson" = "t",
                      "spearman" = "S",
                      "kendall" = "z")
  display_df[[stat_name]] <- formatC(results_df$statistic, format = "f", digits = 2)

  # Format df
  display_df$df <- as.integer(results_df$df)

  # Format p
  display_df$p <- sapply(results_df$p, function(p) {
    if (is.na(p)) return("—")
    if (p < 0.001) return("< .001")
    sub("^0\\.", ".", formatC(p, format = "f", digits = 3))
  })

  # Add n and sig
  display_df$n <- results_df$n
  display_df$Sig <- results_df$sig

  # Auto-consolidate constant values (df, n) to subtitle
  consolidated_info <- list()
  if (auto_consolidate) {
    # Check if df is constant
    if (length(unique(results_df$df)) == 1) {
      consolidated_info$df <- unique(results_df$df)
      display_df$df <- NULL
    }
    # Check if n is constant
    if (length(unique(results_df$n)) == 1) {
      consolidated_info$n <- unique(results_df$n)
      display_df$n <- NULL
    }
  }

  # Apply include/exclude filtering to columns
  # Mapping of user-friendly names to actual column names
  stat_col_map <- list(
    r = "r",
    ci = "95% CI",
    stat = stat_name,  # t, S, or z depending on method
    df = "df",
    p = "p",
    n = "n",
    sig = "Sig"
  )

  if (!is.null(include)) {
    # Keep only included columns (plus Variable1, Variable2, Level)
    keep_cols <- c("Variable1", "Variable2")
    if (multilevel && "Level" %in% names(display_df)) keep_cols <- c(keep_cols, "Level")
    for (stat in include) {
      col_name <- stat_col_map[[stat]]
      if (!is.null(col_name) && col_name %in% names(display_df)) {
        keep_cols <- c(keep_cols, col_name)
      }
    }
    display_df <- display_df[, keep_cols, drop = FALSE]
  } else if (!is.null(exclude)) {
    # Remove excluded columns
    for (stat in exclude) {
      col_name <- stat_col_map[[stat]]
      if (!is.null(col_name) && col_name %in% names(display_df)) {
        display_df[[col_name]] <- NULL
      }
    }
  }

  # ===========================================================================
  # Build title and subtitle strings
  # ===========================================================================
  type_label <- switch(type,
                       "bivariate" = "",
                       "partial" = "Partial ",
                       "semi-partial" = "Semi-partial ")
  method_label <- switch(method,
                         "pearson" = "Pearson",
                         "spearman" = "Spearman",
                         "kendall" = "Kendall")

  if (multilevel) {
    default_title <- paste0("Multilevel ", method_label, " Correlations")
    subtitle <- paste0(n_total, " observations in ", n_clusters, " clusters | ",
                       n_significant, " significant (p < .05)")
  } else {
    default_title <- paste0(type_label, method_label, " Correlations")
    subtitle <- paste0(nrow(results_df), " pairs, ", n_significant, " significant (p < .05)")
  }

  # Append consolidated values to subtitle
  if (length(consolidated_info) > 0) {
    consol_parts <- c()
    if (!is.null(consolidated_info$n)) {
      consol_parts <- c(consol_parts, paste0("n = ", consolidated_info$n))
    }
    if (!is.null(consolidated_info$df)) {
      consol_parts <- c(consol_parts, paste0("df = ", consolidated_info$df))
    }
    if (length(consol_parts) > 0) {
      subtitle <- paste0(subtitle, " | ", paste(consol_parts, collapse = ", "))
    }
  }

  final_title <- if (!is.null(title)) title else default_title

  # ===========================================================================
  # Return Non-GT Formats
  # ===========================================================================
  if (format %in% c("plain", "markdown", "latex", "kable")) {
    # Plain format - return data frame
    if (format == "plain") {
      result <- list(
        data = results_df,
        display = display_df,
        n_pairs = nrow(results_df),
        n_significant = n_significant,
        type = type,
        method = method,
        p_adjust = p_adjust,
        consolidated = consolidated_info
      )
      if (multilevel) {
        result$multilevel <- TRUE
        result$n_clusters <- n_clusters
        result$id_var <- id_var
        if (between && !is.null(between_df)) {
          result$between_data <- between_df
        }
      }
      print(display_df)
      return(invisible(result))
    }

    # Markdown format
    if (format == "markdown") {
      # Build markdown table
      header <- paste("|", paste(names(display_df), collapse = " | "), "|")
      separator <- paste("|", paste(rep("---", ncol(display_df)), collapse = " | "), "|")

      rows <- apply(display_df, 1, function(row) {
        paste("|", paste(row, collapse = " | "), "|")
      })

      if (show_header) {
        md_table <- paste(c(
          paste0("## ", final_title),
          paste0("*", subtitle, "*"),
          "",
          header, separator, rows,
          "",
          "\\* p < .05, \\*\\* p < .01, \\*\\*\\* p < .001"
        ), collapse = "\n")
      } else {
        md_table <- paste(c(header, separator, rows), collapse = "\n")
      }

      class(md_table) <- c("markdown_table", "character")
      print(md_table)
      return(invisible(list(
        table = md_table,
        data = results_df,
        display = display_df,
        n_pairs = nrow(results_df),
        n_significant = n_significant
      )))
    }

    # LaTeX format
    if (format == "latex") {
      col_align <- paste(rep("l", ncol(display_df)), collapse = "")
      header <- paste(names(display_df), collapse = " & ")

      rows <- apply(display_df, 1, function(row) {
        paste(row, collapse = " & ")
      })

      if (show_header) {
        latex_table <- paste(c(
          paste0("% ", final_title),
          "\\begin{tabular}{", col_align, "}",
          "\\hline",
          paste0(header, " \\\\"),
          "\\hline",
          paste0(rows, " \\\\"),
          "\\hline",
          "\\end{tabular}",
          paste0("% ", subtitle)
        ), collapse = "\n")
      } else {
        latex_table <- paste(c(
          "\\begin{tabular}{", col_align, "}",
          "\\hline",
          paste0(header, " \\\\"),
          "\\hline",
          paste0(rows, " \\\\"),
          "\\hline",
          "\\end{tabular}"
        ), collapse = "\n")
      }

      class(latex_table) <- c("latex_table", "character")
      print(latex_table)
      return(invisible(list(
        table = latex_table,
        data = results_df,
        display = display_df,
        n_pairs = nrow(results_df),
        n_significant = n_significant
      )))
    }

    # Kable format
    if (format == "kable") {
      if (requireNamespace("knitr", quietly = TRUE)) {
        kable_out <- knitr::kable(display_df, format = "markdown", digits = digits)
        if (show_header) {
          kable_out <- c(paste0("## ", final_title), "", kable_out)
        }
        print(kable_out)
        return(invisible(list(
          table = kable_out,
          data = results_df,
          display = display_df,
          n_pairs = nrow(results_df),
          n_significant = n_significant
        )))
      } else {
        warning("knitr not available, returning markdown format")
        format <- "markdown"
      }
    }
  }

  # ===========================================================================
  # Create gt table
  # ===========================================================================
  left_cols <- c("Variable1", "Variable2")
  if (multilevel && "Level" %in% names(display_df)) left_cols <- c(left_cols, "Level")

  gt_table <- gt::gt(display_df) |>
    gt::cols_align(align = "left", columns = left_cols) |>
    gt::cols_align(align = "center", columns = setdiff(names(display_df), left_cols)) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = c("Variable1", "Variable2"))
    )

  # Add header (respecting show_header parameter)
  if (show_header) {
    gt_table <- gt_table |>
      gt::tab_header(
        title = final_title,
        subtitle = subtitle
      )
  }

  # Footnotes
  footnote <- "* p < .05, ** p < .01, *** p < .001"
  if (p_adjust != "none") {
    footnote <- paste0(footnote, " (", p_adjust, " adjusted)")
  }
  gt_table <- gt_table |>
    gt::tab_source_note(source_note = footnote)

  if (type != "bivariate") {
    gt_table <- gt_table |>
      gt::tab_source_note(source_note = "Controlling for other variables in Vars")
  }

  if (multilevel) {
    gt_table <- gt_table |>
      gt::tab_source_note(source_note = paste0("Within: group-mean centered (removes between-cluster variance). ID variable: ", id_var))
  }

  # Color significant rows (only if Sig column exists)
  if ("Sig" %in% names(display_df)) {
    sig_rows <- which(results_df$p < 0.05)
    if (length(sig_rows) > 0) {
      gt_table <- gt_table |>
        gt::tab_style(
          style = gt::cell_text(color = "#27AE60"),
          locations = gt::cells_body(columns = "Sig", rows = sig_rows)
        )
    }
  }

  # Highlight strong correlations (only if r column exists)
  if ("r" %in% names(display_df)) {
    strong_pos <- which(results_df$r >= 0.5)
    strong_neg <- which(results_df$r <= -0.5)

    if (length(strong_pos) > 0) {
      gt_table <- gt_table |>
        gt::tab_style(
          style = gt::cell_text(color = "#2980B9", weight = "bold"),
          locations = gt::cells_body(columns = "r", rows = strong_pos)
        )
    }
    if (length(strong_neg) > 0) {
      gt_table <- gt_table |>
        gt::tab_style(
          style = gt::cell_text(color = "#C0392B", weight = "bold"),
          locations = gt::cells_body(columns = "r", rows = strong_neg)
        )
    }
  }

  print(gt_table)

  result <- list(
    table = gt_table,
    data = results_df,
    display = display_df,
    n_pairs = nrow(results_df),
    n_significant = n_significant,
    type = type,
    method = method,
    p_adjust = p_adjust,
    consolidated = consolidated_info
  )

  if (multilevel) {
    result$multilevel <- TRUE
    result$n_clusters <- n_clusters
    result$id_var <- id_var
    if (between && !is.null(between_df)) {
      result$between_data <- between_df
    }
  }

  invisible(result)
}
