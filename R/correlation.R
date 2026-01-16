#' Create a Publication-Ready Correlation Matrix
#'
#' @description
#' Creates a formatted correlation matrix with significance stars, optional
#' confidence intervals, and heatmap visualization.
#'
#' @param data A data frame containing the variables to correlate.
#' @param Vars Character vector of variable names to include in the correlation matrix.
#' @param method Correlation method: "pearson" (default), "spearman", or "kendall".
#' @param triangle Which triangle to display: "lower" (default), "upper", or "full".
#' @param diagonal What to show on the diagonal: "dash" (default), "one", or "names".
#' @param show_n Logical. Show pairwise sample sizes? Default FALSE.
#' @param show_ci Logical. Show 95% confidence intervals? Default FALSE.
#' @param show_p Logical. Show p-values below correlations? Default FALSE.
#' @param p_adjust Method for p-value adjustment: "none" (default), "bonferroni", "holm", "fdr".
#' @param stars Logical. Show significance stars? Default TRUE.
#' @param heatmap Logical. Create a heatmap visualization? Default FALSE.
#' @param digits Number of decimal places. Default 2.
#' @param title Optional title for the table.
#' @param use Method for handling missing data: "pairwise" (default) or "complete".
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
#' @importFrom stats cor cor.test p.adjust complete.cases qt
#' @importFrom gt gt tab_header tab_source_note cols_label cols_align tab_style cell_fill cell_text cells_body
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient2 theme_minimal theme labs element_blank element_text
#' @export
correlation_matrix <- function(data,
                                Vars,
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
                                use = c("pairwise", "complete")) {


  # Match arguments
 method <- match.arg(method)
  triangle <- match.arg(triangle)
  diagonal <- match.arg(diagonal)
  p_adjust <- match.arg(p_adjust)
  use <- match.arg(use)

  # Validate inputs
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (missing(Vars) || length(Vars) < 2) {
    stop("'Vars' must contain at least 2 variable names")
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
  if (use == "complete") {
    data_subset <- data_subset[complete.cases(data_subset), ]
    if (nrow(data_subset) < 3) {
      stop("Fewer than 3 complete cases available")
    }
  }

  n_vars <- length(Vars)

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

  # Calculate correlations pairwise
  for (i in 1:n_vars) {
    for (j in 1:n_vars) {
      if (i == j) {
        cor_matrix[i, j] <- 1
        p_matrix[i, j] <- 0
        n_matrix[i, j] <- sum(!is.na(data_subset[[Vars[i]]]))
        ci_lower[i, j] <- 1
        ci_upper[i, j] <- 1
      } else {
        # Get complete pairs
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

  # Create gt table
  gt_table <- gt::gt(display_df) |>
    gt::cols_align(align = "center", columns = -1) |>
    gt::cols_align(align = "left", columns = 1) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = 1)
    )

  # Add title if provided
  if (!is.null(title)) {
    gt_table <- gt_table |>
      gt::tab_header(title = title)
  }

  # Add footnote for stars
  if (stars) {
    footnote <- paste0("* p < .05, ** p < .01, *** p < .001",
                       if (p_adjust != "none") paste0(" (", p_adjust, " adjusted)") else "")
    gt_table <- gt_table |>
      gt::tab_source_note(source_note = footnote)
  }

  # Add method note
  method_note <- paste0("Correlation method: ",
                        switch(method,
                               "pearson" = "Pearson",
                               "spearman" = "Spearman",
                               "kendall" = "Kendall"))
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
    n_matrix = n_matrix
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
