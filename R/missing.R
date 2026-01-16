#' Analyze Missing Data Patterns
#'
#' @description
#' Provides comprehensive missing data analysis including patterns,
#' Little's MCAR test, and visualizations.
#'
#' @param data A data frame to analyze.
#' @param Vars Character vector of variable names. If NULL (default), all variables are included.
#' @param pattern_plot Logical. Create missing data pattern visualization? Default TRUE.
#' @param mcar_test Logical. Perform Little's MCAR test? Default TRUE.
#' @param correlations Logical. Show correlations between missingness indicators? Default FALSE.
#' @param digits Integer. Number of decimal places. Default 2.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{summary}: gt table with missing data summary
#'   \item \code{patterns}: Data frame of missing patterns
#'   \item \code{mcar}: MCAR test results (if requested)
#'   \item \code{plot}: Missing pattern plot (if requested)
#' }
#'
#' @examples
#' \dontrun{
#' # Create data with missing values
#' df <- mtcars
#' df$mpg[c(1, 5, 10)] <- NA
#' df$hp[c(2, 5, 15)] <- NA
#'
#' missing_analysis(df)
#' }
#'
#' @importFrom gt gt tab_header cols_align tab_style cell_text cells_body tab_source_note
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual theme_minimal labs theme element_text element_blank
#' @importFrom stats complete.cases cor mahalanobis pchisq cov
#' @export
missing_analysis <- function(data,
                             Vars = NULL,
                             pattern_plot = TRUE,
                             mcar_test = TRUE,
                             correlations = FALSE,
                             digits = 2) {

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  # Select variables
  if (is.null(Vars)) {
    Vars <- names(data)
  } else {
    missing_vars <- setdiff(Vars, names(data))
    if (length(missing_vars) > 0) {
      stop("Variables not found: ", paste(missing_vars, collapse = ", "))
    }
  }

  data_subset <- data[, Vars, drop = FALSE]
  n_total <- nrow(data_subset)

  # Calculate missing summary per variable
  missing_summary <- lapply(Vars, function(var_name) {
    x <- data_subset[[var_name]]
    n_missing <- sum(is.na(x))
    n_valid <- n_total - n_missing
    pct_missing <- round(n_missing / n_total * 100, digits)

    data.frame(
      Variable = var_name,
      N = n_valid,
      Missing = n_missing,
      `% Missing` = pct_missing,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  summary_df <- do.call(rbind, missing_summary)
  summary_df <- summary_df[order(-summary_df$`% Missing`), ]

  # Overall missing
  n_complete <- sum(complete.cases(data_subset))
  pct_complete <- round(n_complete / n_total * 100, digits)
  total_cells <- n_total * length(Vars)
  total_missing <- sum(is.na(data_subset))
  pct_total_missing <- round(total_missing / total_cells * 100, digits)

  # Create summary gt table
  gt_table <- gt::gt(summary_df) |>
    gt::tab_header(
      title = "Missing Data Summary",
      subtitle = paste0(
        "Complete cases: ", n_complete, " (", pct_complete, "%) | ",
        "Total missing cells: ", total_missing, " (", pct_total_missing, "%)"
      )
    ) |>
    gt::cols_align(align = "left", columns = "Variable") |>
    gt::cols_align(align = "center", columns = c("N", "Missing", "% Missing")) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = "Variable")
    )

  # Highlight rows with missing data
  missing_rows <- which(summary_df$Missing > 0)
  if (length(missing_rows) > 0) {
    gt_table <- gt_table |>
      gt::tab_style(
        style = gt::cell_text(color = "#E74C3C"),
        locations = gt::cells_body(columns = c("Missing", "% Missing"), rows = missing_rows)
      )
  }

  print(gt_table)

  # Missing patterns
  patterns <- NULL
  if (total_missing > 0) {
    # Create missingness indicator matrix
    miss_matrix <- is.na(data_subset)
    # Get unique patterns
    pattern_strings <- apply(miss_matrix, 1, function(row) paste(as.integer(row), collapse = ""))
    pattern_freq <- sort(table(pattern_strings), decreasing = TRUE)

    patterns <- data.frame(
      Pattern = names(pattern_freq),
      Frequency = as.integer(pattern_freq),
      Percent = round(as.integer(pattern_freq) / n_total * 100, digits),
      stringsAsFactors = FALSE
    )

    # Decode pattern to show which variables are missing
    patterns$Variables_Missing <- sapply(patterns$Pattern, function(p) {
      bits <- as.integer(strsplit(p, "")[[1]])
      if (all(bits == 0)) {
        return("(complete)")
      } else {
        return(paste(Vars[bits == 1], collapse = ", "))
      }
    })

    cat("\n--- Missing Patterns ---\n")
    cat("Top patterns (showing up to 10):\n")
    print(head(patterns[, c("Variables_Missing", "Frequency", "Percent")], 10))
  }

  # Little's MCAR test
  mcar_result <- NULL
  if (mcar_test && total_missing > 0) {
    # Only use numeric variables for MCAR test
    numeric_vars <- Vars[sapply(data_subset, is.numeric)]

    if (length(numeric_vars) >= 2) {
      tryCatch({
        mcar_result <- little_mcar_test(data_subset[, numeric_vars, drop = FALSE])
        cat("\n--- Little's MCAR Test ---\n")
        cat(sprintf("Chi-square = %.3f, df = %d, p = %s\n",
                    mcar_result$chi_sq,
                    mcar_result$df,
                    if (mcar_result$p < 0.001) "< .001" else sprintf("%.3f", mcar_result$p)))
        if (mcar_result$p >= 0.05) {
          cat("Interpretation: Data is consistent with MCAR (p >= .05)\n")
        } else {
          cat("Interpretation: Data is NOT consistent with MCAR (p < .05)\n")
        }
      }, error = function(e) {
        warning("Could not perform MCAR test: ", e$message)
      })
    } else {
      cat("\n--- Little's MCAR Test ---\n")
      cat("Requires at least 2 numeric variables with missing data.\n")
    }
  }

  # Missingness correlations
  cor_matrix <- NULL
  if (correlations && total_missing > 0) {
    # Only for variables with some missing
    vars_with_missing <- Vars[colSums(is.na(data_subset)) > 0]
    if (length(vars_with_missing) >= 2) {
      miss_indicators <- as.data.frame(lapply(data_subset[, vars_with_missing], function(x) as.integer(is.na(x))))
      names(miss_indicators) <- paste0(vars_with_missing, "_miss")
      cor_matrix <- cor(miss_indicators)
      cat("\n--- Missingness Correlations ---\n")
      print(round(cor_matrix, digits))
    }
  }

  # Pattern plot
  plot_obj <- NULL
  if (pattern_plot && total_missing > 0) {
    # Prepare data for heatmap
    miss_matrix <- is.na(data_subset)
    # Order by missingness pattern
    row_order <- order(rowSums(miss_matrix), decreasing = TRUE)
    miss_matrix_ordered <- miss_matrix[row_order, , drop = FALSE]

    # Sample if too large
    if (nrow(miss_matrix_ordered) > 100) {
      sample_idx <- seq(1, nrow(miss_matrix_ordered), length.out = 100)
      miss_matrix_ordered <- miss_matrix_ordered[sample_idx, , drop = FALSE]
    }

    # Convert to long format
    plot_data <- expand.grid(
      Row = 1:nrow(miss_matrix_ordered),
      Variable = factor(Vars, levels = Vars)
    )
    plot_data$Missing <- as.vector(miss_matrix_ordered)

    plot_obj <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Variable, y = Row, fill = Missing)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.1) +
      ggplot2::scale_fill_manual(
        values = c("FALSE" = "#3498DB", "TRUE" = "#E74C3C"),
        labels = c("Present", "Missing"),
        name = ""
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Missing Data Pattern",
        x = "",
        y = "Observations (ordered by missingness)"
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        legend.position = "bottom"
      )

    print(plot_obj)
  }

  invisible(list(
    summary = gt_table,
    summary_data = summary_df,
    patterns = patterns,
    mcar = mcar_result,
    correlations = cor_matrix,
    plot = plot_obj,
    n_complete = n_complete,
    pct_complete = pct_complete
  ))
}


#' Little's MCAR Test (internal helper)
#'
#' @param data Data frame with numeric variables only
#' @return List with chi_sq, df, and p-value
#' @keywords internal
little_mcar_test <- function(data) {
  # Implementation of Little's MCAR test
  # Based on Little (1988) JASA

  n <- nrow(data)
  p <- ncol(data)

  # Remove rows with all missing or all complete for pattern analysis
  miss_pattern <- is.na(data)
  pattern_strings <- apply(miss_pattern, 1, paste, collapse = "")
  unique_patterns <- unique(pattern_strings)

  # Need at least 2 patterns
  if (length(unique_patterns) < 2) {
    stop("Need at least 2 missing patterns for MCAR test")
  }

  # Get complete case mean and covariance
  complete_data <- data[complete.cases(data), , drop = FALSE]
  if (nrow(complete_data) < p + 1) {
    stop("Insufficient complete cases for MCAR test")
  }

  mu <- colMeans(complete_data)
  sigma <- cov(complete_data)

  # Calculate test statistic
  chi_sq <- 0
  df <- 0

  for (pattern in unique_patterns) {
    # Skip complete pattern
    if (!grepl("TRUE", pattern)) next

    # Get rows with this pattern
    pattern_rows <- which(pattern_strings == pattern)
    n_j <- length(pattern_rows)

    if (n_j < 2) next

    # Get observed variables for this pattern
    pattern_vec <- as.logical(strsplit(pattern, "")[[1]])
    obs_vars <- !pattern_vec

    if (sum(obs_vars) == 0) next

    # Subset data for this pattern
    data_j <- data[pattern_rows, obs_vars, drop = FALSE]
    mu_j <- mu[obs_vars]
    sigma_j <- sigma[obs_vars, obs_vars, drop = FALSE]

    # Mean of observed values
    y_bar_j <- colMeans(data_j, na.rm = TRUE)

    # Difference from complete case mean
    diff <- y_bar_j - mu_j

    # Add to chi-square
    tryCatch({
      sigma_j_inv <- solve(sigma_j)
      chi_sq <- chi_sq + n_j * t(diff) %*% sigma_j_inv %*% diff
      df <- df + sum(obs_vars)
    }, error = function(e) {
      # Skip if singular
    })
  }

  # Subtract p for adjustment
  df <- df - p

  if (df <= 0) {
    stop("Insufficient degrees of freedom for MCAR test")
  }

  p_value <- pchisq(chi_sq, df, lower.tail = FALSE)

  list(
    chi_sq = as.numeric(chi_sq),
    df = df,
    p = as.numeric(p_value)
  )
}


#' Replace Missing Values
#'
#' @description
#' Imputes missing values using various methods including mean, median,
#' mode, or custom values.
#'
#' @param data A data frame.
#' @param Vars Character vector of variable names to impute.
#' @param method Imputation method: "mean", "median", "mode", "zero", "min", "max", or "value".
#' @param value Custom value to use when method = "value".
#' @param group_by Unquoted variable name for group-wise imputation. Optional.
#' @param suffix Character. Suffix for new columns. If NULL (default), replaces in place.
#'
#' @return Data frame with imputed values.
#'
#' @examples
#' \dontrun{
#' df <- mtcars
#' df$mpg[c(1, 5, 10)] <- NA
#'
#' # Replace with mean
#' df_imputed <- replace_missing(df, Vars = "mpg", method = "mean")
#'
#' # Replace with median, create new column
#' df_imputed <- replace_missing(df, Vars = "mpg", method = "median", suffix = "_imp")
#'
#' # Group-wise mean imputation
#' df_imputed <- replace_missing(df, Vars = "mpg", method = "mean", group_by = cyl)
#' }
#'
#' @export
replace_missing <- function(data,
                            Vars,
                            method = c("mean", "median", "mode", "zero", "min", "max", "value"),
                            value = NULL,
                            group_by = NULL,
                            suffix = NULL) {

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  method <- match.arg(method)

  if (missing(Vars) || length(Vars) == 0) {
    stop("'Vars' must be specified")
  }

  # Check variables exist
  missing_vars <- setdiff(Vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found: ", paste(missing_vars, collapse = ", "))
  }

  # Check for value if method = "value"
  if (method == "value" && is.null(value)) {
    stop("'value' must be specified when method = 'value'")
  }

  # Capture group_by
  group_var <- NULL
  if (!is.null(substitute(group_by))) {
    group_var <- deparse(substitute(group_by))
    if (group_var == "NULL") group_var <- NULL
  }

  # Helper function to get imputation value
  get_impute_value <- function(x, method, value) {
    x_clean <- na.omit(x)
    if (length(x_clean) == 0) return(NA)

    switch(method,
           "mean" = mean(x_clean),
           "median" = median(x_clean),
           "mode" = {
             ux <- unique(x_clean)
             ux[which.max(tabulate(match(x_clean, ux)))]
           },
           "zero" = 0,
           "min" = min(x_clean),
           "max" = max(x_clean),
           "value" = value
    )
  }

  # Process each variable
  result <- data

  for (var_name in Vars) {
    new_col_name <- if (is.null(suffix)) var_name else paste0(var_name, suffix)

    if (is.null(group_var)) {
      # Simple imputation
      impute_val <- get_impute_value(data[[var_name]], method, value)
      new_vals <- data[[var_name]]
      new_vals[is.na(new_vals)] <- impute_val
      result[[new_col_name]] <- new_vals
    } else {
      # Group-wise imputation
      if (!group_var %in% names(data)) {
        stop("Group variable not found: ", group_var)
      }

      groups <- unique(data[[group_var]])
      new_vals <- data[[var_name]]

      for (g in groups) {
        group_mask <- data[[group_var]] == g & !is.na(data[[group_var]])
        group_data <- data[[var_name]][group_mask]
        impute_val <- get_impute_value(group_data, method, value)
        missing_mask <- group_mask & is.na(data[[var_name]])
        new_vals[missing_mask] <- impute_val
      }

      result[[new_col_name]] <- new_vals
    }

    # Count imputed
    n_imputed <- sum(is.na(data[[var_name]])) - sum(is.na(result[[new_col_name]]))
    cat(sprintf("Imputed %d missing values in '%s' using %s%s\n",
                n_imputed, var_name, method,
                if (!is.null(group_var)) paste0(" (grouped by ", group_var, ")") else ""))
  }

  invisible(result)
}
