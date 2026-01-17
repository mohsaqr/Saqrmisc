# Saqrmisc Package: Mosaic Analysis Functions
#
# This file contains functions for categorical variable analysis using mosaic plots.
# Functions: mosaic_analysis

#' @importFrom vcd mosaic labeling_values
#' @importFrom grid gpar
#' @importFrom dplyr select bind_rows mutate everything
#' @importFrom tibble tibble
#' @importFrom stats chisq.test fisher.test as.formula
NULL

#' Create consolidated frequency table (internal helper)
#'
#' @param observed Observed frequency table
#' @param expected Expected frequency table
#' @param show_percentages Logical for showing percentages
#' @param percentage_base Base for percentage calculation
#' @return A tibble with consolidated results
#' @noRd
create_consolidated_table <- function(observed, expected, show_percentages = TRUE, percentage_base = "total") {


  # Convert to data frames
  obs_df <- as.data.frame.matrix(observed)
  exp_df <- as.data.frame.matrix(expected)

  # Calculate percentages if requested
  pct_df <- NULL
  if (show_percentages) {
    if (percentage_base == "total") {
      pct_df <- as.data.frame.matrix(round(observed / sum(observed) * 100, 1))
    } else if (percentage_base == "row") {
      pct_df <- as.data.frame.matrix(round(observed / rowSums(observed) * 100, 1))
    } else if (percentage_base == "column") {
      pct_df <- as.data.frame.matrix(round(t(t(observed) / colSums(observed)) * 100, 1))
    }
  }

  # Create consolidated table
  consolidated <- tibble::tibble()

  for (i in 1:nrow(obs_df)) {
    row_name <- rownames(obs_df)[i]

    # Observed row
    obs_row <- tibble::tibble(
      Variable = row_name,
      Type = "Observed",
      !!!obs_df[i, ],
      Total = sum(obs_df[i, ])
    )

    # Expected row
    exp_row <- tibble::tibble(
      Variable = "",
      Type = "Expected",
      !!!round(exp_df[i, ], 1),
      Total = round(sum(exp_df[i, ]), 1)
    )

    # Percentage row (if requested)
    if (show_percentages && !is.null(pct_df)) {
      pct_row <- tibble::tibble(
        Variable = "",
        Type = paste0("% (", percentage_base, ")"),
        !!!pct_df[i, ],
        Total = if (percentage_base == "row") 100.0 else round(sum(pct_df[i, ]), 1)
      )
      consolidated <- dplyr::bind_rows(consolidated, obs_row, exp_row, pct_row)
    } else {
      consolidated <- dplyr::bind_rows(consolidated, obs_row, exp_row)
    }
  }

  # Add totals row
  total_obs <- tibble::tibble(
    Variable = "Total",
    Type = "Observed",
    !!!colSums(obs_df),
    Total = sum(obs_df)
  )

  total_exp <- tibble::tibble(
    Variable = "",
    Type = "Expected",
    !!!round(colSums(exp_df), 1),
    Total = round(sum(exp_df), 1)
  )

  if (show_percentages && !is.null(pct_df)) {
    if (percentage_base == "total") {
      total_pct <- tibble::tibble(
        Variable = "",
        Type = paste0("% (", percentage_base, ")"),
        !!!round(colSums(observed) / sum(observed) * 100, 1),
        Total = 100.0
      )
    } else if (percentage_base == "column") {
      total_pct <- tibble::tibble(
        Variable = "",
        Type = paste0("% (", percentage_base, ")"),
        !!!rep(100.0, ncol(observed)),
        Total = 100.0
      )
    } else {
      total_pct <- tibble::tibble(
        Variable = "",
        Type = paste0("% (", percentage_base, ")"),
        !!!round(colSums(pct_df), 1),
        Total = round(sum(pct_df), 1)
      )
    }
    consolidated <- dplyr::bind_rows(consolidated, total_obs, total_exp, total_pct)
  } else {
    consolidated <- dplyr::bind_rows(consolidated, total_obs, total_exp)
  }

  return(consolidated)
}

#' Interpret Cramer's V effect size
#'
#' @param v Cramer's V value
#' @param df Degrees of freedom (min of rows-1, cols-1)
#' @return Character string with interpretation
#' @noRd
interpret_cramers_v <- function(v, df) {
  # Cohen's guidelines adjusted for df
  # For df=1: small=0.10, medium=0.30, large=0.50
  # For df=2: small=0.07, medium=0.21, large=0.35
  # For df=3: small=0.06, medium=0.17, large=0.29
  # For df>=4: small=0.05, medium=0.15, large=0.25

  if (df == 1) {
    thresholds <- c(small = 0.10, medium = 0.30, large = 0.50)
  } else if (df == 2) {
    thresholds <- c(small = 0.07, medium = 0.21, large = 0.35)
  } else if (df == 3) {
    thresholds <- c(small = 0.06, medium = 0.17, large = 0.29)
  } else {
    thresholds <- c(small = 0.05, medium = 0.15, large = 0.25)
  }

  if (is.na(v)) return("NA")
  if (v < thresholds["small"]) return("negligible")
  if (v < thresholds["medium"]) return("small")
  if (v < thresholds["large"]) return("medium")
  return("large")
}

#' Perform a comprehensive mosaic plot analysis
#'
#' @description
#' This function conducts a complete mosaic plot analysis for two categorical variables.
#' It includes filtering by minimum count, chi-square testing (with optional Fisher's exact test),
#' calculation of Cramer's V with effect size interpretation, and generation of a detailed
#' summary table with observed/expected counts and residuals.
#'
#' @param data A data frame containing the variables.
#' @param var1 Character. Name of the first categorical variable.
#' @param var2 Character. Name of the second categorical variable.
#' @param min_count The minimum number of observations required for a category to be
#'   included in the analysis. Defaults to 10.
#' @param fontsize The font size for the mosaic plot labels. Defaults to 8.
#' @param title The title for the mosaic plot. Defaults to "".
#' @param var1_label The label for the first variable in the plot. If NULL, uses the
#'   variable name.
#' @param var2_label The label for the second variable in the plot. If NULL, uses the
#'   variable name.
#' @param show_percentages Logical. If TRUE, includes percentages in the summary table.
#'   Defaults to TRUE.
#' @param percentage_base The base for calculating percentages ("total", "row", or "column").
#'   Defaults to "total".
#' @param use_fisher Logical. If TRUE, uses Fisher's exact test instead of chi-square
#'   (recommended for small expected cell counts). Defaults to FALSE.
#' @param verbose Logical. If TRUE, prints results to console. Defaults to TRUE.
#' @param save_plot Optional file path to save the mosaic plot. Supports .png, .pdf, .svg.
#'   Defaults to NULL (no saving).
#' @param interpret Logical. Pass results to AI for automatic interpretation?
#'   Default FALSE. When TRUE, generates clean Methods and Results text using
#'   AI. Includes chi-square/Fisher test results and Cramer's V effect size.
#'   Requires API key setup (see \code{\link{set_api_key}}).
#' @param ... Additional arguments passed to \code{\link{pass}} when
#'   interpret = TRUE (e.g., provider, model, context, append_prompt).
#'
#' @return A list of class "mosaic_analysis" containing:
#' \itemize{
#'   \item{\code{plot}}: The mosaic plot object
#'   \item{\code{consolidated_table}}: Tibble with observed, expected counts and percentages
#'   \item{\code{residuals}}: Data frame of standardized Pearson residuals
#'   \item{\code{chi_test}}: Chi-square test results (or Fisher's test if use_fisher=TRUE)
#'   \item{\code{cramers_v}}: Cramer's V effect size value
#'   \item{\code{cramers_v_interpretation}}: Effect size interpretation (negligible/small/medium/large)
#'   \item{\code{stats_summary}}: Tibble summarizing all statistical results
#'   \item{\code{filtered_data}}: The filtered data used for analysis
#'   \item{\code{original_n}}: Original sample size before filtering
#'   \item{\code{filtered_n}}: Sample size after filtering
#'   \item{\code{removed_categories}}: List of categories removed due to min_count
#' }
#'
#' @examples
#' # Create example data
#' set.seed(123)
#' example_data <- data.frame(
#'   gender = sample(c("Male", "Female"), 200, replace = TRUE),
#'   education = sample(c("High School", "Bachelor", "Master", "PhD"), 200,
#'                      replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1))
#' )
#'
#' # Basic usage with unquoted variable names
#' results <- mosaic_analysis(example_data, gender, education)
#'
#' # With quoted variable names
#' results <- mosaic_analysis(example_data, "gender", "education")
#'
#' # With row percentages and custom labels
#' results <- mosaic_analysis(
#'   example_data, gender, education,
#'   min_count = 5,
#'   var1_label = "Gender",
#'   var2_label = "Education Level",
#'   percentage_base = "row"
#' )
#'
#' # Using Fisher's exact test for small samples
#' results <- mosaic_analysis(
#'   example_data, gender, education,
#'   use_fisher = TRUE,
#'   verbose = FALSE
#' )
#'
#' # Access results
#' results$cramers_v
#' results$cramers_v_interpretation
#' results$stats_summary
#'
#' @export
mosaic_analysis <- function(data, var1, var2, min_count = 10,
                            fontsize = 8, title = "",
                            var1_label = NULL, var2_label = NULL,
                            show_percentages = TRUE,
                            percentage_base = "total",
                            use_fisher = FALSE,
                            verbose = TRUE,
                            save_plot = NULL,
                            interpret = FALSE,
                            ...) {


  # Input validation

if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (nrow(data) == 0) {
    stop("'data' contains no observations")
  }

  if (!percentage_base %in% c("total", "row", "column")) {
    stop("'percentage_base' must be one of: 'total', 'row', 'column'")
  }

  # Handle variable names (expects quoted strings)
  if (missing(var1) || is.null(var1)) {
    stop("'var1' must be specified as a character string")
  }
  if (!is.character(var1) || length(var1) != 1) {
    stop("'var1' must be a single character string (variable name)")
  }
  var1_name <- var1

  if (missing(var2) || is.null(var2)) {
    stop("'var2' must be specified as a character string")
  }
  if (!is.character(var2) || length(var2) != 1) {
    stop("'var2' must be a single character string (variable name)")
  }
  var2_name <- var2

  # Validate variable names exist in data
  if (!var1_name %in% names(data)) {
    stop(paste0("Variable '", var1_name, "' not found in data"))
  }

  if (!var2_name %in% names(data)) {
    stop(paste0("Variable '", var2_name, "' not found in data"))
  }

  # Set default labels if NULL
  if (is.null(var1_label)) {
    var1_label <- var1_name
  }

  if (is.null(var2_label)) {
    var2_label <- var2_name
  }

  # Remove missing values
  clean_data <- data[!is.na(data[[var1_name]]) & !is.na(data[[var2_name]]), ]

  if (nrow(clean_data) == 0) {
    stop("No complete cases found after removing missing values")
  }

  # Convert to factors and drop unused levels
  clean_data[[var1_name]] <- factor(clean_data[[var1_name]])
  clean_data[[var2_name]] <- factor(clean_data[[var2_name]])

  # Create contingency table
  orig_table <- table(clean_data[[var1_name]], clean_data[[var2_name]])

  # Filter categories below min_count
  var1_counts <- rowSums(orig_table)
  var2_counts <- colSums(orig_table)

  # Keep only categories with sufficient counts
  keep_var1 <- names(var1_counts)[var1_counts >= min_count]
  keep_var2 <- names(var2_counts)[var2_counts >= min_count]

  # Track removed categories
  removed_var1 <- setdiff(names(var1_counts), keep_var1)
  removed_var2 <- setdiff(names(var2_counts), keep_var2)

  # Filter data
  filtered_data <- clean_data[clean_data[[var1_name]] %in% keep_var1 &
                                clean_data[[var2_name]] %in% keep_var2, ]

  # Drop unused factor levels
  filtered_data[[var1_name]] <- droplevels(filtered_data[[var1_name]])
  filtered_data[[var2_name]] <- droplevels(filtered_data[[var2_name]])

  # Create filtered contingency table
  filtered_table <- table(filtered_data[[var1_name]], filtered_data[[var2_name]])

  # Check if we have enough data
  if (nrow(filtered_table) < 2 || ncol(filtered_table) < 2) {
    stop("Not enough categories remaining after filtering. Try reducing min_count.")
  }

  # Perform statistical test
  if (use_fisher) {
    # Fisher's exact test (better for small expected counts)
    stat_test <- stats::fisher.test(filtered_table, simulate.p.value = TRUE, B = 10000)
    test_type <- "Fisher's exact test"
    test_statistic <- NA
    test_df <- NA
    p_value <- stat_test$p.value
  } else {
    # Chi-square test
    stat_test <- stats::chisq.test(filtered_table)
    test_type <- "Chi-square test"
    test_statistic <- stat_test$statistic
    test_df <- stat_test$parameter
    p_value <- stat_test$p.value

    # Warn if expected counts are low
    if (any(stat_test$expected < 5)) {
      warning("Some expected cell counts are < 5. Consider using use_fisher = TRUE for more accurate results.")
    }
  }

  # Calculate Cramer's V (effect size)
  n <- sum(filtered_table)
  k <- min(nrow(filtered_table), ncol(filtered_table))

  if (use_fisher) {
    # For Fisher's test, calculate chi-square statistic for Cramer's V
    chi_for_v <- stats::chisq.test(filtered_table)$statistic
    cramers_v <- as.numeric(sqrt(chi_for_v / (n * (k - 1))))
  } else {
    cramers_v <- as.numeric(sqrt(stat_test$statistic / (n * (k - 1))))
  }

  # Interpret effect size
  df_for_interpretation <- k - 1
  cramers_v_interpretation <- interpret_cramers_v(cramers_v, df_for_interpretation)

  # Create the mosaic plot using the formula approach
  formula_str <- paste("~", var1_name, "+", var2_name)
  formula_obj <- stats::as.formula(formula_str)

  # Create set_varnames for proper labeling
  set_varnames_vector <- c(var1_label, var2_label)
  names(set_varnames_vector) <- c(var1_name, var2_name)

  # Create the mosaic plot
  mosaic_plot <- vcd::mosaic(formula_obj,
                              data = filtered_data,
                              main = title,
                              shade = TRUE,
                              legend = TRUE,
                              labeling = vcd::labeling_values,
                              labeling_args = list(
                                varnames = c(left = FALSE, top = TRUE),
                                varnames_label = c(left = "", top = ""),
                                rot_labels = c(left = 0, top = 0),
                                offset_labels = c(left = 2.5, top = 0.5),
                                set_varnames = set_varnames_vector,
                                gp_text = grid::gpar(fontsize = fontsize)
                              ))

  # Save plot if requested
  if (!is.null(save_plot)) {
    ext <- tolower(tools::file_ext(save_plot))
    if (ext == "png") {
      grDevices::png(save_plot, width = 800, height = 600)
    } else if (ext == "pdf") {
      grDevices::pdf(save_plot, width = 10, height = 8)
    } else if (ext == "svg") {
      grDevices::svg(save_plot, width = 10, height = 8)
    } else {
      warning("Unsupported file format. Use .png, .pdf, or .svg")
    }

    if (ext %in% c("png", "pdf", "svg")) {
      vcd::mosaic(formula_obj,
                  data = filtered_data,
                  main = title,
                  shade = TRUE,
                  legend = TRUE,
                  labeling = vcd::labeling_values,
                  labeling_args = list(
                    varnames = c(left = FALSE, top = TRUE),
                    varnames_label = c(left = "", top = ""),
                    rot_labels = c(left = 0, top = 0),
                    offset_labels = c(left = 2.5, top = 0.5),
                    set_varnames = set_varnames_vector,
                    gp_text = grid::gpar(fontsize = fontsize)
                  ))
      grDevices::dev.off()
      if (verbose) cat("Plot saved to:", save_plot, "\n")
    }
  }

  # Create the consolidated table
  # For Fisher's test, we still need expected values from chi-square
  if (use_fisher) {
    expected_values <- stats::chisq.test(filtered_table)$expected
  } else {
    expected_values <- stat_test$expected
  }

  consolidated_table <- create_consolidated_table(filtered_table, expected_values,
                                                  show_percentages, percentage_base)

  # Create standardized residuals table
  if (use_fisher) {
    chi_for_resid <- stats::chisq.test(filtered_table)
    residuals_df <- as.data.frame.matrix(round(chi_for_resid$stdres, 2))
  } else {
    residuals_df <- as.data.frame.matrix(round(stat_test$stdres, 2))
  }
  residuals_df$Variable <- rownames(residuals_df)
  residuals_df <- residuals_df %>% dplyr::select(Variable, dplyr::everything())

  # Build removed categories text
  removed_text <- paste0(
    if(length(removed_var1) > 0) paste0(var1_name, ": ", paste(removed_var1, collapse = ", ")) else "",
    if(length(removed_var1) > 0 && length(removed_var2) > 0) "; " else "",
    if(length(removed_var2) > 0) paste0(var2_name, ": ", paste(removed_var2, collapse = ", ")) else ""
  )
  if(removed_text == "") removed_text <- "None"

  # Statistical summary
  stats_summary <- tibble::tibble(
    Statistic = c("Test type", "Test statistic", "Degrees of freedom", "p-value",
                  "Cramer's V", "Effect size", "Sample size", "Categories removed"),
    Value = c(
      test_type,
      ifelse(is.na(test_statistic), "N/A", round(test_statistic, 3)),
      ifelse(is.na(test_df), "N/A", test_df),
      ifelse(p_value < 0.001, "< 0.001", round(p_value, 3)),
      round(cramers_v, 3),
      cramers_v_interpretation,
      sum(filtered_table),
      removed_text
    )
  )

  # Print results if verbose
  if (verbose) {
    cat("\n=== MOSAIC ANALYSIS RESULTS ===\n")
    cat("Variables:", var1_name, "\u00d7", var2_name, "\n")
    cat("Minimum count threshold:", min_count, "\n")
    cat("Test used:", test_type, "\n")
    if (show_percentages) {
      cat("Percentages based on:", percentage_base, "\n")
    }
    cat("\n")

    # Print consolidated table
    cat("CONSOLIDATED FREQUENCY TABLE\n")
    cat("===========================\n")
    print(tibble::as_tibble(consolidated_table), n = Inf)
    cat("\n")

    # Print standardized residuals
    cat("STANDARDIZED RESIDUALS\n")
    cat("=====================\n")
    cat("(Values > |2| indicate significant deviation from expected)\n")
    print(tibble::as_tibble(residuals_df), n = Inf)
    cat("\n")

    # Print statistical summary
    cat("STATISTICAL SUMMARY\n")
    cat("==================\n")
    print(tibble::as_tibble(stats_summary), n = Inf)
    cat("\n")
  }

  # Return results as a list with class
  results <- list(
    mosaic_plot = mosaic_plot,
    summary_table = consolidated_table,
    residuals = residuals_df,
    chi_square_test = stat_test,
    cramers_v = cramers_v,
    cramers_v_interpretation = cramers_v_interpretation,
    stats_summary = stats_summary,
    filtered_data = filtered_data,
    original_n = nrow(clean_data),
    filtered_n = nrow(filtered_data),
    removed_categories = list(
      var1 = removed_var1,
      var2 = removed_var2
    ),
    call = match.call()
  )

  class(results) <- c("mosaic_analysis", "list")

  # ===========================================================================
  # AI Interpretation (if requested)
  # ===========================================================================
  if (interpret) {
    # Build comprehensive metadata including chi-square results
    metadata <- list(
      var1 = var1_name,
      var2 = var2_name,
      var1_label = var1_label,
      var2_label = var2_label,
      test_type = test_type,
      total_n = sum(filtered_table),
      original_n = nrow(clean_data),
      filtered_n = nrow(filtered_data)
    )

    # Add chi-square or Fisher test results
    if (use_fisher) {
      metadata$fisher_p <- p_value
    } else {
      metadata$chi_sq <- as.numeric(test_statistic)
      metadata$df <- as.numeric(test_df)
      metadata$p_value <- p_value
    }

    # Add effect size
    metadata$cramers_v <- cramers_v
    metadata$effect_interpretation <- cramers_v_interpretation

    # Add contingency table info
    metadata$n_rows <- nrow(filtered_table)
    metadata$n_cols <- ncol(filtered_table)
    metadata$row_categories <- rownames(filtered_table)
    metadata$col_categories <- colnames(filtered_table)

    # Call the interpretation function
    interpret_with_ai(results, analysis_type = "categorical_analysis", metadata = metadata, ...)
  }

  return(invisible(results))
}

#' Print method for mosaic_analysis objects
#'
#' @param x A mosaic_analysis object
#' @param ... Additional arguments (ignored)
#' @export
print.mosaic_analysis <- function(x, ...) {
  cat("Mosaic Analysis Results\n")
  cat("=======================\n")
  cat("Original N:", x$original_n, "\n")
  cat("Filtered N:", x$filtered_n, "\n")
  cat("Cramer's V:", round(x$cramers_v, 3), "(", x$cramers_v_interpretation, ")\n")
  cat("\nUse summary() for detailed statistics\n")
  invisible(x)
}

#' Summary method for mosaic_analysis objects
#'
#' @param object A mosaic_analysis object
#' @param ... Additional arguments (ignored)
#' @export
summary.mosaic_analysis <- function(object, ...) {
  cat("\n=== MOSAIC ANALYSIS SUMMARY ===\n\n")
  print(object$stats_summary, n = Inf)
  cat("\n")
  invisible(object$stats_summary)
}
