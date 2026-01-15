# Saqrmisc Package: Descriptive Statistics Functions
#
# This file contains functions for generating publication-ready descriptive statistics tables.
# Functions: descriptive_table

#' @importFrom dplyr group_by summarise across all_of mutate select bind_rows arrange n %>%
#' @importFrom rlang sym .data
#' @importFrom stats sd median quantile var IQR setNames
#' @importFrom utils modifyList
#' @importFrom gt gt tab_header tab_spanner cols_label cols_align fmt_number tab_style cell_fill cell_text cells_column_labels cells_body tab_footnote cells_title tab_options px cell_borders
NULL

# Import pipe operator for use in functions
#' @importFrom magrittr %>%
NULL

# Suppress R CMD check notes for tidyverse-style code
utils::globalVariables(c("Variable", "above_threshold", "aic", "bic", "cluster",
                        "delta_aic", "delta_bic", "delta_icl", "icl", "is_best",
                        "is_highest", "loglik", "mean_value", "n_clusters", "pct",
                        "percentage", "size", "value", "variable"))

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================
#' Calculate skewness
#'
#' @param x Numeric vector
#' @param na.rm Remove NA values
#' @return Skewness value
#' @noRd
calc_skewness <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  s <- sd(x)
  sum((x - m)^3) / (n * s^3)
}

#' Calculate kurtosis
#'
#' @param x Numeric vector
#' @param na.rm Remove NA values
#' @return Kurtosis value (excess kurtosis, normal = 0)
#' @noRd
calc_kurtosis <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4) return(NA_real_)
  m <- mean(x)
  s <- sd(x)
  (sum((x - m)^4) / (n * s^4)) - 3
}

#' Calculate standard error of mean
#'
#' @param x Numeric vector
#' @param na.rm Remove NA values
#' @return SE value
#' @noRd
calc_se <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  sd(x) / sqrt(length(x))
}

#' Calculate coefficient of variation
#'
#' @param x Numeric vector
#' @param na.rm Remove NA values
#' @return CV as percentage
#' @noRd
calc_cv <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  (sd(x) / mean(x)) * 100
}

#' Format a single statistic
#'
#' @param x Numeric vector
#' @param stat Statistic name
#' @param digits Decimal places
#' @return Calculated statistic
#' @noRd
calculate_stat <- function(x, stat, digits = 2) {
  x <- x[!is.na(x)]
  n <- length(x)

  result <- switch(stat,
    "n" = n,
    "missing" = NA,  # Calculated separately with full vector
    "missing_pct" = NA,
    "mean" = if (n > 0) mean(x) else NA_real_,
    "sd" = if (n > 1) sd(x) else NA_real_,
    "se" = if (n > 1) calc_se(x, na.rm = FALSE) else NA_real_,
    "var" = if (n > 1) var(x) else NA_real_,
    "median" = if (n > 0) median(x) else NA_real_,
    "min" = if (n > 0) min(x) else NA_real_,
    "max" = if (n > 0) max(x) else NA_real_,
    "range" = if (n > 0) max(x) - min(x) else NA_real_,
    "iqr" = if (n > 0) IQR(x) else NA_real_,
    "q1" = if (n > 0) quantile(x, 0.25, names = FALSE) else NA_real_,
    "q3" = if (n > 0) quantile(x, 0.75, names = FALSE) else NA_real_,
    "skewness" = if (n > 2) calc_skewness(x, na.rm = FALSE) else NA_real_,
    "kurtosis" = if (n > 3) calc_kurtosis(x, na.rm = FALSE) else NA_real_,
    "cv" = if (n > 1 && mean(x) != 0) calc_cv(x, na.rm = FALSE) else NA_real_,
    NA_real_
  )

  return(result)
}

# =============================================================================
# MAIN FUNCTION
# =============================================================================

#' Generate Publication-Ready Descriptive Statistics Table
#'
#' @description
#' Creates a comprehensive, publication-ready table of descriptive statistics
#' for numeric variables. Supports stratification by grouping variables and
#' outputs beautifully formatted gt tables.
#'
#' @param data A data frame containing the variables to summarize.
#' @param Vars A character vector of numeric variable names to describe.
#'   Example: `c("age", "score1", "score2")`.
#' @param group_by Optional. The unquoted name of a grouping variable for
#'   stratified statistics. When provided, statistics are calculated separately
#'   for each group level.
#' @param stats Character vector specifying which statistics to compute.
#'   Default: `c("n", "mean", "sd", "median", "min", "max")`.
#'   Available options:
#'   \describe{
#'     \item{`"n"`}{Sample size (non-missing)}
#'     \item{`"missing"`}{Count of missing values}
#'     \item{`"missing_pct"`}{Percentage of missing values}
#'     \item{`"mean"`}{Arithmetic mean}
#'     \item{`"sd"`}{Standard deviation}
#'     \item{`"se"`}{Standard error of the mean}
#'     \item{`"var"`}{Variance}
#'     \item{`"median"`}{Median (50th percentile)}
#'     \item{`"min"`}{Minimum value}
#'     \item{`"max"`}{Maximum value}
#'     \item{`"range"`}{Range (max - min)}
#'     \item{`"iqr"`}{Interquartile range (Q3 - Q1)}
#'     \item{`"q1"`}{First quartile (25th percentile)}
#'     \item{`"q3"`}{Third quartile (75th percentile)}
#'     \item{`"skewness"`}{Skewness coefficient}
#'     \item{`"kurtosis"`}{Excess kurtosis (normal = 0)}
#'     \item{`"cv"`}{Coefficient of variation (\%)}
#'   }
#' @param digits Integer. Number of decimal places for numeric output.
#'   Default: `2`.
#' @param labels Optional named character vector for variable labels.
#'   Example: `c(age = "Age (years)", score1 = "Test Score")`.
#'   If NULL, variable names are used as-is.
#' @param title Optional character string for table title.
#'   Default: `"Descriptive Statistics"`.
#' @param subtitle Optional character string for table subtitle.
#' @param overall Logical. When `group_by` is specified, also include
#'   overall (ungrouped) statistics? Default: `TRUE`.
#' @param transpose Logical. Transpose the table so variables are columns
#'   and statistics are rows? Default: `FALSE`.
#' @param format Character. Output format: `"gt"` (default) for gt table,
#'   `"data.frame"` for raw data frame, or `"kable"` for kableExtra HTML table.
#' @param theme Character. Visual theme for gt table. Default: `"default"`.
#'   Options:
#'   \itemize{
#'     \item `"default"`: Clean scientific paper style (black/white, simple borders)
#'     \item `"fancy"`: Blue colorful theme with striped rows
#'     \item `"minimal"`: Minimalist with bottom border only
#'     \item `"dark"`: Dark background theme
#'     \item `"colorful"`: Purple accent theme
#'   }
#' @param compare Logical. When `group_by` is specified, run statistical tests
#'   (t-test for 2 groups, ANOVA for 3+ groups) and report p-values and effect
#'   sizes? Default: `FALSE`.
#' @param bold_highest Logical. When `group_by` is specified, bold the highest
#'   mean for each variable? Default: `TRUE` when compare is `TRUE`.
#' @param sig_color Character. Color for significant p-values (< 0.05).
#'   Default: `"red"`. Set to `NULL` to disable coloring.
#'
#' @return Depending on `format`:
#' \describe{
#'   \item{`"gt"`}{A gt table object that can be printed or exported}
#'   \item{`"data.frame"`}{A data frame with computed statistics}
#' }
#'
#' @section Exporting Tables:
#' The gt output can be exported to various formats:
#' \itemize{
#'   \item HTML: `gtsave(table, "descriptives.html")`
#'   \item Word: `gtsave(table, "descriptives.docx")`
#'   \item PDF: `gtsave(table, "descriptives.pdf")`
#'   \item PNG: `gtsave(table, "descriptives.png")`
#' }
#'
#' @examples
#' \dontrun{
#' # ============================================================
#' # EXAMPLE 1: Basic Descriptive Statistics
#' # ============================================================
#' data <- data.frame(
#'   age = rnorm(100, mean = 35, sd = 10),
#'   score = rnorm(100, mean = 75, sd = 15),
#'   income = rnorm(100, mean = 50000, sd = 15000)
#' )
#'
#' # Default statistics (n, mean, sd, median, min, max)
#' descriptive_table(data, Vars = c("age", "score", "income"))
#'
#' # ============================================================
#' # EXAMPLE 2: Custom Statistics Selection
#' # ============================================================
#' descriptive_table(
#'   data = data,
#'   Vars = c("age", "score"),
#'   stats = c("n", "mean", "sd", "se", "median", "iqr", "skewness", "kurtosis")
#' )
#'
#' # ============================================================
#' # EXAMPLE 3: Stratified by Group
#' # ============================================================
#' data$gender <- sample(c("Male", "Female"), 100, replace = TRUE)
#'
#' descriptive_table(
#'   data = data,
#'   Vars = c("age", "score"),
#'   group_by = gender,
#'   overall = TRUE  # Include overall statistics
#' )
#'
#' # ============================================================
#' # EXAMPLE 4: Custom Labels and Title
#' # ============================================================
#' descriptive_table(
#'   data = data,
#'   Vars = c("age", "score", "income"),
#'   labels = c(
#'     age = "Age (years)",
#'     score = "Test Score",
#'     income = "Annual Income ($)"
#'   ),
#'   title = "Sample Characteristics",
#'   subtitle = "N = 100 participants"
#' )
#'
#' # ============================================================
#' # EXAMPLE 5: Export to Data Frame
#' # ============================================================
#' df <- descriptive_table(
#'   data = data,
#'   Vars = c("age", "score"),
#'   format = "data.frame"
#' )
#' print(df)
#'
#' # ============================================================
#' # EXAMPLE 6: Different Themes
#' # ============================================================
#' descriptive_table(data, Vars = c("age", "score"), theme = "minimal")
#' descriptive_table(data, Vars = c("age", "score"), theme = "dark")
#' descriptive_table(data, Vars = c("age", "score"), theme = "colorful")
#' }
#'
#' @seealso
#' \code{\link[gt]{gt}} for gt table customization
#'
#' @importFrom dplyr group_by summarise across all_of mutate select bind_rows arrange n
#' @importFrom gt gt tab_header tab_spanner cols_label cols_align fmt_number tab_style cell_fill cell_text cells_column_labels cells_body tab_footnote cells_title tab_options
#'
#' @export
descriptive_table <- function(data,
                              Vars,
                              group_by = NULL,
                              stats = c("n", "mean", "sd", "median", "min", "max"),
                              digits = 2,
                              labels = NULL,
                              title = "Descriptive Statistics",
                              subtitle = NULL,
                              overall = TRUE,
                              transpose = FALSE,
                              format = "gt",
                              theme = "default",
                              compare = FALSE,
                              bold_highest = NULL,
                              sig_color = "red") {

  # ===========================================================================
  # Input Validation
  # ===========================================================================
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (missing(Vars) || is.null(Vars) || length(Vars) == 0) {
    stop("Vars parameter is required and must be a non-empty character vector")
  }

  if (!is.character(Vars)) {
    stop("Vars must be a character vector of variable names")
  }

  # Check variables exist
  missing_vars <- setdiff(Vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }

  # Check variables are numeric
  non_numeric <- Vars[!sapply(data[Vars], is.numeric)]
  if (length(non_numeric) > 0) {
    stop("Variables must be numeric: ", paste(non_numeric, collapse = ", "))
  }

  # Validate stats
  valid_stats <- c("n", "missing", "missing_pct", "mean", "sd", "se", "var",
                   "median", "min", "max", "range", "iqr", "q1", "q3",
                   "skewness", "kurtosis", "cv")
  invalid_stats <- setdiff(stats, valid_stats)
  if (length(invalid_stats) > 0) {
    stop("Invalid statistics: ", paste(invalid_stats, collapse = ", "),
         "\nValid options: ", paste(valid_stats, collapse = ", "))
  }

  # Validate format
  if (!format %in% c("gt", "data.frame", "kable")) {
    stop("format must be 'gt', 'data.frame', or 'kable'")
  }

  # Set default for bold_highest
  if (is.null(bold_highest)) {
    bold_highest <- compare  # Default to TRUE when compare is TRUE
  }

  # Validate theme
  if (!theme %in% c("default", "fancy", "minimal", "dark", "colorful")) {
    stop("theme must be 'default', 'fancy', 'minimal', 'dark', or 'colorful'")
  }

  # Handle group_by variable
  group_by_str <- NULL
  if (!missing(group_by) && !is.null(substitute(group_by))) {
    group_by_str <- deparse(substitute(group_by))
    if (!group_by_str %in% names(data)) {
      stop("group_by variable '", group_by_str, "' not found in data")
    }
  }

  # ===========================================================================
  # Calculate Statistics
  # ===========================================================================

  # Function to compute stats for a subset
  compute_stats <- function(df, group_label = "Overall") {
    results <- data.frame(Variable = character(), stringsAsFactors = FALSE)

    for (var in Vars) {
      row <- data.frame(Variable = var, stringsAsFactors = FALSE)
      x_full <- df[[var]]
      x <- x_full[!is.na(x_full)]

      for (stat in stats) {
        if (stat == "missing") {
          row[[stat]] <- sum(is.na(x_full))
        } else if (stat == "missing_pct") {
          row[[stat]] <- round(sum(is.na(x_full)) / length(x_full) * 100, 1)
        } else {
          row[[stat]] <- calculate_stat(x, stat, digits)
        }
      }

      results <- rbind(results, row)
    }

    if (!is.null(group_by_str)) {
      results$Group <- group_label
    }

    return(results)
  }

  # Calculate statistics
  if (is.null(group_by_str)) {
    # No grouping
    desc_data <- compute_stats(data)
  } else {
    # With grouping
    groups <- unique(data[[group_by_str]])
    groups <- groups[!is.na(groups)]

    desc_list <- lapply(groups, function(g) {
      subset_data <- data[data[[group_by_str]] == g, ]
      compute_stats(subset_data, group_label = as.character(g))
    })

    desc_data <- do.call(rbind, desc_list)

    # Add overall if requested
    if (overall) {
      overall_stats <- compute_stats(data, group_label = "Overall")
      desc_data <- rbind(desc_data, overall_stats)
    }
  }

  # Apply labels
  if (!is.null(labels)) {
    desc_data$Variable <- sapply(desc_data$Variable, function(v) {
      if (v %in% names(labels)) labels[v] else v
    })
  }

  # ===========================================================================
  # Statistical Comparisons (when compare = TRUE and group_by is specified)
  # ===========================================================================

  test_results <- NULL
  if (compare && !is.null(group_by_str)) {
    groups <- unique(data[[group_by_str]])
    groups <- groups[!is.na(groups)]
    n_groups <- length(groups)

    if (n_groups >= 2) {
      test_results <- data.frame(
        Variable = character(),
        p_value = numeric(),
        effect_size = numeric(),
        test_type = character(),
        stringsAsFactors = FALSE
      )

      for (var in Vars) {
        var_label <- if (!is.null(labels) && var %in% names(labels)) labels[var] else var

        tryCatch({
          if (n_groups == 2) {
            # t-test for 2 groups
            test <- t.test(data[[var]] ~ data[[group_by_str]])
            p_val <- test$p.value

            # Cohen's d effect size
            group_means <- tapply(data[[var]], data[[group_by_str]], mean, na.rm = TRUE)
            group_sds <- tapply(data[[var]], data[[group_by_str]], sd, na.rm = TRUE)
            group_ns <- tapply(data[[var]], data[[group_by_str]], function(x) sum(!is.na(x)))
            pooled_sd <- sqrt(((group_ns[1] - 1) * group_sds[1]^2 + (group_ns[2] - 1) * group_sds[2]^2) / (sum(group_ns) - 2))
            cohens_d <- abs(diff(group_means)) / pooled_sd

            test_results <- rbind(test_results, data.frame(
              Variable = var_label,
              p_value = p_val,
              effect_size = as.numeric(cohens_d),
              test_type = "t-test (Cohen's d)",
              stringsAsFactors = FALSE
            ))
          } else {
            # ANOVA for 3+ groups
            aov_result <- aov(data[[var]] ~ data[[group_by_str]])
            aov_summary <- summary(aov_result)
            p_val <- aov_summary[[1]][["Pr(>F)"]][1]

            # Eta-squared effect size
            ss_between <- aov_summary[[1]][["Sum Sq"]][1]
            ss_total <- sum(aov_summary[[1]][["Sum Sq"]])
            eta_sq <- ss_between / ss_total

            test_results <- rbind(test_results, data.frame(
              Variable = var_label,
              p_value = p_val,
              effect_size = eta_sq,
              test_type = "ANOVA (eta-squared)",
              stringsAsFactors = FALSE
            ))
          }
        }, error = function(e) {
          warning("Could not compute test for variable: ", var, " - ", e$message)
        })
      }
    }
  }

  # ===========================================================================
  # Return Data Frame if Requested
  # ===========================================================================
  if (format == "data.frame") {
    if (!is.null(test_results) && nrow(test_results) > 0) {
      attr(desc_data, "test_results") <- test_results
    }
    return(desc_data)
  }

  # ===========================================================================
  # Create kableExtra Table (HTML with bold highest & colored p-values)
  # ===========================================================================

  if (format == "kable" && !is.null(group_by_str)) {
    # Check for kableExtra
    if (!requireNamespace("kableExtra", quietly = TRUE)) {
      stop("Package 'kableExtra' is required for format = 'kable'. Install with: install.packages('kableExtra')")
    }

    # Build a formatted comparison table similar to user's example
    # Structure: Variable | Group | Mean | SD | N | Effect Size | P-value

    # Get unique variables (excluding Overall rows for now)
    var_list <- unique(desc_data$Variable)

    # Build formatted table
    formatted_rows <- list()

    for (var_name in var_list) {
      var_data <- desc_data[desc_data$Variable == var_name & desc_data$Group != "Overall", ]

      if (nrow(var_data) == 0) next

      # Find highest mean for this variable
      highest_mean <- max(var_data$mean, na.rm = TRUE)

      # Get test results for this variable
      p_val <- NA
      es_val <- NA
      if (!is.null(test_results)) {
        test_row <- test_results[test_results$Variable == var_name, ]
        if (nrow(test_row) > 0) {
          p_val <- test_row$p_value[1]
          es_val <- test_row$effect_size[1]
        }
      }

      for (i in seq_len(nrow(var_data))) {
        row <- var_data[i, ]
        is_highest <- !is.na(row$mean) && row$mean == highest_mean

        # Format values with bold for highest
        mean_fmt <- round(row$mean, digits)
        sd_fmt <- round(row$sd, digits)
        n_fmt <- row$n
        group_fmt <- row$Group

        if (bold_highest && is_highest) {
          mean_fmt <- paste0("<b>", mean_fmt, "</b>")
          sd_fmt <- paste0("<b>", sd_fmt, "</b>")
          n_fmt <- paste0("<b>", n_fmt, "</b>")
          group_fmt <- paste0("<b>", group_fmt, "</b>")
        }

        # Format p-value (only show on first row of each variable)
        p_fmt <- ""
        es_fmt <- ""
        if (i == 1 && !is.na(p_val)) {
          p_fmt <- if (p_val < 0.001) "< 0.001" else as.character(round(p_val, 3))
          es_fmt <- if (!is.na(es_val)) as.character(round(es_val, 3)) else ""
        }

        formatted_rows[[length(formatted_rows) + 1]] <- data.frame(
          Variable = if (i == 1) var_name else "",
          Group = group_fmt,
          Mean = mean_fmt,
          SD = sd_fmt,
          N = n_fmt,
          ES = es_fmt,
          `P-value` = p_fmt,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    }

    kable_df <- do.call(rbind, formatted_rows)

    # Create color vector for p-values
    p_colors <- sapply(kable_df$`P-value`, function(x) {
      if (is.null(sig_color) || is.na(x) || x == "") {
        return("black")
      }
      if (x == "< 0.001") {
        return(sig_color)
      }
      tryCatch({
        if (as.numeric(x) < 0.05) sig_color else "black"
      }, error = function(e) "black")
    })

    # Create kable table
    kable_table <- kableExtra::kable(kable_df, format = "html", escape = FALSE,
                                     align = "c", caption = title) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        full_width = FALSE
      ) %>%
      kableExtra::column_spec(1, bold = TRUE) %>%
      kableExtra::column_spec(ncol(kable_df), color = p_colors)

    return(kable_table)
  }

  # ===========================================================================
  # Create GT Table
  # ===========================================================================

  # Column labels mapping
  stat_labels <- c(
    n = "N",
    missing = "Missing",
    missing_pct = "Missing %",
    mean = "Mean",
    sd = "SD",
    se = "SE",
    var = "Variance",
    median = "Median",
    min = "Min",
    max = "Max",
    range = "Range",
    iqr = "IQR",
    q1 = "Q1",
    q3 = "Q3",
    skewness = "Skewness",
    kurtosis = "Kurtosis",
    cv = "CV %"
  )

  # Reorder columns for grouped data
  if (!is.null(group_by_str)) {
    col_order <- c("Variable", "Group", stats)
    desc_data <- desc_data[, col_order]
  }

  # Add test results columns when compare = TRUE
  if (compare && !is.null(test_results) && nrow(test_results) > 0 && !is.null(group_by_str)) {
    # Merge test results with desc_data
    desc_data <- merge(desc_data, test_results[, c("Variable", "p_value", "effect_size")],
                       by = "Variable", all.x = TRUE)

    # Only show p-value and ES on first row of each variable
    for (var in unique(desc_data$Variable)) {
      var_rows <- which(desc_data$Variable == var)
      if (length(var_rows) > 1) {
        desc_data$p_value[var_rows[-1]] <- NA
        desc_data$effect_size[var_rows[-1]] <- NA
      }
    }
  }

  # Identify rows with highest mean for bold styling
  highest_mean_rows <- c()
  if (bold_highest && !is.null(group_by_str) && "mean" %in% stats) {
    for (var in unique(desc_data$Variable)) {
      var_data <- desc_data[desc_data$Variable == var & desc_data$Group != "Overall", ]
      if (nrow(var_data) > 0) {
        max_mean <- max(var_data$mean, na.rm = TRUE)
        max_rows <- which(desc_data$Variable == var &
                         desc_data$Group != "Overall" &
                         !is.na(desc_data$mean) &
                         desc_data$mean == max_mean)
        highest_mean_rows <- c(highest_mean_rows, max_rows)
      }
    }
  }

  # Create gt table
  gt_table <- gt::gt(desc_data)

  # Add header
  if (!is.null(title)) {
    gt_table <- gt_table %>%
      gt::tab_header(
        title = title,
        subtitle = subtitle
      )
  }

  # Rename columns
  cols_to_rename <- stats[stats %in% names(stat_labels)]
  rename_list <- setNames(
    as.list(stat_labels[cols_to_rename]),
    cols_to_rename
  )

  if (!is.null(group_by_str)) {
    rename_list$Group <- group_by_str
  }
  rename_list$Variable <- "Variable"

  # Add test result column labels if present
  if ("p_value" %in% names(desc_data)) {
    rename_list$p_value <- "P-value"
  }
  if ("effect_size" %in% names(desc_data)) {
    rename_list$effect_size <- "Effect Size"
  }

  gt_table <- gt_table %>%
    gt::cols_label(.list = rename_list)

  # Format numeric columns
  numeric_stats <- setdiff(stats, c("n", "missing"))
  if (length(numeric_stats) > 0) {
    gt_table <- gt_table %>%
      gt::fmt_number(
        columns = dplyr::all_of(numeric_stats),
        decimals = digits
      )
  }

  # Format integer columns
  int_stats <- intersect(stats, c("n", "missing"))
  if (length(int_stats) > 0) {
    gt_table <- gt_table %>%
      gt::fmt_number(
        columns = dplyr::all_of(int_stats),
        decimals = 0
      )
  }

  # Align columns
  gt_table <- gt_table %>%
    gt::cols_align(align = "left", columns = "Variable") %>%
    gt::cols_align(align = "center", columns = dplyr::all_of(stats))

  if (!is.null(group_by_str)) {
    gt_table <- gt_table %>%
      gt::cols_align(align = "center", columns = "Group")
  }

  # Format p_value and effect_size columns if present
  if ("p_value" %in% names(desc_data)) {
    gt_table <- gt_table %>%
      gt::fmt_number(columns = "p_value", decimals = 3) %>%
      gt::cols_align(align = "center", columns = "p_value") %>%
      gt::sub_small_vals(columns = "p_value", threshold = 0.001)
  }

  if ("effect_size" %in% names(desc_data)) {
    gt_table <- gt_table %>%
      gt::fmt_number(columns = "effect_size", decimals = 3) %>%
      gt::cols_align(align = "center", columns = "effect_size")
  }

  # Bold styling for highest mean rows
  if (length(highest_mean_rows) > 0) {
    gt_table <- gt_table %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(rows = highest_mean_rows)
      )
  }

  # Red color for significant p-values
  if (!is.null(sig_color) && "p_value" %in% names(desc_data)) {
    sig_rows <- which(!is.na(desc_data$p_value) & desc_data$p_value < 0.05)
    if (length(sig_rows) > 0) {
      gt_table <- gt_table %>%
        gt::tab_style(
          style = gt::cell_text(color = sig_color),
          locations = gt::cells_body(columns = "p_value", rows = sig_rows)
        )
    }
  }

  # ===========================================================================
  # Apply Theme
  # ===========================================================================

  if (theme == "default") {
    # Clean scientific paper style - black/white, simple borders
    gt_table <- gt_table %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "bold"),
          gt::cell_borders(sides = "top", color = "black", weight = gt::px(2)),
          gt::cell_borders(sides = "bottom", color = "black", weight = gt::px(1))
        ),
        locations = gt::cells_column_labels()
      ) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_title(groups = "title")
      ) %>%
      gt::tab_options(
        table.border.top.width = gt::px(2),
        table.border.top.color = "black",
        table.border.bottom.width = gt::px(2),
        table.border.bottom.color = "black",
        heading.align = "left",
        column_labels.border.bottom.color = "black",
        table_body.border.bottom.color = "black"
      )

  } else if (theme == "fancy") {
    # Blue colorful theme
    gt_table <- gt_table %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#4472C4"),
          gt::cell_text(color = "white", weight = "bold")
        ),
        locations = gt::cells_column_labels()
      ) %>%
      gt::tab_style(
        style = gt::cell_fill(color = "#D6DCE5"),
        locations = gt::cells_body(rows = seq(1, nrow(desc_data), 2))
      ) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_title(groups = "title")
      ) %>%
      gt::tab_options(
        table.border.top.width = gt::px(2),
        table.border.top.color = "#4472C4",
        table.border.bottom.width = gt::px(2),
        table.border.bottom.color = "#4472C4",
        heading.align = "left"
      )

  } else if (theme == "minimal") {
    gt_table <- gt_table %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "bold"),
          gt::cell_borders(sides = "bottom", color = "#333333", weight = gt::px(2))
        ),
        locations = gt::cells_column_labels()
      ) %>%
      gt::tab_options(
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden",
        heading.align = "left"
      )

  } else if (theme == "dark") {
    gt_table <- gt_table %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#1a1a2e"),
          gt::cell_text(color = "#eaeaea", weight = "bold")
        ),
        locations = gt::cells_column_labels()
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#16213e"),
          gt::cell_text(color = "#eaeaea")
        ),
        locations = gt::cells_body(rows = seq(1, nrow(desc_data), 2))
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#1a1a2e"),
          gt::cell_text(color = "#eaeaea")
        ),
        locations = gt::cells_body(rows = seq(2, nrow(desc_data), 2))
      ) %>%
      gt::tab_style(
        style = gt::cell_text(color = "#eaeaea", weight = "bold"),
        locations = gt::cells_title(groups = "title")
      ) %>%
      gt::tab_options(
        table.background.color = "#0f0f23",
        heading.align = "left"
      )

  } else if (theme == "colorful") {
    gt_table <- gt_table %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#6C63FF"),
          gt::cell_text(color = "white", weight = "bold")
        ),
        locations = gt::cells_column_labels()
      ) %>%
      gt::tab_style(
        style = gt::cell_fill(color = "#F0EFFF"),
        locations = gt::cells_body(rows = seq(1, nrow(desc_data), 2))
      ) %>%
      gt::tab_style(
        style = gt::cell_fill(color = "#E8E6FF"),
        locations = gt::cells_body(rows = seq(2, nrow(desc_data), 2))
      ) %>%
      gt::tab_style(
        style = gt::cell_text(color = "#6C63FF", weight = "bold"),
        locations = gt::cells_title(groups = "title")
      ) %>%
      gt::tab_options(
        table.border.top.width = gt::px(3),
        table.border.top.color = "#6C63FF",
        table.border.bottom.width = gt::px(3),
        table.border.bottom.color = "#6C63FF",
        heading.align = "left"
      )
  }

  # Highlight "Overall" row if present
  if (!is.null(group_by_str) && overall) {
    overall_rows <- which(desc_data$Group == "Overall")
    if (length(overall_rows) > 0) {
      gt_table <- gt_table %>%
        gt::tab_style(
          style = list(
            gt::cell_text(weight = "bold"),
            gt::cell_fill(color = "#FFF3CD")
          ),
          locations = gt::cells_body(rows = overall_rows)
        )
    }
  }

  return(gt_table)
}


# =============================================================================
# CATEGORICAL TABLE FUNCTION
# =============================================================================

#' Generate Publication-Ready Frequency Tables for Categorical Variables
#'
#' @description
#' Creates comprehensive frequency tables for categorical variables with counts,
#' percentages, and optional cross-tabulation. Supports chi-square tests for
#' associations and outputs beautifully formatted gt tables.
#'
#' @param data A data frame containing the variables to summarize.
#' @param var The unquoted name of the primary categorical variable.
#' @param by Optional. The unquoted name of a second categorical variable for
#'   cross-tabulation. When provided, creates a contingency table.
#' @param group_by Optional. The unquoted name of a stratification variable.
#'   Creates separate tables for each level of this variable.
#' @param percentages Character vector specifying which percentages to include.
#'   Options: `"col"` (column), `"row"`, `"total"`, `"none"`.
#'   Default: `c("col")` for cross-tabs, `c("total")` for single variable.
#' @param show_n Logical. Show frequency counts? Default: `TRUE`.
#' @param show_total Logical. Include row/column totals? Default: `TRUE`.
#' @param show_missing Logical. Include missing values as a category?
#'   Default: `FALSE`.
#' @param chi_square Logical. Compute chi-square test for cross-tabulations?
#'   Default: `TRUE`.
#' @param fisher Logical. Also compute Fisher's exact test (for small samples)?
#'   Default: `FALSE`.
#' @param cramers_v Logical. Compute Cramer's V effect size? Default: `TRUE`.
#' @param digits Integer. Decimal places for percentages. Default: `1`.
#' @param sort_by How to sort categories: `"none"` (original order),
#'   `"frequency"` (descending), `"alphabetical"`. Default: `"none"`.
#' @param labels Optional named character vector for category labels.
#' @param title Optional character string for table title.
#' @param subtitle Optional character string for table subtitle.
#' @param format Character. Output format: `"gt"` (default) or `"data.frame"`.
#' @param theme Character. Visual theme: `"default"`, `"minimal"`, `"dark"`,
#'   `"colorful"`. Default: `"default"`.
#' @param combine Logical. For single variables, combine n and % in one column?
#'   Default: `TRUE`. Shows as "n (%)".
#'
#' @return A gt table object (default) or data frame with frequencies and percentages.
#' For cross-tabulations with chi_square = TRUE, includes test statistics.
#'
#' @examples
#' \dontrun{
#' # ============================================================
#' # EXAMPLE 1: Single Variable Frequency Table
#' # ============================================================
#' data <- data.frame(
#'   gender = sample(c("Male", "Female", "Other"), 200, replace = TRUE,
#'                   prob = c(0.45, 0.45, 0.10)),
#'   education = sample(c("High School", "Bachelor", "Master", "PhD"), 200,
#'                      replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)),
#'   country = sample(c("USA", "UK", "Germany"), 200, replace = TRUE)
#' )
#'
#' # Basic frequency table
#' categorical_table(data, var = gender)
#'
#' # With sorting by frequency
#' categorical_table(data, var = education, sort_by = "frequency")
#'
#' # ============================================================
#' # EXAMPLE 2: Cross-Tabulation (Two Variables)
#' # ============================================================
#' categorical_table(
#'   data = data,
#'   var = gender,
#'   by = education,
#'   chi_square = TRUE,
#'   cramers_v = TRUE
#' )
#'
#' # ============================================================
#' # EXAMPLE 3: Stratified Analysis
#' # ============================================================
#' categorical_table(
#'   data = data,
#'   var = gender,
#'   by = education,
#'   group_by = country
#' )
#'
#' # ============================================================
#' # EXAMPLE 4: Custom Labels and Formatting
#' # ============================================================
#' categorical_table(
#'   data = data,
#'   var = education,
#'   labels = c(
#'     "High School" = "Secondary Education",
#'     "Bachelor" = "Bachelor's Degree",
#'     "Master" = "Master's Degree",
#'     "PhD" = "Doctorate"
#'   ),
#'   title = "Educational Attainment",
#'   subtitle = "Sample Distribution",
#'   theme = "colorful"
#' )
#'
#' # ============================================================
#' # EXAMPLE 5: Show Row and Column Percentages
#' # ============================================================
#' categorical_table(
#'   data = data,
#'   var = gender,
#'   by = education,
#'   percentages = c("row", "col"),
#'   combine = FALSE
#' )
#' }
#'
#' @seealso
#' \code{\link{descriptive_table}} for numeric variable summaries
#' \code{\link{Mosaic}} for mosaic plot visualization with chi-square tests
#'
#' @importFrom dplyr group_by summarise n mutate arrange desc filter
#' @importFrom stats chisq.test fisher.test
#' @importFrom gt gt tab_header cols_label cols_align fmt_number tab_style cell_fill cell_text cells_column_labels cells_body tab_footnote cells_title tab_options
#'
#' @export
categorical_table <- function(data,
                              var,
                              by = NULL,
                              group_by = NULL,
                              percentages = NULL,
                              show_n = TRUE,
                              show_total = TRUE,
                              show_missing = FALSE,
                              chi_square = TRUE,
                              fisher = FALSE,
                              cramers_v = TRUE,
                              digits = 1,
                              sort_by = "none",
                              labels = NULL,
                              title = NULL,
                              subtitle = NULL,
                              format = "gt",
                              theme = "default",
                              combine = TRUE) {

  # ===========================================================================
  # Input Validation
  # ===========================================================================
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # Handle variable names

  var_str <- deparse(substitute(var))
  if (!var_str %in% names(data)) {
    stop("Variable '", var_str, "' not found in data")
  }

  by_str <- NULL
  if (!missing(by) && !is.null(substitute(by))) {
    by_str <- deparse(substitute(by))
    if (!by_str %in% names(data)) {
      stop("Variable '", by_str, "' not found in data")
    }
  }

  group_by_str <- NULL
  if (!missing(group_by) && !is.null(substitute(group_by))) {
    group_by_str <- deparse(substitute(group_by))
    if (!group_by_str %in% names(data)) {
      stop("Variable '", group_by_str, "' not found in data")
    }
  }

  # Set default percentages
  if (is.null(percentages)) {
    percentages <- if (is.null(by_str)) "total" else "col"
  }

  # Validate inputs
  if (!format %in% c("gt", "data.frame")) {
    stop("format must be 'gt' or 'data.frame'")
  }

  if (!theme %in% c("default", "fancy", "minimal", "dark", "colorful")) {
    stop("theme must be 'default', 'fancy', 'minimal', 'dark', or 'colorful'")
  }

  if (!sort_by %in% c("none", "frequency", "alphabetical")) {
    stop("sort_by must be 'none', 'frequency', or 'alphabetical'")
  }

  # ===========================================================================
  # Helper Functions
  # ===========================================================================

  # Calculate Cramer's V
  calc_cramers_v <- function(chi_result, n, min_dim) {
    sqrt(chi_result$statistic / (n * (min_dim - 1)))
  }

  # Interpret Cramer's V
  interpret_cramers_v <- function(v) {
    if (is.na(v)) return("NA")
    if (v < 0.10) return("negligible")
    if (v < 0.20) return("small")
    if (v < 0.40) return("medium")
    return("large")
  }

  # Format percentage
  format_pct <- function(x, digits) {
    sprintf(paste0("%.", digits, "f%%"), x)
  }

  # Apply labels to a vector
  apply_labels <- function(x, labels) {
    if (is.null(labels)) return(x)
    sapply(as.character(x), function(v) {
      if (v %in% names(labels)) labels[v] else v
    }, USE.NAMES = FALSE)
  }

  # ===========================================================================
  # Process Data
  # ===========================================================================

  # Handle missing values
  work_data <- data
  if (!show_missing) {
    work_data <- work_data[!is.na(work_data[[var_str]]), ]
    if (!is.null(by_str)) {
      work_data <- work_data[!is.na(work_data[[by_str]]), ]
    }
  } else {
    # Convert NA to "Missing" category
    work_data[[var_str]] <- ifelse(is.na(work_data[[var_str]]), "(Missing)", as.character(work_data[[var_str]]))
    if (!is.null(by_str)) {
      work_data[[by_str]] <- ifelse(is.na(work_data[[by_str]]), "(Missing)", as.character(work_data[[by_str]]))
    }
  }

  # Convert to factors if needed
  work_data[[var_str]] <- as.factor(work_data[[var_str]])
  if (!is.null(by_str)) {
    work_data[[by_str]] <- as.factor(work_data[[by_str]])
  }

  # ===========================================================================
  # Single Variable Frequency Table
  # ===========================================================================

  if (is.null(by_str)) {
    # Calculate frequencies
    freq_data <- work_data %>%
      dplyr::group_by(.data[[var_str]]) %>%
      dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>%
      dplyr::mutate(
        pct = n / sum(n) * 100,
        cum_n = cumsum(n),
        cum_pct = cumsum(pct)
      )

    # Sort if requested
    if (sort_by == "frequency") {
      freq_data <- freq_data %>% dplyr::arrange(dplyr::desc(n))
      freq_data$cum_n <- cumsum(freq_data$n)
      freq_data$cum_pct <- cumsum(freq_data$pct)
    } else if (sort_by == "alphabetical") {
      freq_data <- freq_data %>% dplyr::arrange(.data[[var_str]])
      freq_data$cum_n <- cumsum(freq_data$n)
      freq_data$cum_pct <- cumsum(freq_data$pct)
    }

    # Apply labels
    freq_data[[var_str]] <- apply_labels(freq_data[[var_str]], labels)

    # Add total row
    if (show_total) {
      total_row <- data.frame(
        var_str = "Total",
        n = sum(freq_data$n),
        pct = 100,
        cum_n = sum(freq_data$n),
        cum_pct = 100
      )
      names(total_row)[1] <- var_str
      freq_data <- rbind(freq_data, total_row)
    }

    # Combine n and % if requested
    if (combine && show_n) {
      freq_data$`n (%)` <- paste0(freq_data$n, " (", format_pct(freq_data$pct, digits), ")")
    }

    # Select columns for output
    if (format == "data.frame") {
      return(freq_data)
    }

    # Build GT table
    if (combine && show_n) {
      result_df <- freq_data[, c(var_str, "n (%)", "cum_pct")]
      names(result_df) <- c("Category", "n (%)", "Cumulative %")
    } else {
      result_df <- freq_data[, c(var_str, "n", "pct", "cum_pct")]
      names(result_df) <- c("Category", "N", "%", "Cumulative %")
    }

    gt_table <- gt::gt(result_df)

    # Default title
    if (is.null(title)) {
      title <- paste("Frequency Table:", var_str)
    }

  } else {
    # ===========================================================================
    # Cross-Tabulation (Two Variables)
    # ===========================================================================

    # Create contingency table
    cont_table <- table(work_data[[var_str]], work_data[[by_str]])

    # Run statistical tests
    test_results <- NULL
    if (chi_square) {
      test_results <- tryCatch({
        chi_res <- stats::chisq.test(cont_table)
        n_obs <- sum(cont_table)
        min_dim <- min(nrow(cont_table), ncol(cont_table))
        v <- as.numeric(calc_cramers_v(chi_res, n_obs, min_dim))

        list(
          chi_sq = chi_res$statistic,
          df = chi_res$parameter,
          p_value = chi_res$p.value,
          cramers_v = v,
          v_interp = interpret_cramers_v(v),
          fisher_p = if (fisher) tryCatch(stats::fisher.test(cont_table, simulate.p.value = TRUE)$p.value, error = function(e) NA) else NA
        )
      }, error = function(e) NULL)
    }

    # Calculate percentages
    row_totals <- rowSums(cont_table)
    col_totals <- colSums(cont_table)
    grand_total <- sum(cont_table)

    # Build output data frame
    row_names <- rownames(cont_table)
    col_names <- colnames(cont_table)

    # Apply labels
    row_names_display <- apply_labels(row_names, labels)

    # Create base data frame with counts
    result_df <- as.data.frame.matrix(cont_table)
    result_df <- cbind(Category = row_names_display, result_df)

    # Add Total column if requested
    if (show_total) {
      result_df$Total <- row_totals
    }

    # Add percentages based on selection
    if (!combine) {
      # Create separate percentage columns
      if ("col" %in% percentages) {
        col_pct_df <- as.data.frame.matrix(sweep(cont_table, 2, col_totals, "/") * 100)
        names(col_pct_df) <- paste0(names(col_pct_df), " (%col)")
        result_df <- cbind(result_df, col_pct_df)
      }
      if ("row" %in% percentages) {
        row_pct_df <- as.data.frame.matrix(sweep(cont_table, 1, row_totals, "/") * 100)
        names(row_pct_df) <- paste0(names(row_pct_df), " (%row)")
        result_df <- cbind(result_df, row_pct_df)
      }
    } else {
      # Combine n (%) in cells
      pct_matrix <- if ("col" %in% percentages) {
        sweep(cont_table, 2, col_totals, "/") * 100
      } else if ("row" %in% percentages) {
        sweep(cont_table, 1, row_totals, "/") * 100
      } else {
        cont_table / grand_total * 100
      }

      combined_df <- data.frame(Category = row_names_display)
      for (j in seq_along(col_names)) {
        combined_df[[col_names[j]]] <- paste0(
          cont_table[, j], " (", format_pct(pct_matrix[, j], digits), ")"
        )
      }
      if (show_total) {
        total_pct <- if ("row" %in% percentages) {
          rep(100, length(row_totals))
        } else {
          row_totals / grand_total * 100
        }
        combined_df$Total <- paste0(row_totals, " (", format_pct(total_pct, digits), ")")
      }
      result_df <- combined_df
    }

    # Add Total row
    if (show_total) {
      if (combine) {
        total_row <- data.frame(Category = "Total")
        for (j in seq_along(col_names)) {
          col_pct <- if ("col" %in% percentages) 100 else col_totals[j] / grand_total * 100
          total_row[[col_names[j]]] <- paste0(col_totals[j], " (", format_pct(col_pct, digits), ")")
        }
        total_row$Total <- paste0(grand_total, " (100.0%)")
      } else {
        total_row <- c("Total", col_totals, grand_total)
        if ("col" %in% percentages) {
          total_row <- c(total_row, rep(100, length(col_totals)))
        }
        if ("row" %in% percentages) {
          total_row <- c(total_row, col_totals / grand_total * 100)
        }
        total_row <- as.data.frame(t(total_row))
        names(total_row) <- names(result_df)
      }
      result_df <- rbind(result_df, total_row)
    }

    if (format == "data.frame") {
      attr(result_df, "test_results") <- test_results
      return(result_df)
    }

    # Build GT table
    gt_table <- gt::gt(result_df)

    # Add spanner for the by variable
    gt_table <- gt_table %>%
      gt::tab_spanner(
        label = by_str,
        columns = col_names
      )

    # Default title
    if (is.null(title)) {
      title <- paste(var_str, "by", by_str)
    }

    # Add test results footnote
    if (!is.null(test_results) && chi_square) {
      footnote_text <- sprintf(
        "Chi-square(%.0f) = %.2f, p %s",
        test_results$df,
        test_results$chi_sq,
        if (test_results$p_value < 0.001) "< .001" else sprintf("= %.3f", test_results$p_value)
      )
      if (cramers_v && !is.na(test_results$cramers_v)) {
        footnote_text <- paste0(footnote_text,
                               sprintf("; Cramer's V = %.3f (%s effect)",
                                      test_results$cramers_v, test_results$v_interp))
      }
      if (fisher && !is.na(test_results$fisher_p)) {
        footnote_text <- paste0(footnote_text,
                               sprintf("; Fisher's p %s",
                                      if (test_results$fisher_p < 0.001) "< .001" else sprintf("= %.3f", test_results$fisher_p)))
      }

      gt_table <- gt_table %>%
        gt::tab_footnote(
          footnote = footnote_text,
          locations = gt::cells_title(groups = "title")
        )
    }
  }

  # ===========================================================================
  # Apply Header and Theme
  # ===========================================================================

  gt_table <- gt_table %>%
    gt::tab_header(
      title = title,
      subtitle = subtitle
    )

  # Rename Category column
  gt_table <- gt_table %>%
    gt::cols_label(Category = var_str) %>%
    gt::cols_align(align = "left", columns = "Category")

  # Apply theme
  if (theme == "default") {
    # Clean scientific paper style
    gt_table <- gt_table %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "bold"),
          gt::cell_borders(sides = "top", color = "black", weight = gt::px(2)),
          gt::cell_borders(sides = "bottom", color = "black", weight = gt::px(1))
        ),
        locations = gt::cells_column_labels()
      ) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_title(groups = "title")
      ) %>%
      gt::tab_options(
        table.border.top.width = gt::px(2),
        table.border.top.color = "black",
        table.border.bottom.width = gt::px(2),
        table.border.bottom.color = "black",
        heading.align = "left",
        column_labels.border.bottom.color = "black",
        table_body.border.bottom.color = "black"
      )

    # Highlight Total row
    if (show_total) {
      n_rows <- nrow(result_df)
      gt_table <- gt_table %>%
        gt::tab_style(
          style = gt::cell_text(weight = "bold"),
          locations = gt::cells_body(rows = n_rows)
        )
    }

  } else if (theme == "fancy") {
    # Blue colorful theme
    gt_table <- gt_table %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#4472C4"),
          gt::cell_text(color = "white", weight = "bold")
        ),
        locations = gt::cells_column_labels()
      ) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_title(groups = "title")
      ) %>%
      gt::tab_options(
        table.border.top.width = gt::px(2),
        table.border.top.color = "#4472C4",
        table.border.bottom.width = gt::px(2),
        table.border.bottom.color = "#4472C4",
        heading.align = "left"
      )

    # Highlight Total row
    if (show_total) {
      n_rows <- nrow(result_df)
      gt_table <- gt_table %>%
        gt::tab_style(
          style = list(
            gt::cell_text(weight = "bold"),
            gt::cell_fill(color = "#D6DCE5")
          ),
          locations = gt::cells_body(rows = n_rows)
        )
    }

  } else if (theme == "minimal") {
    gt_table <- gt_table %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "bold"),
          gt::cell_borders(sides = "bottom", color = "#333333", weight = gt::px(2))
        ),
        locations = gt::cells_column_labels()
      ) %>%
      gt::tab_options(
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden",
        heading.align = "left"
      )

  } else if (theme == "dark") {
    gt_table <- gt_table %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#1a1a2e"),
          gt::cell_text(color = "#eaeaea", weight = "bold")
        ),
        locations = gt::cells_column_labels()
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#16213e"),
          gt::cell_text(color = "#eaeaea")
        ),
        locations = gt::cells_body()
      ) %>%
      gt::tab_style(
        style = gt::cell_text(color = "#eaeaea", weight = "bold"),
        locations = gt::cells_title(groups = "title")
      ) %>%
      gt::tab_options(
        table.background.color = "#0f0f23",
        heading.align = "left"
      )

  } else if (theme == "colorful") {
    gt_table <- gt_table %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#6C63FF"),
          gt::cell_text(color = "white", weight = "bold")
        ),
        locations = gt::cells_column_labels()
      ) %>%
      gt::tab_style(
        style = gt::cell_text(color = "#6C63FF", weight = "bold"),
        locations = gt::cells_title(groups = "title")
      ) %>%
      gt::tab_options(
        table.border.top.width = gt::px(3),
        table.border.top.color = "#6C63FF",
        table.border.bottom.width = gt::px(3),
        table.border.bottom.color = "#6C63FF",
        heading.align = "left"
      )

    if (show_total) {
      n_rows <- nrow(result_df)
      gt_table <- gt_table %>%
        gt::tab_style(
          style = list(
            gt::cell_text(weight = "bold"),
            gt::cell_fill(color = "#E8E6FF")
          ),
          locations = gt::cells_body(rows = n_rows)
        )
    }
  }

  return(gt_table)
}


# =============================================================================
# AUTO DESCRIBE FUNCTION
# =============================================================================

#' Automatically Generate Descriptive Statistics for All Variables
#'
#' @description
#' Automatically detects numeric and categorical variables in a data frame
#' and generates appropriate descriptive statistics tables for each type.
#' Numeric variables get summary statistics (mean, SD, etc.) and categorical
#' variables get frequency tables.
#'
#' @param data A data frame to describe.
#' @param group_by Optional. Unquoted name of a grouping variable for
#'   stratified statistics.
#' @param numeric_stats Character vector of statistics for numeric variables.
#'   Default: `c("n", "mean", "sd", "median", "min", "max")`.
#' @param digits Integer. Decimal places for numeric output. Default: `2`.
#' @param max_categories Integer. Maximum unique values for a numeric variable
#'   to be auto-classified as categorical. Default: `10`.
#' @param exclude Character vector of column names to exclude. Default: `NULL`.
#' @param force_numeric Character vector of column names to force-treat as numeric,
#'   regardless of unique value count. Useful for Likert scales, ratings, etc.
#'   Default: `NULL`.
#' @param force_categorical Character vector of column names to force-treat as
#'   categorical, even if numeric. Default: `NULL`.
#' @param title_numeric Title for numeric table. Default: `"Numeric Variables"`.
#' @param title_categorical Title for categorical tables. Default: `"Categorical Variables"`.
#' @param theme Visual theme: `"default"`, `"minimal"`, `"dark"`, `"colorful"`.
#'   Default: `"default"`.
#' @param format Output format: `"gt"` or `"data.frame"`. Default: `"gt"`.
#' @param print Logical. Print tables to console? Default: `TRUE`.
#'
#' @return A list with:
#' \itemize{
#'   \item numeric: Descriptive table for numeric variables (or NULL if none)
#'   \item categorical: List of frequency tables for categorical variables
#'   \item variable_types: Data frame showing detected variable types
#' }
#'
#' @examples
#' \dontrun{
#' # Automatic description of all variables
#' data <- data.frame(
#'   age = rnorm(100, 35, 10),
#'   score = rnorm(100, 75, 15),
#'   gender = sample(c("M", "F"), 100, replace = TRUE),
#'   education = sample(c("HS", "BA", "MA", "PhD"), 100, replace = TRUE)
#' )
#'
#' # Describe all variables automatically
#' results <- auto_describe(data)
#'
#' # With grouping
#' results <- auto_describe(data, group_by = gender)
#'
#' # Access individual tables
#' results$numeric
#' results$categorical$education
#' }
#'
#' @export
auto_describe <- function(data,
                          group_by = NULL,
                          numeric_stats = c("n", "mean", "sd", "median", "min", "max"),
                          digits = 2,
                          max_categories = 10,
                          exclude = NULL,
                          force_numeric = NULL,
                          force_categorical = NULL,
                          title_numeric = "Numeric Variables",
                          title_categorical = "Categorical Variables",
                          theme = "default",
                          format = "gt",
                          print = TRUE) {

  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # Handle group_by
  group_by_str <- NULL
  if (!missing(group_by) && !is.null(substitute(group_by))) {
    group_by_str <- deparse(substitute(group_by))
    if (!group_by_str %in% names(data)) {
      stop("group_by variable '", group_by_str, "' not found in data")
    }
  }

  # Exclude specified columns and group_by column from analysis
  cols_to_analyze <- setdiff(names(data), c(exclude, group_by_str))

  # Detect variable types
  var_types <- sapply(cols_to_analyze, function(col_name) {
    # Check forced classifications first
    if (!is.null(force_numeric) && col_name %in% force_numeric) {
      return("numeric")
    }
    if (!is.null(force_categorical) && col_name %in% force_categorical) {
      return("categorical")
    }

    x <- data[[col_name]]
    if (is.numeric(x)) {
      n_unique <- length(unique(x[!is.na(x)]))
      if (n_unique <= max_categories) {
        return("categorical")
      }
      return("numeric")
    } else if (is.factor(x) || is.character(x) || is.logical(x)) {
      return("categorical")
    } else {
      return("other")
    }
  })

  numeric_vars <- names(var_types[var_types == "numeric"])
  categorical_vars <- names(var_types[var_types == "categorical"])

  # Create variable types summary
  var_types_df <- data.frame(
    Variable = names(var_types),
    Type = as.character(var_types),
    stringsAsFactors = FALSE
  )

  if (print) {
    cat("=== Variable Types Detected ===\n")
    cat("Numeric variables:", length(numeric_vars), "\n")
    cat("Categorical variables:", length(categorical_vars), "\n\n")
  }

  # Initialize results
  results <- list(
    numeric = NULL,
    categorical = list(),
    variable_types = var_types_df
  )

  # Generate numeric descriptives
  if (length(numeric_vars) > 0) {
    if (print) cat("=== Numeric Variables ===\n")

    if (is.null(group_by_str)) {
      results$numeric <- descriptive_table(
        data = data,
        Vars = numeric_vars,
        stats = numeric_stats,
        digits = digits,
        title = title_numeric,
        theme = theme,
        format = format
      )
    } else {
      results$numeric <- descriptive_table(
        data = data,
        Vars = numeric_vars,
        group_by = !!rlang::sym(group_by_str),
        stats = numeric_stats,
        digits = digits,
        title = title_numeric,
        theme = theme,
        format = format
      )
    }

    if (print && format == "gt") {
      print(results$numeric)
      cat("\n")
    }
  } else {
    if (print) cat("No numeric variables found.\n\n")
  }

  # Generate categorical descriptives - combined into one table
  if (length(categorical_vars) > 0) {
    if (print) cat("=== Categorical Variables ===\n")

    # Build combined categorical data frame
    all_cat_data <- data.frame()

    for (cat_var in categorical_vars) {
      # Calculate frequencies for this variable
      var_data <- data[[cat_var]]
      freq_table <- table(var_data, useNA = "no")
      total_n <- sum(freq_table)

      # Find the mode (most frequent category)
      mode_category <- names(freq_table)[which.max(freq_table)]

      # Create data frame for this variable
      var_df <- data.frame(
        Variable = c(cat_var, rep("", length(freq_table) - 1)),
        Category = names(freq_table),
        n = as.integer(freq_table),
        pct = round(as.numeric(freq_table) / total_n * 100, 1),
        stringsAsFactors = FALSE
      )

      # Mark the mode (most frequent)
      var_df$is_mode <- var_df$Category == mode_category

      all_cat_data <- rbind(all_cat_data, var_df)
    }

    # Store raw data
    results$categorical <- all_cat_data

    if (format == "data.frame") {
      if (print) {
        print(all_cat_data[, c("Variable", "Category", "n", "pct")])
      }
    } else {
      # Create gt table
      display_data <- all_cat_data
      display_data$`n (%)` <- paste0(display_data$n, " (", display_data$pct, "%)")

      gt_cat <- gt::gt(display_data[, c("Variable", "Category", "n (%)")]) %>%
        gt::tab_header(
          title = title_categorical
        ) %>%
        gt::cols_align(align = "left", columns = "Variable") %>%
        gt::cols_align(align = "left", columns = "Category") %>%
        gt::cols_align(align = "center", columns = "n (%)")

      # Bold the mode (most frequent) rows
      mode_rows <- which(all_cat_data$is_mode)
      if (length(mode_rows) > 0) {
        gt_cat <- gt_cat %>%
          gt::tab_style(
            style = gt::cell_text(weight = "bold"),
            locations = gt::cells_body(rows = mode_rows)
          )
      }

      # Apply theme
      if (theme == "default") {
        # Clean scientific paper style
        gt_cat <- gt_cat %>%
          gt::tab_style(
            style = list(
              gt::cell_text(weight = "bold"),
              gt::cell_borders(sides = "top", color = "black", weight = gt::px(2)),
              gt::cell_borders(sides = "bottom", color = "black", weight = gt::px(1))
            ),
            locations = gt::cells_column_labels()
          ) %>%
          gt::tab_style(
            style = gt::cell_text(weight = "bold"),
            locations = gt::cells_title(groups = "title")
          ) %>%
          gt::tab_options(
            table.border.top.width = gt::px(2),
            table.border.top.color = "black",
            table.border.bottom.width = gt::px(2),
            table.border.bottom.color = "black",
            column_labels.border.bottom.color = "black",
            table_body.border.bottom.color = "black"
          )
      } else if (theme == "fancy") {
        # Blue colorful theme
        gt_cat <- gt_cat %>%
          gt::tab_style(
            style = list(
              gt::cell_fill(color = "#4472C4"),
              gt::cell_text(color = "white", weight = "bold")
            ),
            locations = gt::cells_column_labels()
          ) %>%
          gt::tab_style(
            style = gt::cell_text(weight = "bold"),
            locations = gt::cells_title(groups = "title")
          ) %>%
          gt::tab_options(
            table.border.top.width = gt::px(2),
            table.border.top.color = "#4472C4",
            table.border.bottom.width = gt::px(2),
            table.border.bottom.color = "#4472C4"
          )
      } else if (theme == "colorful") {
        gt_cat <- gt_cat %>%
          gt::tab_style(
            style = list(
              gt::cell_fill(color = "#6C63FF"),
              gt::cell_text(color = "white", weight = "bold")
            ),
            locations = gt::cells_column_labels()
          ) %>%
          gt::tab_style(
            style = gt::cell_text(color = "#6C63FF", weight = "bold"),
            locations = gt::cells_title(groups = "title")
          )
      }

      # Add footnote about bold
      gt_cat <- gt_cat %>%
        gt::tab_footnote(
          footnote = "Bold = most frequent category (mode)",
          locations = gt::cells_title(groups = "title")
        )

      results$categorical_table <- gt_cat

      if (print) {
        print(gt_cat)
      }
    }
  } else {
    if (print) cat("No categorical variables found.\n")
  }

  invisible(results)
}
