#' Quick Dataset Overview
#'
#' @description
#' Provides a comprehensive snapshot of a dataset including variable types,
#' missing data percentages, unique values, and basic statistics.
#'
#' @param data A data frame to summarize.
#' @param Vars Character vector of variable names. If NULL (default), all variables are included.
#' @param max_levels Integer. For categorical variables, show level counts if <= this value. Default 10.
#' @param digits Integer. Number of decimal places for numeric summaries. Default 2.
#'
#' @return A gt table with dataset overview.
#'
#' @examples
#' \dontrun{
#' data_overview(mtcars)
#' data_overview(iris, Vars = c("Sepal.Length", "Species"))
#' }
#'
#' @importFrom gt gt tab_header cols_align tab_style cell_text cells_body tab_source_note
#' @importFrom stats sd median
#' @export
data_overview <- function(data,
                          Vars = NULL,
                          max_levels = 10,
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

  n_total <- nrow(data)

  # Build summary for each variable
  summary_list <- lapply(Vars, function(var_name) {
    x <- data[[var_name]]
    n_missing <- sum(is.na(x))
    n_valid <- n_total - n_missing
    pct_missing <- round(n_missing / n_total * 100, 1)
    n_unique <- length(unique(na.omit(x)))

    # Determine type
    if (is.numeric(x)) {
      var_type <- "numeric"
      x_clean <- na.omit(x)
      if (length(x_clean) > 0) {
        mean_val <- round(mean(x_clean), digits)
        sd_val <- round(sd(x_clean), digits)
        min_val <- round(min(x_clean), digits)
        max_val <- round(max(x_clean), digits)
        median_val <- round(median(x_clean), digits)
        central <- paste0(mean_val, " (", sd_val, ")")
        range_str <- paste0("[", min_val, ", ", max_val, "]")
      } else {
        central <- "—"
        range_str <- "—"
        median_val <- NA
      }
      levels_str <- "—"
    } else if (is.factor(x) || is.character(x)) {
      var_type <- if (is.factor(x)) "factor" else "character"
      x_clean <- na.omit(x)
      if (length(x_clean) > 0) {
        freq_table <- sort(table(x_clean), decreasing = TRUE)
        mode_val <- names(freq_table)[1]
        mode_freq <- freq_table[1]
        central <- paste0(mode_val, " (n=", mode_freq, ")")

        if (n_unique <= max_levels) {
          levels_str <- paste(names(freq_table), collapse = ", ")
        } else {
          levels_str <- paste0(n_unique, " levels")
        }
      } else {
        central <- "—"
        levels_str <- "—"
      }
      range_str <- "—"
      median_val <- NA
    } else if (is.logical(x)) {
      var_type <- "logical"
      x_clean <- na.omit(x)
      if (length(x_clean) > 0) {
        n_true <- sum(x_clean)
        pct_true <- round(n_true / length(x_clean) * 100, 1)
        central <- paste0("TRUE: ", pct_true, "%")
      } else {
        central <- "—"
      }
      range_str <- "—"
      levels_str <- "TRUE, FALSE"
      median_val <- NA
    } else if (inherits(x, "Date") || inherits(x, "POSIXct")) {
      var_type <- "date"
      x_clean <- na.omit(x)
      if (length(x_clean) > 0) {
        min_date <- min(x_clean)
        max_date <- max(x_clean)
        central <- as.character(median(x_clean))
        range_str <- paste0("[", min_date, ", ", max_date, "]")
      } else {
        central <- "—"
        range_str <- "—"
      }
      levels_str <- "—"
      median_val <- NA
    } else {
      var_type <- class(x)[1]
      central <- "—"
      range_str <- "—"
      levels_str <- "—"
      median_val <- NA
    }

    data.frame(
      Variable = var_name,
      Type = var_type,
      N = n_valid,
      Missing = paste0(n_missing, " (", pct_missing, "%)"),
      Unique = n_unique,
      `Mean/Mode (SD/n)` = central,
      Range = range_str,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  summary_df <- do.call(rbind, summary_list)

  # Create gt table
  gt_table <- gt::gt(summary_df) |>
    gt::tab_header(
      title = "Data Overview",
      subtitle = paste0("N = ", n_total, " observations, ", length(Vars), " variables")
    ) |>
    gt::cols_align(align = "left", columns = c("Variable", "Type")) |>
    gt::cols_align(align = "center", columns = c("N", "Missing", "Unique")) |>
    gt::cols_align(align = "right", columns = c("Mean/Mode (SD/n)", "Range")) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = "Variable")
    ) |>
    gt::tab_source_note(
      source_note = "Mean/Mode (SD/n): Mean (SD) for numeric, Mode (count) for categorical"
    )

  # Highlight rows with missing data
  missing_rows <- which(summary_df$Missing != "0 (0%)")
  if (length(missing_rows) > 0) {
    gt_table <- gt_table |>
      gt::tab_style(
        style = gt::cell_text(color = "#E74C3C"),
        locations = gt::cells_body(columns = "Missing", rows = missing_rows)
      )
  }

  print(gt_table)

  invisible(list(
    table = gt_table,
    summary = summary_df,
    n_obs = n_total,
    n_vars = length(Vars)
  ))
}


#' Check Normality of Variables
#'
#' @description
#' Tests and visualizes normality assumptions for numeric variables using
#' Shapiro-Wilk test, skewness, kurtosis, and Q-Q plots.
#'
#' @param data A data frame containing the variables.
#' @param Vars Character vector of numeric variable names to check.
#' @param tests Logical. Perform normality tests? Default TRUE.
#' @param plots Logical. Create Q-Q plots? Default TRUE.
#' @param digits Integer. Number of decimal places. Default 3.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{table}: A gt table with normality statistics
#'   \item \code{plots}: Q-Q plots (if requested)
#'   \item \code{results}: Data frame with all statistics
#' }
#'
#' @examples
#' \dontrun{
#' normality_check(mtcars, Vars = c("mpg", "hp", "wt"))
#' }
#'
#' @importFrom stats shapiro.test ks.test na.omit qqnorm qqline sd
#' @importFrom gt gt tab_header cols_align tab_style cell_text cells_body tab_source_note
#' @importFrom ggplot2 ggplot aes stat_qq stat_qq_line theme_minimal labs facet_wrap
#' @export
normality_check <- function(data,
                            Vars,
                            tests = TRUE,
                            plots = TRUE,
                            digits = 3) {

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (missing(Vars) || length(Vars) == 0) {
    stop("'Vars' must be specified")
  }

  # Check variables exist and are numeric
  missing_vars <- setdiff(Vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found: ", paste(missing_vars, collapse = ", "))
  }

  non_numeric <- Vars[!sapply(data[Vars], is.numeric)]
  if (length(non_numeric) > 0) {
    stop("All variables must be numeric: ", paste(non_numeric, collapse = ", "))
  }

  # Calculate statistics for each variable
  results_list <- lapply(Vars, function(var_name) {
    x <- na.omit(data[[var_name]])
    n <- length(x)

    if (n < 3) {
      return(data.frame(
        Variable = var_name,
        n = n,
        Skewness = NA,
        Kurtosis = NA,
        Statistic = NA,
        p = NA,
        Test = "—",
        Normal = "—",
        stringsAsFactors = FALSE
      ))
    }

    # Calculate skewness and kurtosis
    m <- mean(x)
    s <- sd(x)
    skewness <- sum((x - m)^3) / (n * s^3)
    kurtosis <- sum((x - m)^4) / (n * s^4) - 3  # Excess kurtosis

    # Normality test
    if (tests) {
      if (n >= 3 && n <= 5000) {
        # Shapiro-Wilk for n <= 5000
        test_result <- shapiro.test(x)
        test_name <- "Shapiro-Wilk"
        stat_val <- test_result$statistic
        p_val <- test_result$p.value
      } else if (n > 5000) {
        # Kolmogorov-Smirnov for larger samples
        test_result <- ks.test(x, "pnorm", mean = m, sd = s)
        test_name <- "Kolmogorov-Smirnov"
        stat_val <- test_result$statistic
        p_val <- test_result$p.value
      } else {
        test_name <- "—"
        stat_val <- NA
        p_val <- NA
      }

      # Interpret
      if (!is.na(p_val)) {
        if (p_val >= 0.05) {
          normal_interp <- "Yes"
        } else if (p_val >= 0.01) {
          normal_interp <- "Borderline"
        } else {
          normal_interp <- "No"
        }
      } else {
        normal_interp <- "—"
      }
    } else {
      test_name <- "—"
      stat_val <- NA
      p_val <- NA
      normal_interp <- "—"
    }

    data.frame(
      Variable = var_name,
      n = n,
      Skewness = round(skewness, digits),
      Kurtosis = round(kurtosis, digits),
      Statistic = round(stat_val, digits),
      p = p_val,
      Test = test_name,
      Normal = normal_interp,
      stringsAsFactors = FALSE
    )
  })

  results_df <- do.call(rbind, results_list)

  # Format p-values
  results_df$p_formatted <- sapply(results_df$p, function(p) {
    if (is.na(p)) return("—")
    if (p < 0.001) return("< .001")
    return(sub("^0\\.", ".", formatC(p, format = "f", digits = 3)))
  })

  # Create display dataframe
  display_df <- results_df[, c("Variable", "n", "Skewness", "Kurtosis", "Test", "Statistic", "p_formatted", "Normal")]
  names(display_df)[names(display_df) == "p_formatted"] <- "p"

  # Create gt table
  gt_table <- gt::gt(display_df) |>
    gt::tab_header(title = "Normality Check") |>
    gt::cols_align(align = "left", columns = "Variable") |>
    gt::cols_align(align = "center", columns = c("n", "Skewness", "Kurtosis", "Test", "Statistic", "p", "Normal")) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = "Variable")
    ) |>
    gt::tab_source_note(
      source_note = "Skewness: |value| > 2 suggests non-normality. Kurtosis: |value| > 7 suggests non-normality."
    )

  # Color-code Normal column
  no_rows <- which(display_df$Normal == "No")
  borderline_rows <- which(display_df$Normal == "Borderline")
  yes_rows <- which(display_df$Normal == "Yes")

  if (length(no_rows) > 0) {
    gt_table <- gt_table |>
      gt::tab_style(
        style = gt::cell_text(color = "#E74C3C", weight = "bold"),
        locations = gt::cells_body(columns = "Normal", rows = no_rows)
      )
  }
  if (length(borderline_rows) > 0) {
    gt_table <- gt_table |>
      gt::tab_style(
        style = gt::cell_text(color = "#F39C12"),
        locations = gt::cells_body(columns = "Normal", rows = borderline_rows)
      )
  }
  if (length(yes_rows) > 0) {
    gt_table <- gt_table |>
      gt::tab_style(
        style = gt::cell_text(color = "#27AE60"),
        locations = gt::cells_body(columns = "Normal", rows = yes_rows)
      )
  }

  print(gt_table)

  # Create Q-Q plots if requested
  qq_plot <- NULL
  if (plots) {
    # Prepare data for ggplot
    plot_data <- do.call(rbind, lapply(Vars, function(var_name) {
      x <- na.omit(data[[var_name]])
      data.frame(
        Variable = var_name,
        value = x,
        stringsAsFactors = FALSE
      )
    }))

    qq_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(sample = value)) +
      ggplot2::stat_qq(alpha = 0.6, color = "#3498DB") +
      ggplot2::stat_qq_line(color = "#E74C3C", linewidth = 0.8) +
      ggplot2::facet_wrap(~ Variable, scales = "free") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Q-Q Plots for Normality Assessment",
        x = "Theoretical Quantiles",
        y = "Sample Quantiles"
      ) +
      ggplot2::theme(
        strip.text = ggplot2::element_text(face = "bold", size = 10),
        plot.title = ggplot2::element_text(face = "bold", size = 12)
      )

    print(qq_plot)
  }

  invisible(list(
    table = gt_table,
    results = results_df,
    plots = qq_plot
  ))
}
