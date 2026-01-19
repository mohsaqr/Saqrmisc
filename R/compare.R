# Saqrmisc Package: Comparison Analysis Functions
#
# This file contains functions for group comparison analysis using ggbetweenstats.
# Functions: compare_groups

#' @importFrom dplyr group_by summarise mutate filter count select bind_rows any_of case_when ungroup first left_join all_of
#' @importFrom rlang sym .data
#' @importFrom ggplot2 ggplot aes scale_color_manual
#' @importFrom stats t.test aov sd var p.adjust
#' @importFrom grDevices colorRampPalette
#' @importFrom tidyr pivot_wider
NULL

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Interpret Bayes Factor
#'
#' @param bf Bayes factor value
#' @return Character interpretation
#' @noRd
interpret_bf <- function(bf) {
  if (is.na(bf) || is.null(bf)) return("NA")
  if (!is.finite(bf)) return(if (bf > 0) "Extreme evidence for H1" else "Extreme evidence for H0")

  if (bf > 100) return("Extreme evidence for H1")
  if (bf > 30) return("Very strong evidence for H1")
  if (bf > 10) return("Strong evidence for H1")
  if (bf > 3) return("Moderate evidence for H1")
  if (bf > 1) return("Anecdotal evidence for H1")
  if (bf == 1) return("No evidence")
  if (bf > 1/3) return("Anecdotal evidence for H0")
  if (bf > 1/10) return("Moderate evidence for H0")
  if (bf > 1/30) return("Strong evidence for H0")
  if (bf > 1/100) return("Very strong evidence for H0")
  return("Extreme evidence for H0")
}

#' Calculate pooled standard deviation
#'
#' @param x Numeric vector for group 1
#' @param y Numeric vector for group 2
#' @return Pooled SD value
#' @noRd
pooled_sd <- function(x, y) {
  n1 <- length(x)
  n2 <- length(y)
  sqrt(((n1 - 1) * stats::var(x) + (n2 - 1) * stats::var(y)) / (n1 + n2 - 2))
}

#' Reshape post-hoc p-value matrix to data frame
#'
#' @param p_matrix Matrix of p-values from pairwise tests
#' @param variable Variable name
#' @param method Adjustment method used
#' @return Data frame with comparisons
#' @noRd
reshape_posthoc_matrix <- function(p_matrix, variable, method) {
  comparisons <- data.frame(
    variable = character(),
    comparison = character(),
    p_adj = numeric(),
    method = character(),
    stringsAsFactors = FALSE
  )

  row_names <- rownames(p_matrix)
  col_names <- colnames(p_matrix)

  for (i in seq_along(row_names)) {
    for (j in seq_along(col_names)) {
      if (!is.na(p_matrix[i, j])) {
        comparisons <- rbind(comparisons, data.frame(
          variable = variable,
          comparison = paste(row_names[i], "vs", col_names[j]),
          p_adj = p_matrix[i, j],
          method = method,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  return(comparisons)
}

#' Generate ANOVA text report
#'
#' @param variable Variable name
#' @param category Grouping variable name
#' @param data Data subset
#' @param anova_details List with ANOVA statistics
#' @param posthoc_results Data frame with post-hoc results
#' @return Character string with formatted report
#' @noRd
generate_anova_text_report <- function(variable, category, data, anova_details, posthoc_results) {

  # Descriptive statistics by group
  desc_stats <- data %>%
    dplyr::group_by(.data[[category]]) %>%
    dplyr::summarise(
      M = mean(.data[[variable]], na.rm = TRUE),
      SD = stats::sd(.data[[variable]], na.rm = TRUE),
      n = dplyr::n(),
      .groups = 'drop'
    )

  # Format p-value
  format_p <- function(p) {
    if (p < 0.001) "< .001"
    else sprintf("= %.3f", p)
  }

  # Build report
  report <- paste0(
    "\n",
    paste(rep("=", 70), collapse = ""), "\n",
    "ANOVA RESULTS: ", variable, " by ", category, "\n",
    paste(rep("=", 70), collapse = ""), "\n\n"
  )

  # Descriptive statistics
  report <- paste0(report, "DESCRIPTIVE STATISTICS:\n")
  for (i in 1:nrow(desc_stats)) {
    report <- paste0(report, sprintf("  %-15s M = %6.2f, SD = %5.2f, n = %d\n",
                                     paste0(desc_stats[[category]][i], ":"),
                                     desc_stats$M[i],
                                     desc_stats$SD[i],
                                     desc_stats$n[i]))
  }

  # Omnibus test
  report <- paste0(report, "\nOMNIBUS TEST:\n")
  if (anova_details$test == "One-way ANOVA") {
    report <- paste0(report, sprintf("  F(%d, %d) = %.2f, p %s, eta-sq = %.3f (%s effect)\n",
                                     anova_details$df1,
                                     anova_details$df2,
                                     anova_details$statistic,
                                     format_p(anova_details$p_value),
                                     anova_details$effect_size,
                                     anova_details$effect_interp))

    # APA-style sentence
    sig_text <- if (anova_details$p_value < 0.05) "a statistically significant" else "no statistically significant"
    report <- paste0(report, sprintf("\n  A one-way ANOVA revealed %s difference in %s\n",
                                     sig_text, variable))
    report <- paste0(report, sprintf("  between %s groups, F(%d, %d) = %.2f, p %s, eta-sq = %.3f.\n",
                                     category,
                                     anova_details$df1,
                                     anova_details$df2,
                                     anova_details$statistic,
                                     format_p(anova_details$p_value),
                                     anova_details$effect_size))
  } else {
    report <- paste0(report, sprintf("  H(%d) = %.2f, p %s, epsilon-sq = %.3f\n",
                                     anova_details$df,
                                     anova_details$statistic,
                                     format_p(anova_details$p_value),
                                     anova_details$effect_size))
  }

  # Post-hoc comparisons
  if (!is.null(posthoc_results) && nrow(posthoc_results) > 0) {
    method_name <- if ("method" %in% names(posthoc_results)) posthoc_results$method[1] else "Bonferroni"
    report <- paste0(report, sprintf("\nPOST-HOC COMPARISONS (%s):\n", method_name))

    for (i in 1:nrow(posthoc_results)) {
      diff_str <- if ("diff" %in% names(posthoc_results) && !is.na(posthoc_results$diff[i])) {
        sprintf("diff = %6.2f, ", posthoc_results$diff[i])
      } else ""

      sig_star <- if ("significance" %in% names(posthoc_results)) posthoc_results$significance[i] else ""

      report <- paste0(report, sprintf("  %-25s %sp %s %s\n",
                                       paste0(posthoc_results$comparison[i], ":"),
                                       diff_str,
                                       format_p(posthoc_results$p_adj[i]),
                                       sig_star))
    }

    # Conclusion
    sig_comparisons <- posthoc_results[posthoc_results$p_adj < 0.05, ]
    if (nrow(sig_comparisons) > 0) {
      report <- paste0(report, "\nCONCLUSION:\n")
      report <- paste0(report, "  Significant pairwise differences were found between:\n")
      for (i in 1:nrow(sig_comparisons)) {
        report <- paste0(report, sprintf("  - %s (p %s)\n",
                                         sig_comparisons$comparison[i],
                                         format_p(sig_comparisons$p_adj[i])))
      }
    } else {
      report <- paste0(report, "\nCONCLUSION:\n")
      report <- paste0(report, "  No significant pairwise differences were found.\n")
    }
  }

  report <- paste0(report, paste(rep("=", 70), collapse = ""), "\n")

  return(report)
}

# =============================================================================
# PLOTTING HELPER FUNCTIONS
# =============================================================================

#' Create a modern theme for comparison plots
#' @noRd
theme_compare <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5, color = "gray40"),
      axis.title = ggplot2::element_text(face = "bold", size = 11),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "none",
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}

#' Modern color palette for comparison plots
#' @noRd
get_compare_colors <- function(n, palette = "default") {
  if (palette == "default") {
    # Modern, vibrant but professional colors
    cols <- c("#3498DB", "#E74C3C", "#2ECC71", "#9B59B6", "#F39C12", "#1ABC9C",
              "#E91E63", "#00BCD4", "#FF5722", "#607D8B")
  } else if (palette == "pastel") {
    cols <- c("#74B9FF", "#FF7675", "#55EFC4", "#A29BFE", "#FFEAA7", "#81ECEC",
              "#FD79A8", "#74B9FF", "#FAB1A0", "#B2BEC3")
  } else if (palette == "bold") {
    cols <- c("#2980B9", "#C0392B", "#27AE60", "#8E44AD", "#D35400", "#16A085",
              "#C2185B", "#0097A7", "#E64A19", "#455A64")
  }
  if (n <= length(cols)) cols[1:n] else colorRampPalette(cols)(n)
}

#' Create points + mean + CI plot
#' @noRd
plot_points_style <- function(data, x_var, y_var, colors, title = NULL, subtitle = NULL) {
  # Calculate summary statistics
  summary_stats <- data %>%
    dplyr::group_by(.data[[x_var]]) %>%
    dplyr::summarise(
      mean = mean(.data[[y_var]], na.rm = TRUE),
      sd = stats::sd(.data[[y_var]], na.rm = TRUE),
      n = dplyr::n(),
      se = sd / sqrt(n),
      ci = stats::qt(0.975, n - 1) * se,
      .groups = "drop"
    )

  n_groups <- length(unique(data[[x_var]]))
  plot_colors <- if (length(colors) >= n_groups) colors[1:n_groups] else get_compare_colors(n_groups)

  ggplot2::ggplot() +
    # Jittered points
    ggplot2::geom_jitter(
      data = data,
      ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]], color = .data[[x_var]]),
      width = 0.15, alpha = 0.5, size = 2
    ) +
    # Mean point
    ggplot2::geom_point(
      data = summary_stats,
      ggplot2::aes(x = .data[[x_var]], y = mean, fill = .data[[x_var]]),
      shape = 21, size = 4, color = "white", stroke = 1.5
    ) +
    # Error bars (CI)
    ggplot2::geom_errorbar(
      data = summary_stats,
      ggplot2::aes(x = .data[[x_var]], ymin = mean - ci, ymax = mean + ci, color = .data[[x_var]]),
      width = 0.1, linewidth = 1
    ) +
    ggplot2::scale_color_manual(values = plot_colors) +
    ggplot2::scale_fill_manual(values = plot_colors) +
    ggplot2::labs(x = NULL, y = y_var, title = title, subtitle = subtitle) +
    theme_compare()
}

#' Create modern boxplot
#' @noRd
plot_boxplot_style <- function(data, x_var, y_var, colors, title = NULL, subtitle = NULL,
                                show_points = TRUE) {
  n_groups <- length(unique(data[[x_var]]))
  plot_colors <- if (length(colors) >= n_groups) colors[1:n_groups] else get_compare_colors(n_groups)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]],
                                           fill = .data[[x_var]], color = .data[[x_var]])) +
    ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA, color = "gray30", linewidth = 0.5)

  if (show_points) {
    p <- p + ggplot2::geom_jitter(width = 0.15, alpha = 0.4, size = 1.5)
  }

  p +
    ggplot2::scale_fill_manual(values = plot_colors) +
    ggplot2::scale_color_manual(values = plot_colors) +
    ggplot2::labs(x = NULL, y = y_var, title = title, subtitle = subtitle) +
    theme_compare()
}

#' Create bar plot with error bars
#' @noRd
plot_bar_style <- function(data, x_var, y_var, colors, title = NULL, subtitle = NULL,
                            error_type = "ci") {
  # Calculate summary statistics
  summary_stats <- data %>%
    dplyr::group_by(.data[[x_var]]) %>%
    dplyr::summarise(
      mean = mean(.data[[y_var]], na.rm = TRUE),
      sd = stats::sd(.data[[y_var]], na.rm = TRUE),
      n = dplyr::n(),
      se = sd / sqrt(n),
      ci = stats::qt(0.975, n - 1) * se,
      .groups = "drop"
    )

  # Choose error bar type
  summary_stats$error <- if (error_type == "ci") summary_stats$ci else summary_stats$se

  n_groups <- length(unique(data[[x_var]]))
  plot_colors <- if (length(colors) >= n_groups) colors[1:n_groups] else get_compare_colors(n_groups)

  ggplot2::ggplot(summary_stats, ggplot2::aes(x = .data[[x_var]], y = mean, fill = .data[[x_var]])) +
    ggplot2::geom_col(alpha = 0.85, color = "gray30", linewidth = 0.3, width = 0.7) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean - error, ymax = mean + error),
      width = 0.15, linewidth = 0.7, color = "gray20"
    ) +
    ggplot2::scale_fill_manual(values = plot_colors) +
    ggplot2::labs(x = NULL, y = y_var, title = title, subtitle = subtitle) +
    theme_compare()
}

#' Create violin plot
#' @noRd
plot_violin_style <- function(data, x_var, y_var, colors, title = NULL, subtitle = NULL,
                               show_box = TRUE) {
  n_groups <- length(unique(data[[x_var]]))
  plot_colors <- if (length(colors) >= n_groups) colors[1:n_groups] else get_compare_colors(n_groups)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]],
                                           fill = .data[[x_var]])) +
    ggplot2::geom_violin(alpha = 0.7, color = "gray30", linewidth = 0.5, trim = FALSE)

  if (show_box) {
    p <- p + ggplot2::geom_boxplot(width = 0.15, fill = "white", alpha = 0.8,
                                    outlier.shape = NA, color = "gray30")
  }

  p +
    ggplot2::scale_fill_manual(values = plot_colors) +
    ggplot2::labs(x = NULL, y = y_var, title = title, subtitle = subtitle) +
    theme_compare()
}

# =============================================================================
# PIVOT TABLE HELPER FUNCTIONS
# =============================================================================

#' Create pivoted summary table
#'
#' @param combined_data Data frame with combined summary statistics
#' @param category_name Name of the grouping variable (becomes columns)
#' @param repeat_category_name Name of the repeat category variable
#' @param repeat_category_orig_vars Original variable names if multiple were combined
#' @param pivot_stat Statistic to show: "mean", "mean_sd", "median", "n"
#' @param pivot_stars Logical, show significance stars
#' @param pivot_split_level Logical, split combined level column into separate columns
#' @param p_adjust_method Method used for p-value adjustment
#' @param comparison_categories Variables being compared
#' @return gt table object
#' @noRd
#' Create Simple Pivot Table (without repeat_category)
#'
#' Creates a pivot table with variables as rows and category levels as columns
#' @param summary_data Data frame with summary statistics
#' @param category_name Name of category column
#' @param pivot_stat Statistic to show
#' @param pivot_stars Show p (ES) column
#' @param p_adjust_method P-value adjustment method
#' @param format Output format
#' @param show_header Show header
#' @param verbose Print messages
#' @return Formatted table
#' @noRd
create_pivot_table_simple <- function(summary_data, category_name,
                                       pivot_stat = "mean", pivot_stars = TRUE,
                                       p_adjust_method = "fdr",
                                       format = "gt", show_header = TRUE, verbose = TRUE) {

  # Get category levels for columns
  cat_levels <- unique(summary_data[[category_name]])

  # Get unique variables for rows
  variables <- unique(summary_data$variable)

  # Build pivot data
  rows <- list()

  for (var in variables) {
    var_data <- summary_data[summary_data$variable == var, ]

    row <- list(Variable = var)

    # Get p-value and ES for this variable (same across all category levels)
    p_val <- var_data$p_value[!is.na(var_data$p_value)][1]
    es_val <- if ("efsz" %in% names(var_data)) var_data$efsz[!is.na(var_data$efsz)][1] else NA

    # Add value for each category level
    for (cat_level in cat_levels) {
      cat_row <- var_data[var_data[[category_name]] == cat_level, ]
      if (nrow(cat_row) > 0) {
        mean_val <- cat_row$mean[1]
        sd_val <- cat_row$sd[1]
        n_val <- cat_row$n[1]

        if (pivot_stat == "mean") {
          row[[as.character(cat_level)]] <- sprintf("%.2f", mean_val)
        } else if (pivot_stat == "mean_sd") {
          row[[as.character(cat_level)]] <- sprintf("%.2f (%.2f)", mean_val, sd_val)
        } else if (pivot_stat == "n") {
          row[[as.character(cat_level)]] <- as.character(n_val)
        } else {
          row[[as.character(cat_level)]] <- sprintf("%.2f", mean_val)
        }
      } else {
        row[[as.character(cat_level)]] <- "-"
      }
    }

    # Add p (ES) column
    if (pivot_stars) {
      if (!is.na(p_val)) {
        if (p_val < 0.001) {
          p_str <- "<.001"
        } else {
          p_str <- sprintf("%.3f", p_val)
        }
      } else {
        p_str <- "-"
      }

      if (!is.na(es_val)) {
        es_str <- sprintf("%.2f", es_val)
      } else {
        es_str <- "-"
      }

      row[["p (ES)"]] <- sprintf("%s (%s)", p_str, es_str)
    }

    rows[[length(rows) + 1]] <- row
  }

  # Convert to data frame
  df <- dplyr::bind_rows(rows)

  # Build title
  stat_label <- switch(pivot_stat,
    "mean" = "Mean",
    "mean_sd" = "Mean (SD)",
    "median" = "Median",
    "n" = "N",
    "Mean"
  )

  title_text <- paste0(stat_label, " by ", category_name)
  subtitle_text <- paste0("Columns: ", paste(cat_levels, collapse = ", "),
                          " | p (ES) = p-value (effect size)")

  # Handle plain format
  if (format %in% c("plain", "markdown", "latex", "kable")) {
    result <- format_table(
      df = df,
      format = format,
      title = if (show_header) title_text else NULL,
      subtitle = if (show_header) subtitle_text else NULL,
      show_header = show_header,
      bold_cols = "Variable",
      align_left = "Variable"
    )
    return(result)
  }

  # Create gt table
  gt_table <- df %>%
    gt::gt() %>%
    gt::tab_header(
      title = title_text,
      subtitle = subtitle_text
    ) %>%
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
      locations = gt::cells_body(columns = "Variable")
    ) %>%
    gt::tab_options(
      table.border.top.color = "black",
      table.border.bottom.color = "black",
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black"
    ) %>%
    gt::tab_footnote(
      footnote = "p (ES) = p-value (effect size); ES = eta-squared or epsilon-squared"
    )

  return(gt_table)
}

create_pivot_table <- function(combined_data, category_name, repeat_category_name,
                                repeat_category_orig_vars = NULL,
                                pivot_by = "category",
                                pivot_stat = "mean", pivot_stars = TRUE,
                                pivot_split_level = TRUE,
                                p_adjust_method = "fdr", comparison_categories = NULL,
                                format = "gt", show_header = TRUE, verbose = TRUE) {

  # Validate inputs
  if (!category_name %in% names(combined_data)) {
    stop("Category column '", category_name, "' not found in data. Available columns: ",
         paste(names(combined_data), collapse = ", "))
  }
  if (!repeat_category_name %in% names(combined_data)) {
    stop("Repeat category column '", repeat_category_name, "' not found in data. Available columns: ",
         paste(names(combined_data), collapse = ", "))
  }

  # Prepare data for pivoting using symbols for grouping
  repeat_sym <- rlang::sym(repeat_category_name)
  cat_sym <- rlang::sym(category_name)

  # Determine which column becomes columns vs rows based on pivot_by
  if (pivot_by == "category") {
    # Category groups become columns (e.g., GPT, Mistral, Qwen as columns)
    pivot_col_name <- category_name
    pivot_col_sym <- cat_sym
    row_col_name <- repeat_category_name
    row_col_sym <- repeat_sym
    groups <- unique(combined_data[[category_name]])
  } else {
    # Repeat category levels become columns (e.g., he/him, she/her as columns)
    pivot_col_name <- repeat_category_name
    pivot_col_sym <- repeat_sym
    row_col_name <- category_name
    row_col_sym <- cat_sym
    groups <- unique(combined_data[[repeat_category_name]])
  }

  # Prepare data for pivoting
  pivot_data <- combined_data %>%
    dplyr::group_by(!!repeat_sym, variable, !!cat_sym) %>%
    dplyr::summarise(
      mean_val = mean(mean, na.rm = TRUE),
      sd_val = mean(sd, na.rm = TRUE),
      median_val = if ("median" %in% names(combined_data)) mean(median, na.rm = TRUE) else NA,
      n_val = sum(n, na.rm = TRUE),
      p_value_agg = dplyr::first(p_value[!is.na(p_value)]),
      es_agg = if ("efsz" %in% names(combined_data)) dplyr::first(efsz[!is.na(efsz)]) else NA_real_,
      .groups = "drop"
    )

  # Create the statistic to display
  pivot_data <- pivot_data %>%
    dplyr::mutate(
      display_val = dplyr::case_when(
        pivot_stat == "mean" ~ sprintf("%.3f", mean_val),
        pivot_stat == "mean_sd" ~ sprintf("%.2f (%.2f)", mean_val, sd_val),
        pivot_stat == "median" ~ sprintf("%.3f", median_val),
        pivot_stat == "n" ~ as.character(n_val),
        TRUE ~ sprintf("%.3f", mean_val)
      )
    )

  # Select columns for pivot (exclude p_value_agg to avoid creating separate rows)
  pivot_subset <- pivot_data %>%
    dplyr::select(!!repeat_sym, variable, !!cat_sym, display_val)

  # Pivot to wide format - pivot the selected column
  pivot_wide <- pivot_subset %>%
    tidyr::pivot_wider(
      names_from = !!pivot_col_sym,
      values_from = display_val
    )

  # Get p-value and ES per row (one p-value/ES per variable/row_col combination)
  stats_unique <- pivot_data %>%
    dplyr::group_by(!!row_col_sym, variable) %>%
    dplyr::summarise(
      p_value = dplyr::first(p_value_agg[!is.na(p_value_agg)]),
      es_value = dplyr::first(es_agg[!is.na(es_agg)]),
      .groups = "drop"
    )

  # Merge p-values and ES back
  pivot_wide <- pivot_wide %>%
    dplyr::left_join(stats_unique, by = c(row_col_name, "variable"))

  # Add p (ES) column instead of just stars
  if (pivot_stars) {
    pivot_wide <- pivot_wide %>%
      dplyr::mutate(
        `p (ES)` = dplyr::case_when(
          is.na(p_value) & is.na(es_value) ~ "-",
          is.na(p_value) ~ sprintf("- (%.2f)", es_value),
          is.na(es_value) & p_value < 0.001 ~ "<.001 (-)",
          is.na(es_value) ~ sprintf("%.3f (-)", p_value),
          p_value < 0.001 ~ sprintf("<.001 (%.2f)", es_value),
          TRUE ~ sprintf("%.3f (%.2f)", p_value, es_value)
        )
      )
  }

  # Rename columns for display
  names(pivot_wide)[names(pivot_wide) == row_col_name] <- "level"
  names(pivot_wide)[names(pivot_wide) == "variable"] <- "Variable"

  # Split combined level column if requested and multiple vars were combined
  level_cols <- "level"
  if (pivot_split_level && !is.null(repeat_category_orig_vars) && length(repeat_category_orig_vars) > 1) {
    # Check if level column contains the separator
    if (any(grepl(" \\| ", pivot_wide$level))) {
      # Split the level column into separate columns
      split_levels <- strsplit(as.character(pivot_wide$level), " \\| ")
      max_parts <- max(sapply(split_levels, length))

      # Use original variable names if available and match count
      if (length(repeat_category_orig_vars) == max_parts) {
        new_col_names <- repeat_category_orig_vars
      } else {
        new_col_names <- paste0("level_", seq_len(max_parts))
      }

      # Create new columns from split
      for (i in seq_len(max_parts)) {
        pivot_wide[[new_col_names[i]]] <- sapply(split_levels, function(x) {
          if (length(x) >= i) x[i] else NA_character_
        })
      }

      # Remove original level column
      pivot_wide <- pivot_wide %>% dplyr::select(-level)
      level_cols <- new_col_names
    }
  }

  # Reorder columns: Variable, level col(s), groups..., p (ES) (if present)
  col_order <- c("Variable", level_cols, as.character(groups))
  if (pivot_stars) col_order <- c(col_order, "p (ES)")
  pivot_wide <- pivot_wide %>%
    dplyr::select(dplyr::all_of(col_order), dplyr::everything())

  # Remove p_value and es_value from display (keep for formatting)
  cols_to_remove <- intersect(c("p_value", "es_value"), names(pivot_wide))
  display_data <- pivot_wide %>% dplyr::select(-dplyr::all_of(cols_to_remove))

  # Build title and subtitle
  stat_label <- switch(pivot_stat,
    "mean" = "Mean",
    "mean_sd" = "Mean (SD)",
    "median" = "Median",
    "n" = "N"
  )

  title_text <- paste0(stat_label, " by Factor Levels")
  subtitle_text <- paste0("Columns: ", paste(groups, collapse = ", "),
                          " | P-adjustment: ", toupper(p_adjust_method))

  # Handle non-GT formats

  if (format %in% c("plain", "markdown", "latex", "kable")) {
    result <- format_table(
      df = display_data,
      format = format,
      title = if (show_header) title_text else NULL,
      subtitle = if (show_header) subtitle_text else NULL,
      show_header = show_header,
      bold_cols = "Variable",
      align_left = c("Variable", level_cols)
    )
    return(result)
  }

  # Create gt table
  gt_pivot <- display_data %>%
    gt::gt() %>%
    gt::tab_header(
      title = title_text,
      subtitle = subtitle_text
    ) %>%
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
      locations = gt::cells_body(columns = "Variable")
    ) %>%
    gt::cols_align(align = "center") %>%
    gt::cols_align(align = "left", columns = c("Variable", level_cols)) %>%
    gt::tab_options(
      table.border.top.width = gt::px(2),
      table.border.top.color = "black",
      table.border.bottom.width = gt::px(2),
      table.border.bottom.color = "black"
    )

  # Add footnote for p (ES) column
  gt_pivot <- gt_pivot %>%
    gt::tab_footnote(
      footnote = "p (ES) = p-value (effect size); ES = eta-squared or epsilon-squared"
    )

  return(gt_pivot)
}

#' Create pivoted summary table with row grouping by factor
#'
#' @param combined_data Data frame with combined summary statistics including factor and level columns
#' @param category_name Name of the grouping variable (becomes columns)
#' @param repeat_category_vars Original repeat category variable names
#' @param pivot_stat Statistic to show: "mean", "mean_sd", "median", "n"
#' @param pivot_stars Logical, show significance stars
#' @param p_adjust_method Method used for p-value adjustment
#' @param comparison_categories Variables being compared
#' @param format Output format
#' @param show_header Show table header
#' @param verbose Print messages
#' @return gt table object with row grouping
#' @noRd
create_pivot_table_grouped <- function(combined_data, category_name,
                                        repeat_category_vars,
                                        pivot_stat = "mean", pivot_stars = TRUE,
                                        p_adjust_method = "fdr",
                                        comparison_categories = NULL,
                                        format = "gt", show_header = TRUE,
                                        verbose = TRUE) {

  # Get unique groups (will become columns)
  groups <- unique(combined_data[[category_name]])
  cat_sym <- rlang::sym(category_name)

  # Data should already have 'factor' and 'level' columns from the collection step
  if (!"factor" %in% names(combined_data) || !"level" %in% names(combined_data)) {
    stop("combined_data must have 'factor' and 'level' columns")
  }

  # Aggregate by factor, level, variable, and category
  pivot_agg <- combined_data %>%
    dplyr::group_by(factor, level, variable, !!cat_sym) %>%
    dplyr::summarise(
      mean_val = mean(mean, na.rm = TRUE),
      sd_val = mean(sd, na.rm = TRUE),
      median_val = if ("median" %in% names(combined_data)) mean(median, na.rm = TRUE) else NA,
      n_val = sum(n, na.rm = TRUE),
      p_value_agg = dplyr::first(p_value[!is.na(p_value)]),
      .groups = "drop"
    )

  # Create the statistic to display
  pivot_agg <- pivot_agg %>%
    dplyr::mutate(
      display_val = dplyr::case_when(
        pivot_stat == "mean" ~ sprintf("%.3f", mean_val),
        pivot_stat == "mean_sd" ~ sprintf("%.2f (%.2f)", mean_val, sd_val),
        pivot_stat == "median" ~ sprintf("%.3f", median_val),
        pivot_stat == "n" ~ as.character(n_val),
        TRUE ~ sprintf("%.3f", mean_val)
      )
    )

  # Select columns for pivot (exclude p_value_agg)
  pivot_subset <- pivot_agg %>%
    dplyr::select(factor, level, variable, !!cat_sym, display_val)

  # Pivot to wide format
  pivot_wide <- pivot_subset %>%
    tidyr::pivot_wider(
      names_from = !!cat_sym,
      values_from = display_val
    )

  # Get p-value per row
  p_values_unique <- pivot_agg %>%
    dplyr::group_by(factor, level, variable) %>%
    dplyr::summarise(p_value = dplyr::first(p_value_agg[!is.na(p_value_agg)]), .groups = "drop")

  # Merge p-values back
  pivot_wide <- pivot_wide %>%
    dplyr::left_join(p_values_unique, by = c("factor", "level", "variable"))

  # Add significance stars
  if (pivot_stars) {
    pivot_wide <- pivot_wide %>%
      dplyr::mutate(
        Sig = dplyr::case_when(
          is.na(p_value) ~ "",
          p_value < 0.001 ~ "***",
          p_value < 0.01 ~ "**",
          p_value < 0.05 ~ "*",
          TRUE ~ ""
        )
      )
  }

  # Reorder columns
  col_order <- c("factor", "level", "variable", as.character(groups))
  if (pivot_stars) col_order <- c(col_order, "Sig")
  pivot_wide <- pivot_wide %>%
    dplyr::select(dplyr::all_of(col_order), dplyr::everything())

  # Remove p_value and variable column from display if only one variable
  display_data <- pivot_wide %>% dplyr::select(-p_value)
  if (length(unique(display_data$variable)) == 1) {
    display_data <- display_data %>% dplyr::select(-variable)
  }

  # Build title and subtitle
  stat_label <- switch(pivot_stat,
    "mean" = "Mean",
    "mean_sd" = "Mean (SD)",
    "median" = "Median",
    "n" = "N"
  )

  title_text <- paste0(stat_label, " by Factor Levels")
  subtitle_text <- paste0("Columns: ", paste(groups, collapse = ", "),
                          " | P-adjustment: ", toupper(p_adjust_method))

  # Handle non-GT formats
  if (format %in% c("plain", "markdown", "latex", "kable")) {
    result <- format_table(
      df = display_data,
      format = format,
      title = if (show_header) title_text else NULL,
      subtitle = if (show_header) subtitle_text else NULL,
      show_header = show_header,
      bold_cols = c("factor"),
      align_left = c("factor", "level")
    )
    return(result)
  }

  # Create gt table with row grouping
  gt_pivot <- display_data %>%
    gt::gt(groupname_col = "factor") %>%
    gt::tab_header(
      title = title_text,
      subtitle = subtitle_text
    ) %>%
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
      locations = gt::cells_row_groups()
    ) %>%
    gt::tab_options(
      table.border.top.color = "black",
      table.border.bottom.color = "black",
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black"
    )

  # Add footnote
  gt_pivot <- gt_pivot %>%
    gt::tab_footnote(
      footnote = "*** p < 0.001, ** p < 0.01, * p < 0.05"
    )

  return(gt_pivot)
}

#' Create pivoted post-hoc comparison table
#'
#' @param all_posthoc_data Data frame with all post-hoc results
#' @param repeat_category_name Name of the repeat category variable
#' @param posthoc_format "wide", "long", or "both"
#' @param p_adjust_method Method used for p-value adjustment
#' @return List with wide and/or long format tables
#' @noRd
create_posthoc_pivot_table <- function(all_posthoc_data, repeat_category_name,
                                        posthoc_format = "wide", p_adjust_method = "fdr",
                                        format = "gt", show_header = TRUE, verbose = TRUE) {

  result <- list()

  if (nrow(all_posthoc_data) == 0) {
    return(result)
  }

  # Standardize column names
  if (!"group" %in% names(all_posthoc_data) && repeat_category_name %in% names(all_posthoc_data)) {
    all_posthoc_data$group <- all_posthoc_data[[repeat_category_name]]
  }

  # Add diff column if missing (for nonparametric tests)
  if (!"diff" %in% names(all_posthoc_data)) {
    all_posthoc_data$diff <- NA_real_
  }

  # Add significance stars
  all_posthoc_data <- all_posthoc_data %>%
    dplyr::mutate(
      sig = dplyr::case_when(
        is.na(p_adj) ~ "",
        p_adj < 0.001 ~ "***",
        p_adj < 0.01 ~ "**",
        p_adj < 0.05 ~ "*",
        TRUE ~ ""
      ),
      # Format difference with stars
      diff_sig = dplyr::case_when(
        is.na(diff) ~ sig,
        TRUE ~ paste0(sprintf("%.2f", diff), sig)
      )
    )

  # =========================================================================
  # LONG FORMAT
  # =========================================================================
  if (posthoc_format %in% c("long", "both")) {
    long_data <- all_posthoc_data %>%
      dplyr::mutate(
        Variable = variable,
        Level = group,
        Comparison = comparison,
        Diff = ifelse(is.na(diff), "-", sprintf("%.2f", diff)),
        `P-adj` = dplyr::case_when(
          is.na(p_adj) ~ "NA",
          p_adj < 0.001 ~ "< .001",
          TRUE ~ sprintf("%.3f", p_adj)
        ),
        Sig = sig
      ) %>%
      dplyr::select(Variable, Level, Comparison, Diff, `P-adj`, Sig)

    title_text <- "Post-hoc Pairwise Comparisons"
    subtitle_text <- paste0("P-adjustment: ", toupper(p_adjust_method))

    if (format %in% c("plain", "markdown", "latex", "kable")) {
      result$long <- format_table(
        df = long_data,
        format = format,
        title = if (show_header) title_text else NULL,
        subtitle = if (show_header) subtitle_text else NULL,
        show_header = show_header,
        bold_cols = "Variable",
        align_left = c("Variable", "Level", "Comparison")
      )
    } else {
      sig_rows <- which(long_data$Sig != "")

      gt_long <- long_data %>%
        gt::gt() %>%
        gt::tab_header(
          title = title_text,
          subtitle = subtitle_text
        ) %>%
        gt::tab_style(
          style = list(
            gt::cell_text(weight = "bold"),
            gt::cell_borders(sides = "bottom", color = "black", weight = gt::px(2))
          ),
          locations = gt::cells_column_labels()
        ) %>%
        gt::cols_align(align = "center") %>%
        gt::cols_align(align = "left", columns = c("Variable", "Level", "Comparison")) %>%
        gt::tab_options(
          table.border.top.width = gt::px(2),
          table.border.top.color = "black",
          table.border.bottom.width = gt::px(2),
          table.border.bottom.color = "black"
        )

      if (length(sig_rows) > 0) {
        gt_long <- gt_long %>%
          gt::tab_style(
            style = gt::cell_text(weight = "bold"),
            locations = gt::cells_body(rows = sig_rows)
          ) %>%
          gt::tab_style(
            style = gt::cell_text(color = "red"),
            locations = gt::cells_body(columns = "P-adj", rows = sig_rows)
          )
      }

      gt_long <- gt_long %>%
        gt::tab_footnote(footnote = "*** p < 0.001, ** p < 0.01, * p < 0.05")

      result$long <- gt_long
    }
  }

  # =========================================================================
  # WIDE FORMAT
  # =========================================================================
  if (posthoc_format %in% c("wide", "both")) {
    # Get unique comparisons
    comparisons <- unique(all_posthoc_data$comparison)

    wide_data <- all_posthoc_data %>%
      dplyr::select(variable, group, comparison, diff_sig) %>%
      tidyr::pivot_wider(
        names_from = comparison,
        values_from = diff_sig
      )

    # Rename columns for display
    names(wide_data)[names(wide_data) == "variable"] <- "Variable"
    names(wide_data)[names(wide_data) == "group"] <- "Level"

    title_text <- "Post-hoc Pairwise Comparisons (Wide)"
    subtitle_text <- paste0("Values show mean difference with significance | P-adjustment: ",
                            toupper(p_adjust_method))

    if (format %in% c("plain", "markdown", "latex", "kable")) {
      result$wide <- format_table(
        df = wide_data,
        format = format,
        title = if (show_header) title_text else NULL,
        subtitle = if (show_header) subtitle_text else NULL,
        show_header = show_header,
        bold_cols = "Variable",
        align_left = c("Variable", "Level")
      )
    } else {
      gt_wide <- wide_data %>%
        gt::gt() %>%
        gt::tab_header(
          title = title_text,
          subtitle = subtitle_text
        ) %>%
        gt::tab_style(
          style = list(
            gt::cell_text(weight = "bold"),
            gt::cell_borders(sides = "bottom", color = "black", weight = gt::px(2))
          ),
          locations = gt::cells_column_labels()
        ) %>%
        gt::cols_align(align = "center") %>%
        gt::cols_align(align = "left", columns = c("Variable", "Level")) %>%
        gt::tab_options(
          table.border.top.width = gt::px(2),
          table.border.top.color = "black",
          table.border.bottom.width = gt::px(2),
          table.border.bottom.color = "black"
        ) %>%
        gt::tab_footnote(footnote = "*** p < 0.001, ** p < 0.01, * p < 0.05")

      result$wide <- gt_wide
    }
  }

  return(result)
}

#' Create pivot table for "within" compare_mode
#'
#' @param all_results List of results from within-mode analysis
#' @param category_levels Levels of the category variable (columns)
#' @param compare_by Factors that were tested
#' @param pivot_stat Statistic to show
#' @param pivot_stars Show significance stars
#' @param p_adjust_method P-value adjustment method
#' @param format Output format
#' @param show_header Show header
#' @param verbose Print messages
#' @return Formatted table
#' @noRd
create_pivot_table_within <- function(all_results, category_levels, compare_by,
                                       pivot_stat = "mean", pivot_stars = TRUE,
                                       p_adjust_method = "fdr",
                                       format = "gt", show_header = TRUE,
                                       verbose = TRUE) {

  # Build data frame for the pivot table
  rows <- list()

  for (outcome_var in names(all_results)) {
    var_results <- all_results[[outcome_var]]

    for (factor_var in names(var_results)) {
      factor_data <- var_results[[factor_var]]

      # Get factor levels in order
      factor_levels <- unique(sapply(factor_data, function(x) x$level))

      # Collect p-values and effect sizes for this factor across all category levels (for the Sig row)
      p_values_by_cat <- list()
      es_by_cat <- list()

      for (key in names(factor_data)) {
        item <- factor_data[[key]]

        # Create row for this factor level
        row <- list(
          factor = factor_var,
          level = item$level,
          variable = outcome_var
        )

        # Add means for each category level
        for (cat_level in category_levels) {
          mean_val <- item[[paste0(cat_level, "_mean")]]
          sd_val <- item[[paste0(cat_level, "_sd")]]
          p_val <- item[[paste0(cat_level, "_p")]]
          es_val <- item[[paste0(cat_level, "_es")]]

          # Format display value
          if (pivot_stat == "mean") {
            row[[cat_level]] <- if (!is.null(mean_val) && !is.na(mean_val)) sprintf("%.2f", mean_val) else "-"
          } else if (pivot_stat == "mean_sd") {
            if (!is.null(mean_val) && !is.na(mean_val)) {
              row[[cat_level]] <- sprintf("%.2f (%.2f)", mean_val, ifelse(is.na(sd_val), 0, sd_val))
            } else {
              row[[cat_level]] <- "-"
            }
          } else {
            row[[cat_level]] <- if (!is.null(mean_val) && !is.na(mean_val)) sprintf("%.2f", mean_val) else "-"
          }

          # Collect p-value and effect size (same for all levels of this factor within this category)
          if (!is.null(p_val) && !is.na(p_val)) {
            p_values_by_cat[[cat_level]] <- p_val
          }
          if (!is.null(es_val) && !is.na(es_val)) {
            es_by_cat[[cat_level]] <- es_val
          }
        }

        rows[[length(rows) + 1]] <- row
      }

      # Add Sig row for this factor
      # Combined p-value (effect size) row
      stats_row <- list(
        factor = factor_var,
        level = "p (ES)",
        variable = outcome_var
      )

      for (cat_level in category_levels) {
        p_val <- p_values_by_cat[[cat_level]]
        es_val <- es_by_cat[[cat_level]]

        # Format p-value
        if (!is.null(p_val) && !is.na(p_val)) {
          if (p_val < 0.001) {
            p_str <- "<.001"
          } else {
            p_str <- sprintf("%.3f", p_val)
          }
        } else {
          p_str <- "-"
        }

        # Format effect size
        if (!is.null(es_val) && !is.na(es_val)) {
          es_str <- sprintf("%.2f", es_val)
        } else {
          es_str <- "-"
        }

        # Combine: p-value (effect size)
        stats_row[[cat_level]] <- sprintf("%s (%s)", p_str, es_str)
      }

      rows[[length(rows) + 1]] <- stats_row
    }
  }

  # Convert to data frame
  df <- dplyr::bind_rows(rows)

  # If only one variable, remove variable column
  if (length(unique(df$variable)) == 1) {
    df <- df %>% dplyr::select(-variable)
  }

  # Build title
  stat_label <- switch(pivot_stat,
    "mean" = "Mean",
    "mean_sd" = "Mean (SD)",
    "median" = "Median",
    "n" = "N",
    "Mean"
  )

  title_text <- paste0(stat_label, " by Factor Levels (Within-Group Analysis)")
  subtitle_text <- paste0("Columns: ", paste(category_levels, collapse = ", "),
                          " | Sig row shows factor effect within each column")

  # Handle plain format
  if (format %in% c("plain", "markdown", "latex", "kable")) {
    result <- format_table(
      df = df,
      format = format,
      title = if (show_header) title_text else NULL,
      subtitle = if (show_header) subtitle_text else NULL,
      show_header = show_header,
      bold_cols = "factor",
      align_left = c("factor", "level")
    )
    return(result)
  }

  # Create gt table with row grouping
  gt_table <- df %>%
    gt::gt(groupname_col = "factor") %>%
    gt::tab_header(
      title = title_text,
      subtitle = subtitle_text
    ) %>%
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
      locations = gt::cells_row_groups()
    ) %>%
    gt::tab_options(
      table.border.top.color = "black",
      table.border.bottom.color = "black",
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black"
    ) %>%
    gt::tab_footnote(
      footnote = "p (ES) = p-value (effect size); ES = eta-squared or epsilon-squared"
    )

  return(gt_table)
}

# =============================================================================
# MAIN FUNCTION
# =============================================================================

#' Generate Group Comparison Plots and Statistics
#'
#' @description
#' A comprehensive function for comparing groups using publication-ready
#' visualizations and statistical tests. Creates `ggbetweenstats` plots from
#' the ggstatsplot package with automatic test selection based on the number
#' of groups:
#'
#' For 2 groups: t-test (parametric) or Mann-Whitney U (nonparametric).
#' For 3+ groups: ANOVA (parametric) or Kruskal-Wallis (nonparametric)
#' with post-hoc pairwise comparisons.
#' Also supports Bayesian analysis, equivalence testing (TOST),
#' and stratified analyses using repeat_category.
#'
#' @param data A data frame containing the variables to analyze.
#' @param category Character. Name(s) of the grouping variable(s). Can be a single
#'   variable (e.g., "gender") or multiple variables for interaction groups
#'   (e.g., c("gender", "treatment") creates "Male | Treatment", etc.).
#' @param category_sep Character. Separator for combined group labels when
#'   multiple variables are passed to category. Default: " | ".
#' @param Vars Column specification for variables to compare. Can be NULL
#'   (default, all numeric columns), a character vector of column names,
#'   a numeric vector of column indices, or a single number (from that column to end).
#' @param repeat_category Optional character. Name of a stratification variable.
#'   When provided, separate analyses are performed for each level (e.g., analyze
#'   gender differences separately for each country). Used with compare_mode = "between".
#' @param compare_by Optional character vector. Factors to test within each category level.
#'   Used with compare_mode = "within". For example, if category = "llm" and
#'   compare_by = c("pronoun", "support"), tests whether pronoun and support have
#'   effects within each LLM dataset separately.
#' @param compare_mode Character. Analysis mode:
#'   "between" (default) - compare category levels (e.g., GPT vs Mistral vs Qwen),
#'   "within" - test compare_by factors within each category level (e.g., test
#'   pronoun effect within GPT, within Mistral, within Qwen separately).
#' @param repeat_combine Logical. When multiple repeat_category variables are provided
#'   with compare_mode = "between", should they be combined into one grouping
#'   (e.g., "he/him | High")? Default TRUE.
#' @param repeat_levels Optional character vector. Specific levels of repeat_category
#'   to include in the analysis. If NULL (default), all levels are used.
#' @param plots Logical. Generate visualizations? Default TRUE.
#' @param plot_style Character. Style of plots: "points" (default, jittered with CI),
#'   "boxplot", "bar", "violin", or "ggstatsplot" (requires ggstatsplot package).
#' @param table Logical. Generate summary statistics table? Default TRUE.
#' @param type Character. Type of statistical test: "auto" (default), "parametric"/"p",
#'   "nonparametric"/"np", "kw" (Kruskal-Wallis), "mw" (Mann-Whitney), or "bayes"/"bf".
#' @param bayesian Logical. Compute Bayesian t-tests with Bayes Factors? Default: FALSE.
#'   Requires the BayesFactor package. Can also use `type = "bayes"`.
#' @param equivalence Logical. Perform equivalence testing (TOST)? Default: FALSE.
#'   Requires the TOSTER package.
#' @param equivalence_bounds Numeric vector of length 2. Equivalence bounds in
#'   Cohen's d units for TOST. Default: c(-0.5, 0.5).
#' @param nonparametric Logical. Use nonparametric tests? Default: FALSE.
#'   When TRUE, uses Mann-Whitney U (2 groups) or Kruskal-Wallis (3+ groups).
#' @param p_adjust_method Method for multiple comparison correction. Options:
#'   "none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr".
#'   Default: "fdr" (Benjamini-Hochberg False Discovery Rate).
#'   Applied to both tables and post-hoc tests.
#' @param posthoc Logical. Compute post-hoc pairwise comparisons for 3+ groups?
#'   Default: TRUE. Results include a comparison table and text report.
#' @param posthoc_method Method for post-hoc comparisons: "games-howell" (default,
#'   does not assume equal variances) or "tukey" (Tukey's HSD, assumes equal variances).
#' @param posthoc_table Logical. Include detailed post-hoc pairwise comparison table
#'   in the output? Default FALSE.
#' @param posthoc_format Character. Format for post-hoc table when pivot = TRUE:
#'   "wide" (default, comparisons as columns), "long" (one row per comparison),
#'   or "both" (return both formats).
#' @param pairwise_display Which pairwise comparisons to show on plots? Options:
#'   "significant" (default), "all", "none".
#' @param pivot Logical. Pivot to wide format? Default FALSE.
#'   When TRUE, creates a wide table based on pivot_by parameter.
#' @param pivot_by Character. Which variable to pivot to columns:
#'   "category" (default) - category groups become columns (e.g., GPT, Mistral, Qwen),
#'   "repeat_category" - repeat_category levels become columns (e.g., he/him, she/her).
#' @param pivot_stat Character. Statistic to display in pivoted cells:
#'   "mean" (default), "mean_sd" (mean with SD in parentheses),
#'   "median", or "n" (count).
#' @param pivot_stars Logical. Show significance stars in pivot table?
#'   Default TRUE when pivot = TRUE.
#' @param pivot_split_level Logical. When multiple repeat_category variables are
#'   combined (e.g., "he/him | High"), split them back into separate columns?
#'   Default TRUE. When FALSE, keeps the combined "level" column.
#' @param min_threshold Numeric. Minimum proportion (0-1) of total sample required
#'   to include a repeat_category level. Default: 0.05 (5 percent).
#' @param min_subcategory Integer. Minimum observations required per group.
#'   Groups with fewer observations are excluded from analysis. Default: 5.
#' @param colors Character vector of colors for groups. Default: NULL uses
#'   a built-in palette.
#' @param verbose Logical. Print progress messages? Default: TRUE.
#' @param combined_table Logical. When `repeat_category` is used, combine all
#'   results into a single table? Default: TRUE. The combined table shows
#'   all repeat_category levels together with bold highest means and red
#'   significant p-values.
#' @param format Character. Output format for tables: "gt" (default, publication-ready),
#'   "plain" (data frame), "markdown", "latex", or "kable".
#' @param show_header Logical. Show title/subtitle header? Default TRUE.
#'   Set to FALSE to hide the table header.
#' @param interpret Logical. Pass results to AI for automatic interpretation?
#'   Default FALSE. When TRUE, generates clean Methods and Results text using
#'   AI. Requires API key setup (see \code{\link{set_api_key}}).
#' @param ... Additional arguments passed to \code{\link{pass}} when
#'   interpret = TRUE (e.g., provider, model, context).
#'
#' @return A list with class "comparison_results" containing: plots (named list
#'   of ggplot objects), grid_plot (combined plot grid if multiple variables),
#'   summary_table (formatted gt table), and summary_data (data frame with raw
#'   statistics). For stratified analysis (with repeat_category), returns a named
#'   list where each element corresponds to a level of repeat_category.
#'   When pivot = TRUE, additional elements are returned:
#'   \itemize{
#'     \item pivot_table: gt table with groups as columns and factor levels as rows
#'     \item posthoc_pivot_wide: Post-hoc comparisons in wide format (if posthoc_table = TRUE)
#'     \item posthoc_pivot_long: Post-hoc comparisons in long format (if posthoc_format = "long" or "both")
#'   }
#'
#' @examples
#' \dontrun{
#' # ============================================================
#' # EXAMPLE 1: Basic Two-Group Comparison (t-test)
#' # ============================================================
#' data <- data.frame(
#'   gender = rep(c("Male", "Female"), each = 50),
#'   score = c(rnorm(50, mean = 100, sd = 15),
#'             rnorm(50, mean = 105, sd = 15))
#' )
#'
#' results <- compare_groups(
#'   data = data,
#'   category = "gender",
#'   Vars = c("score")
#' )
#'
#' # View the plot
#' results$plots$gender_vs_score
#'
#' # View summary statistics
#' results$summary_data
#'
#' # ============================================================
#' # EXAMPLE 2: Three-Group Comparison (ANOVA with post-hoc)
#' # ============================================================
#' data <- data.frame(
#'   country = rep(c("USA", "UK", "Germany"), each = 40),
#'   score1 = c(rnorm(40, 75, 10), rnorm(40, 70, 11), rnorm(40, 85, 9)),
#'   score2 = c(rnorm(40, 50, 8), rnorm(40, 55, 9), rnorm(40, 48, 7))
#' )
#'
#' results <- compare_groups(
#'   data = data,
#'   category = "country",
#'   Vars = c("score1", "score2"),
#'   posthoc = TRUE,
#'   posthoc_method = "tukey",
#'   pairwise_display = "all"
#' )
#'
#' # View post-hoc pairwise comparisons
#' results$summary_data$posthoc_results[[1]]
#'
#' # Print full ANOVA report
#' cat(results$summary_data$anova_report[1])
#'
#' # ============================================================
#' # EXAMPLE 3: Bayesian Analysis
#' # ============================================================
#' results <- compare_groups(
#'   data = data,
#'   category = "country",
#'   Vars = c("score1"),
#'   bayesian = TRUE
#' )
#'
#' # View Bayes Factors
#' results$summary_data[, c("country", "mean", "bf10", "bf_interpretation")]
#'
#' # ============================================================
#' # EXAMPLE 4: Nonparametric Tests
#' # ============================================================
#' results <- compare_groups(
#'   data = data,
#'   category = "country",
#'   Vars = c("score1"),
#'   nonparametric = TRUE  # Uses Kruskal-Wallis + pairwise Wilcoxon
#' )
#'
#' # ============================================================
#' # EXAMPLE 5: Stratified Analysis (separate tests per subgroup)
#' # ============================================================
#' data <- data.frame(
#'   country = rep(c("USA", "UK", "Germany"), each = 60),
#'   gender = rep(c("Male", "Female"), times = 90),
#'   score = rnorm(180, mean = 100, sd = 15)
#' )
#'
#' # Compare gender WITHIN each country
#' results <- compare_groups(
#'   data = data,
#'   category = "gender",
#'   Vars = c("score"),
#'   repeat_category = "country"
#' )
#'
#' # Access results by country
#' results$USA$summary_data
#' results$UK$plots$gender_vs_score
#' results$Germany$summary_table
#'
#' # ============================================================
#' # EXAMPLE 6: Equivalence Testing (TOST)
#' # ============================================================
#' results <- compare_groups(
#'   data = data,
#'   category = "gender",
#'   Vars = c("score"),
#'   equivalence = TRUE,
#'   equivalence_bounds = c(-0.3, 0.3)  # Small effect bounds
#' )
#'
#' # Check equivalence conclusions
#' results$summary_data[, c("gender", "mean", "equivalence_conclusion")]
#'
#' # ============================================================
#' # EXAMPLE 7: Combined/Interaction Groups
#' # ============================================================
#' data <- data.frame(
#'   strength = rep(c("Strong", "Weak"), each = 60),
#'   gender = rep(c("Male", "Female"), times = 60),
#'   score = rnorm(120, mean = 100, sd = 15)
#' )
#'
#' # Pass multiple variables to category for interaction groups
#' results <- compare_groups(
#'   data = data,
#'   category = c("strength", "gender"),  # Creates "Strong | Male", "Strong | Female", etc.
#'   Vars = c("score")
#' )
#'
#' # Custom separator
#' results <- compare_groups(
#'   data = data,
#'   category = c("strength", "gender"),
#'   category_sep = " x ",  # Creates "Strong x Male", etc.
#'   Vars = c("score")
#' )
#'
#' # ============================================================
#' # EXAMPLE 8: Pivoted Table (groups as columns)
#' # ============================================================
#' # LLM comparison data
#' data <- data.frame(
#'   Support_level = c(rnorm(100, 4, 1), rnorm(100, 6.5, 1), rnorm(100, 4.8, 1)),
#'   pronoun = rep(c("he/him", "she/her", "they/them"), each = 100),
#'   LA_activity = rep(c("Negative", "Positive"), 150),
#'   llm = rep(c("GPT", "Mistral", "Qwen"), each = 100)
#' )
#'
#' # Basic pivot - groups become columns
#' results <- compare_groups(
#'   data = data,
#'   category = "llm",                    # Becomes columns: GPT | Mistral | Qwen
#'   Vars = "Support_level",
#'   repeat_category = "pronoun",         # Rows: he/him, she/her, they/them
#'   pivot = TRUE                         # Enable pivot mode
#' )
#'
#' # Access the pivot table
#' results$pivot_table
#'
#' # ============================================================
#' # EXAMPLE 9: Pivot with Mean and SD
#' # ============================================================
#' results <- compare_groups(
#'   data = data,
#'   category = "llm",
#'   Vars = "Support_level",
#'   repeat_category = c("pronoun", "LA_activity"),
#'   pivot = TRUE,
#'   pivot_stat = "mean_sd"              # Shows "4.32 (1.21)"
#' )
#'
#' # ============================================================
#' # EXAMPLE 10: Pivot with Post-hoc Comparisons Table
#' # ============================================================
#' results <- compare_groups(
#'   data = data,
#'   category = "llm",
#'   Vars = "Support_level",
#'   repeat_category = "pronoun",
#'   pivot = TRUE,
#'   posthoc = TRUE,
#'   posthoc_table = TRUE,               # Include post-hoc table
#'   posthoc_format = "wide"             # "wide", "long", or "both"
#' )
#'
#' # Access tables
#' results$pivot_table              # Main pivoted means table
#' results$posthoc_pivot_wide       # Post-hoc comparisons (wide format)
#'
#' # ============================================================
#' # EXAMPLE 11: Different P-value Adjustment Methods
#' # ============================================================
#' # Available methods: "none", "bonferroni", "holm", "hochberg",
#' #                   "hommel", "BH", "BY", "fdr" (default)
#' results <- compare_groups(
#'   data = data,
#'   category = "llm",
#'   Vars = "Support_level",
#'   repeat_category = "pronoun",
#'   pivot = TRUE,
#'   p_adjust_method = "bonferroni"
#' )
#' }
#'
#' @seealso
#' \code{\link[ggstatsplot]{ggbetweenstats}} for the underlying plot function
#'
#' @importFrom dplyr group_by summarise mutate filter count select bind_rows any_of case_when ungroup first
#' @importFrom rlang sym .data
#' @importFrom ggplot2 ggplot aes scale_color_manual
#' @importFrom stats t.test aov sd var p.adjust pairwise.t.test pairwise.wilcox.test TukeyHSD as.formula kruskal.test wilcox.test
#'
#' @export
compare_groups <- function(data, category, Vars = NULL,
                                      category_sep = " | ",
                                      repeat_category = NULL,
                                      compare_by = NULL,
                                      compare_mode = c("between", "within"),
                                      repeat_combine = TRUE,
                                      repeat_levels = NULL,
                                      plots = TRUE,
                                      plot_style = c("points", "boxplot", "bar", "violin", "ggstatsplot"),
                                      table = TRUE,
                                      type = "auto",
                                      bayesian = FALSE,
                                      equivalence = FALSE, equivalence_bounds = c(-0.5, 0.5),
                                      nonparametric = FALSE, p_adjust_method = "fdr",
                                      posthoc = TRUE, posthoc_method = "games-howell",
                                      posthoc_table = FALSE,
                                      posthoc_format = c("wide", "long", "both"),
                                      pairwise_display = "significant",
                                      pivot = FALSE,
                                      pivot_by = c("category", "repeat_category"),
                                      pivot_stat = c("mean", "mean_sd", "median", "n"),
                                      pivot_stars = TRUE,
                                      pivot_split_level = TRUE,
                                      min_threshold = 0.05, min_subcategory = 5,
                                      colors = NULL, verbose = TRUE,
                                      combined_table = TRUE,
                                      format = c("gt", "plain", "markdown", "latex", "kable"),
                                      show_header = TRUE,
                                      interpret = FALSE,
                                      ...) {

  # ===========================================================================
  # Input Validation
  # ===========================================================================

  # Capture Vars expression for NSE (e.g., col_a:col_b) - must be done early

  Vars_expr <- substitute(Vars)

  # Check for ggstatsplot (optional dependency)
  has_ggstatsplot <- requireNamespace("ggstatsplot", quietly = TRUE)
  if (plots && !has_ggstatsplot) {
    warning("Package 'ggstatsplot' is not installed. Plots will be disabled.\n",
            "Install with: install.packages('ggstatsplot') (requires R >= 4.3.0)",
            call. = FALSE)
    plots <- FALSE
  }

  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # Validate format parameter
  format <- match.arg(format)

  # Validate compare_mode parameter
  compare_mode <- match.arg(compare_mode)

  # ===========================================================================
  # Handle compare_mode = "within" (test factors within each category level)
  # ===========================================================================
  if (compare_mode == "within") {
    if (is.null(compare_by)) {
      stop("compare_by must be specified when compare_mode = 'within'")
    }

    # Validate pivot_stat for within mode
    pivot_stat <- match.arg(pivot_stat, c("mean", "mean_sd", "median", "n"))

    # Validate compare_by variables exist
    if (!is.character(compare_by)) {
      stop("'compare_by' must be a character vector of variable names")
    }
    missing_vars <- setdiff(compare_by, names(data))
    if (length(missing_vars) > 0) {
      stop("compare_by variable(s) not found in data: ", paste(missing_vars, collapse = ", "))
    }

    # Get category variable name
    if (!is.character(category)) {
      category_name <- deparse(substitute(category))
    } else {
      category_name <- category
    }

    if (!category_name %in% names(data)) {
      stop("Category variable '", category_name, "' not found in data")
    }

    # Get category levels (these become columns/separate datasets)
    category_levels <- unique(data[[category_name]])
    category_levels <- category_levels[!is.na(category_levels)]

    if (verbose) {
      cat("Compare mode: WITHIN\n")
      cat("Testing factors:", paste(compare_by, collapse = ", "), "\n")
      cat("Within each:", category_name, "(", paste(category_levels, collapse = ", "), ")\n\n")
    }

    # Resolve Vars
    Vars <- resolve_cols(data, Vars, cols_expr = Vars_expr, numeric_only = TRUE, exclude = c(category_name, compare_by))

    if (length(Vars) == 0) {
      stop("No numeric variables found to analyze")
    }

    # Determine if nonparametric tests should be used
    use_nonparametric <- nonparametric || tolower(type) %in% c("nonparametric", "np", "kw", "mw", "wilcox", "wilcoxon", "kruskal-wallis", "kruskal", "mann-whitney")

    # Run analysis: for each category level, for each compare_by factor, run ANOVA
    all_results <- list()

    for (outcome_var in Vars) {
      var_results <- list()

      for (factor_var in compare_by) {
        factor_levels <- unique(data[[factor_var]])
        factor_levels <- factor_levels[!is.na(factor_levels)]

        # Collect means and p-values for each category level
        row_data <- list()

        for (cat_level in category_levels) {
          # Subset data for this category level
          subset_data <- data[data[[category_name]] == cat_level & !is.na(data[[category_name]]), ]

          # Get means for each factor level
          for (fac_level in factor_levels) {
            fac_subset <- subset_data[subset_data[[factor_var]] == fac_level & !is.na(subset_data[[factor_var]]), ]
            mean_val <- mean(fac_subset[[outcome_var]], na.rm = TRUE)
            sd_val <- sd(fac_subset[[outcome_var]], na.rm = TRUE)
            n_val <- sum(!is.na(fac_subset[[outcome_var]]))

            key <- paste0(factor_var, "_", fac_level)
            if (is.null(row_data[[key]])) {
              row_data[[key]] <- list(
                factor = factor_var,
                level = fac_level,
                variable = outcome_var
              )
            }
            row_data[[key]][[paste0(cat_level, "_mean")]] <- mean_val
            row_data[[key]][[paste0(cat_level, "_sd")]] <- sd_val
            row_data[[key]][[paste0(cat_level, "_n")]] <- n_val
          }

          # Run ANOVA for this factor within this category level
          if (length(factor_levels) >= 2) {
            # Check if we have enough data
            valid_data <- subset_data[!is.na(subset_data[[outcome_var]]) & !is.na(subset_data[[factor_var]]), ]

            if (nrow(valid_data) >= 3) {
              tryCatch({
                # Determine if nonparametric (resolved_type not yet defined at this point)
                use_nonparametric <- nonparametric || tolower(type) %in% c("nonparametric", "np", "kw", "mw", "wilcox", "wilcoxon", "kruskal-wallis", "kruskal", "mann-whitney")
                if (use_nonparametric) {
                  # Kruskal-Wallis test
                  test_result <- kruskal.test(
                    as.formula(paste0("`", outcome_var, "` ~ `", factor_var, "`")),
                    data = valid_data
                  )
                  p_val <- test_result$p.value
                  # Epsilon-squared effect size for Kruskal-Wallis: H / (n - 1)
                  n_total <- nrow(valid_data)
                  es_val <- as.numeric(test_result$statistic) / (n_total - 1)
                  es_type <- "e2"
                } else {
                  # ANOVA
                  aov_result <- aov(
                    as.formula(paste0("`", outcome_var, "` ~ `", factor_var, "`")),
                    data = valid_data
                  )
                  aov_summary <- summary(aov_result)[[1]]
                  p_val <- aov_summary[["Pr(>F)"]][1]
                  # Eta-squared effect size: SS_between / SS_total
                  ss_between <- aov_summary[["Sum Sq"]][1]
                  ss_total <- sum(aov_summary[["Sum Sq"]])
                  es_val <- ss_between / ss_total
                  es_type <- "n2"
                }

                # Store p-value and effect size for this category level
                for (key in names(row_data)) {
                  if (startsWith(key, paste0(factor_var, "_"))) {
                    row_data[[key]][[paste0(cat_level, "_p")]] <- p_val
                    row_data[[key]][[paste0(cat_level, "_es")]] <- es_val
                    row_data[[key]][[paste0(cat_level, "_es_type")]] <- es_type
                  }
                }
              }, error = function(e) {
                if (verbose) warning("Error in test for ", factor_var, " within ", cat_level, ": ", e$message)
              })
            }
          }
        }

        var_results[[factor_var]] <- row_data
      }

      all_results[[outcome_var]] <- var_results
    }

    # Create pivot table for "within" mode
    within_table <- create_pivot_table_within(
      all_results = all_results,
      category_levels = category_levels,
      compare_by = compare_by,
      pivot_stat = pivot_stat,
      pivot_stars = pivot_stars,
      p_adjust_method = p_adjust_method,
      format = format,
      show_header = show_header,
      verbose = verbose
    )

    # Print within table if verbose
    if (verbose && !is.null(within_table)) {
      cat("\n--- Within-Group Analysis Table ---\n")
      print(within_table)
      cat("\n")
    }

    # Also run the standard "between" comparison (LLM vs LLM)
    if (verbose) {
      cat("\nAlso running standard between-group comparison...\n")
    }

    between_result <- compare_groups(
      data = data,
      category = category_name,
      Vars = Vars,
      compare_by = NULL,
      compare_mode = "between",
      repeat_category = NULL,
      plots = FALSE,
      table = TRUE,
      type = if (use_nonparametric) "np" else type,
      nonparametric = use_nonparametric,
      p_adjust_method = p_adjust_method,
      posthoc = posthoc,
      posthoc_method = posthoc_method,
      pivot = FALSE,
      verbose = FALSE,
      combined_table = TRUE,
      format = format,
      show_header = show_header
    )

    # Return both results
    result <- list(
      summary_table = between_result$summary_table,
      summary_data = between_result$summary_data,
      within_table = within_table,
      raw_results = all_results,
      metadata = list(
        compare_mode = "within",
        category = category_name,
        category_levels = category_levels,
        compare_by = compare_by,
        variables_analyzed = Vars,
        test_type = if (nonparametric) "nonparametric" else "parametric"
      )
    )
    class(result) <- c("comparison_results", "list")
    return(result)
  }

  # Vars will be resolved after category is determined (to exclude it)

  # ===========================================================================
  # Process 'type' parameter
  # ===========================================================================
  # Normalize type parameter
  type <- tolower(type)

  # Map shorthand to full names
  type_mapping <- list(
    # Parametric
    "parametric" = "parametric",
    "p" = "parametric",
    "param" = "parametric",
    "ttest" = "parametric",
    "t" = "parametric",
    "t-test" = "parametric",
    "anova" = "parametric",
    # Nonparametric
    "nonparametric" = "nonparametric",
    "np" = "nonparametric",
    "nonparam" = "nonparametric",
    "mann-whitney" = "nonparametric",
    "mw" = "nonparametric",
    "wilcox" = "nonparametric",
    "wilcoxon" = "nonparametric",
    "kruskal-wallis" = "nonparametric",
    "kruskal" = "nonparametric",
    "kw" = "nonparametric",
    # Bayesian
    "bayes" = "bayes",
    "bayesian" = "bayes",
    "bf" = "bayes",
    # Auto
    "auto" = "auto",
    "automatic" = "auto"
  )

  if (!type %in% names(type_mapping)) {
    stop("type must be one of: 'auto', 'parametric'/'p', 'nonparametric'/'np'/'kw'/'mw', 'bayes'")
  }

  resolved_type <- type_mapping[[type]]

  # Set flags based on resolved type
  if (resolved_type == "nonparametric") {
    nonparametric <- TRUE
  } else if (resolved_type == "bayes") {
    bayesian <- TRUE
    nonparametric <- FALSE
  } else if (resolved_type == "parametric") {
    nonparametric <- FALSE
  }
  # If "auto", use the nonparametric parameter as-is (default FALSE)

  # Validate p_adjust_method
  valid_methods <- c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr")
  if (!p_adjust_method %in% valid_methods) {
    stop("p_adjust_method must be one of: ", paste(valid_methods, collapse = ", "))
  }

  # Validate plot_style
  plot_style <- match.arg(plot_style)

  # Validate pivot parameters
  pivot_stat <- match.arg(pivot_stat)
  pivot_by <- match.arg(pivot_by)
  posthoc_format <- match.arg(posthoc_format)

  # Pivot with multiple variables uses variables as rows
  # repeat_category is optional for pivot

  # Validate equivalence bounds
  if (equivalence) {
    if (length(equivalence_bounds) != 2 || equivalence_bounds[1] >= equivalence_bounds[2]) {
      stop("equivalence_bounds must be a numeric vector of length 2 with lower < upper")
    }
  }

  # ===========================================================================
  # Handle category variable(s)
  # ===========================================================================
  if (missing(category) || is.null(category)) {
    stop("'category' must be specified")
  }
  if (!is.character(category)) {
    stop("'category' must be a character string or vector of variable names")
  }

  # Check that all category variables exist
  missing_cat <- setdiff(category, names(data))
  if (length(missing_cat) > 0) {
    stop("Category variable(s) not found in data: ", paste(missing_cat, collapse = ", "))
  }

  # Store original category variables for reference
  category_vars_orig <- category

  # If multiple category variables, combine them
  if (length(category) > 1) {
    data$.combined_category <- apply(
      data[, category, drop = FALSE], 1,
      function(x) paste(x, collapse = category_sep)
    )
    category_name_str <- ".combined_category"

    if (verbose) {
      n_groups <- length(unique(data$.combined_category[!is.na(data$.combined_category)]))
      cat("Combined", paste(category, collapse = " + "), "into", n_groups, "groups\n\n")
    }
  } else {
    category_name_str <- category
  }

  category_sym <- rlang::sym(category_name_str)

  if (!category_name_str %in% names(data)) {
    stop("Category variable '", category_name_str, "' not found in data")
  }

  # Resolve Vars using flexible specification (exclude category variables)
  # Supports unquoted ranges like Vars = score1:score10
  Vars <- resolve_cols(data, Vars, cols_expr = Vars_expr, numeric_only = TRUE, exclude = category_vars_orig)

  # Handle repeat_category variable(s) (expects character string or vector)
  repeat_category_vars <- NULL
  if (!is.null(repeat_category)) {
    if (!is.character(repeat_category)) {
      stop("'repeat_category' must be a character string or vector of variable names")
    }
    repeat_category_vars <- repeat_category
  }

  # Validate all repeat_category variables exist
  if (!is.null(repeat_category_vars)) {
    missing_vars <- setdiff(repeat_category_vars, names(data))
    if (length(missing_vars) > 0) {
      stop("Repeat category variable(s) not found in data: ", paste(missing_vars, collapse = ", "))
    }

    # Handle multiple repeat_category variables
    if (length(repeat_category_vars) > 1) {
      if (repeat_combine) {
        # Combine into single grouping variable (current behavior)
        data$.repeat_group <- apply(data[, repeat_category_vars, drop = FALSE], 1, paste, collapse = " | ")
        repeat_category_name_str <- ".repeat_group"
        repeat_category_orig_vars <- repeat_category_vars
      } else {
        # Run separate analyses for each repeat_category variable
        if (verbose) {
          cat("Running separate analyses for each repeat_category variable...\n\n")
        }

        all_var_results <- list()
        all_var_summary_data <- list()

        for (rc_var in repeat_category_vars) {
          if (verbose) {
            cat("Analyzing by:", rc_var, "\n")
          }

          # Recursively call with single repeat_category
          var_result <- compare_groups(
            data = data,
            category = category_name_str,
            Vars = Vars,
            category_sep = category_sep,
            repeat_category = rc_var,
            repeat_combine = TRUE,
            repeat_levels = repeat_levels,
            plots = FALSE,  # Disable plots for sub-analyses
            plot_style = plot_style,
            table = table,
            type = type,
            bayesian = bayesian,
            equivalence = equivalence,
            equivalence_bounds = equivalence_bounds,
            nonparametric = nonparametric,
            p_adjust_method = p_adjust_method,
            posthoc = posthoc,
            posthoc_method = posthoc_method,
            posthoc_table = posthoc_table,
            posthoc_format = posthoc_format,
            pairwise_display = pairwise_display,
            pivot = FALSE,  # We'll create our own pivot table
            pivot_by = pivot_by,
            pivot_stat = pivot_stat,
            pivot_stars = pivot_stars,
            pivot_split_level = pivot_split_level,
            min_threshold = min_threshold,
            min_subcategory = min_subcategory,
            colors = colors,
            verbose = FALSE,
            combined_table = FALSE,
            format = "plain",
            show_header = show_header,
            interpret = FALSE
          )

          all_var_results[[rc_var]] <- var_result

          # Collect summary data from each level with factor column
          # Results are structured as: var_result$level_name$summary_data
          for (level_name in names(var_result)) {
            if (level_name != "metadata" && !is.null(var_result[[level_name]]$summary_data)) {
              level_summary <- var_result[[level_name]]$summary_data
              level_summary$factor <- rc_var
              level_summary$level <- level_name
              all_var_summary_data[[paste0(rc_var, "_", level_name)]] <- level_summary
            }
          }
        }

        # Combine all summary data
        if (length(all_var_summary_data) > 0) {
          combined_all <- dplyr::bind_rows(all_var_summary_data)

          # Create pivot table with row grouping
          if (pivot) {
            pivot_result <- create_pivot_table_grouped(
              combined_data = combined_all,
              category_name = category_name_str,
              repeat_category_vars = repeat_category_vars,
              pivot_stat = pivot_stat,
              pivot_stars = pivot_stars,
              p_adjust_method = p_adjust_method,
              comparison_categories = Vars,
              format = format,
              show_header = show_header,
              verbose = verbose
            )
          } else {
            pivot_result <- NULL
          }

          # Return combined results
          result <- list(
            by_variable = all_var_results,
            combined_data = combined_all,
            pivot_table = pivot_result,
            metadata = list(
              repeat_category_vars = repeat_category_vars,
              repeat_combine = FALSE,
              variables_analyzed = Vars
            )
          )
          class(result) <- c("comparison_results", "list")
          return(result)
        } else {
          # No data collected - return empty result
          warning("No data collected from repeat_category analyses")
          result <- list(
            by_variable = all_var_results,
            combined_data = NULL,
            pivot_table = NULL,
            metadata = list(
              repeat_category_vars = repeat_category_vars,
              repeat_combine = FALSE,
              variables_analyzed = Vars
            )
          )
          class(result) <- c("comparison_results", "list")
          return(result)
        }
      }
    } else {
      repeat_category_name_str <- repeat_category_vars
      repeat_category_orig_vars <- repeat_category_vars
    }
  } else {
    repeat_category_name_str <- NULL
    repeat_category_orig_vars <- NULL
  }

  # Set default colors
  if (is.null(colors)) {
    colors <- c("#EF476F", "#26547C", "#FFD166", "#06D6A0", "#118AB2", "#073B4C")
  }

  comparison_categories <- Vars

  # ===========================================================================
  # Check Required Packages
  # ===========================================================================
  required_pkgs <- c("gridExtra")
  if (format == "gt") required_pkgs <- c(required_pkgs, "gt")
  if (bayesian) required_pkgs <- c(required_pkgs, "BayesFactor")
  if (equivalence) required_pkgs <- c(required_pkgs, "TOSTER")
  if (!nonparametric) required_pkgs <- c(required_pkgs, "effsize", "effectsize")

  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required. Install with: install.packages('", pkg, "')")
    }
  }

  # Handle ggstatsplot separately - it's optional

  if (plots && plot_style == "ggstatsplot") {
    if (!requireNamespace("ggstatsplot", quietly = TRUE)) {
      message("\n", strrep("=", 60))
      message("ggstatsplot is not installed.")
      message("Install with: install.packages('ggstatsplot')")
      message("Falling back to plot_style = 'points'")
      message(strrep("=", 60), "\n")
      plot_style <- "points"
    }
  }

  # ===========================================================================
  # Single Analysis Function
  # ===========================================================================
  run_single_analysis <- function(data_subset, group_label = NULL, silent = FALSE) {
    # When silent = TRUE, suppress all printing (for combined mode)
    result <- list()

    # -------------------------------------------------------------------------
    # Filter groups with too few observations
    # -------------------------------------------------------------------------
    if (min_subcategory > 0) {
      subcategory_counts <- data_subset %>%
        dplyr::count(.data[[category_name_str]]) %>%
        dplyr::filter(n >= min_subcategory)

      original_subcats <- unique(data_subset[[category_name_str]])
      kept_subcats <- subcategory_counts[[category_name_str]]
      excluded_subcats <- setdiff(original_subcats, kept_subcats)

      if (verbose && !silent && length(excluded_subcats) > 0) {
        cat("  Excluded groups (<", min_subcategory, " obs):",
            paste(excluded_subcats, collapse = ", "), "\n")
      }

      data_subset <- data_subset %>%
        dplyr::filter(.data[[category_name_str]] %in% kept_subcats)

      if (nrow(data_subset) < 2 || length(unique(data_subset[[category_name_str]])) < 2) {
        if (verbose && !silent) cat("  Warning: Insufficient data after filtering\n")
        return(list(
          plots = NULL, grid_plot = NULL, summary_table = NULL,
          summary_data = NULL, filtered_out = TRUE,
          reason = "Insufficient group data after filtering"
        ))
      }
    }

    # -------------------------------------------------------------------------
    # Initialize storage
    # -------------------------------------------------------------------------
    all_plots <- list()
    summary_list <- list()

    # -------------------------------------------------------------------------
    # Process each variable
    # -------------------------------------------------------------------------
    for (comp_cat_name in comparison_categories) {
      plot_title <- if (is.null(group_label)) {
        paste0(category_name_str, " vs ", comp_cat_name)
      } else {
        paste0(category_name_str, " vs ", comp_cat_name, " (", group_label, ")")
      }

      comp_cat_sym <- rlang::sym(comp_cat_name)

      # -----------------------------------------------------------------------
      # Create Plot
      # -----------------------------------------------------------------------
      if (plots) {
        tryCatch({
          if (nrow(data_subset) < 2) {
            warning("Insufficient data for plot: ", plot_title)
            next
          }

          n_groups <- length(unique(data_subset[[category_name_str]]))
          if (n_groups < 2) {
            warning("Need at least 2 groups for: ", plot_title)
            next
          }

          if (all(is.na(data_subset[[comp_cat_name]]))) {
            warning("All NA values for: ", comp_cat_name)
            next
          }

          # Create plot based on style
          if (plot_style == "ggstatsplot") {
            # Original ggstatsplot style
            if (nonparametric) {
              test_type <- "nonparametric"
            } else if (bayesian) {
              test_type <- "bayes"
            } else {
              test_type <- "parametric"
            }

            p <- ggstatsplot::ggbetweenstats(
              data = data_subset,
              x = !!category_sym,
              y = !!comp_cat_sym,
              type = test_type,
              bf.message = bayesian,
              pairwise.display = if (n_groups > 2 && posthoc) pairwise_display else "none",
              p.adjust.method = if (p_adjust_method == "none") "bonferroni" else p_adjust_method,
              xlab = "",
              title = "",
              conf.level = 0.95
            ) +
              ggplot2::scale_color_manual(values = colors[1:n_groups])

          } else {
            # Clean modern styles
            # Filter out NA values for plotting
            plot_data <- data_subset[!is.na(data_subset[[comp_cat_name]]), ]

            p <- switch(plot_style,
              "points" = plot_points_style(
                data = plot_data,
                x_var = category_name_str,
                y_var = comp_cat_name,
                colors = colors
              ),
              "boxplot" = plot_boxplot_style(
                data = plot_data,
                x_var = category_name_str,
                y_var = comp_cat_name,
                colors = colors
              ),
              "bar" = plot_bar_style(
                data = plot_data,
                x_var = category_name_str,
                y_var = comp_cat_name,
                colors = colors
              ),
              "violin" = plot_violin_style(
                data = plot_data,
                x_var = category_name_str,
                y_var = comp_cat_name,
                colors = colors
              )
            )
          }

          plot_name <- paste0(category_name_str, "_vs_", comp_cat_name)
          all_plots[[plot_name]] <- p
        }, error = function(e) {
          warning("Error creating plot for ", plot_title, ": ", e$message)
        })
      }

      # -----------------------------------------------------------------------
      # Calculate Statistics
      # -----------------------------------------------------------------------
      if (table) {
        tryCatch({
          if (nrow(data_subset) < 2 || all(is.na(data_subset[[comp_cat_name]]))) {
            next
          }

          # Basic summary statistics
          summary_stats <- data_subset %>%
            dplyr::group_by(!!category_sym) %>%
            dplyr::summarise(
              mean = mean(!!comp_cat_sym, na.rm = TRUE),
              sd = stats::sd(!!comp_cat_sym, na.rm = TRUE),
              n = dplyr::n(),
              .groups = 'drop'
            ) %>%
            dplyr::mutate(
              variable = comp_cat_name,
              category = category_name_str
            )

          # Initialize test results
          p_value <- NA
          efsz <- NA
          ef_type <- NA
          t_statistic <- NA
          t_df <- NA
          f_statistic <- NA
          df1 <- NA
          df2 <- NA
          test_type <- NA
          bf10 <- NA
          bf_interpretation <- NA
          tost_p <- NA
          equivalence_conclusion <- NA

          n_groups <- length(unique(data_subset[[category_name_str]]))

          # ===================================================================
          # Two-group comparisons
          # ===================================================================
          if (n_groups == 2 && nrow(data_subset) > 2) {
            groups <- unique(data_subset[[category_name_str]])
            group1_data <- data_subset[data_subset[[category_name_str]] == groups[1], comp_cat_name]
            group2_data <- data_subset[data_subset[[category_name_str]] == groups[2], comp_cat_name]

            group1_data <- group1_data[!is.na(group1_data)]
            group2_data <- group2_data[!is.na(group2_data)]

            if (length(group1_data) > 1 && length(group2_data) > 1) {

              # Parametric/Nonparametric test
              if (nonparametric) {
                tryCatch({
                  test_result <- stats::wilcox.test(group1_data, group2_data)
                  p_value <- test_result$p.value
                  t_statistic <- as.numeric(test_result$statistic)  # W statistic
                  test_type <- "Mann-Whitney U"

                  # Rank-biserial correlation as effect size
                  n1 <- length(group1_data)
                  n2 <- length(group2_data)
                  U <- test_result$statistic
                  efsz <- 1 - (2 * U) / (n1 * n2)  # Rank-biserial
                  ef_type <- "r_rb"
                }, error = function(e) {
                  warning("Error in Mann-Whitney test: ", e$message)
                })
              } else {
                tryCatch({
                  test_result <- stats::t.test(group1_data, group2_data)
                  p_value <- test_result$p.value
                  t_statistic <- as.numeric(test_result$statistic)
                  t_df <- as.numeric(test_result$parameter)
                  test_type <- "t-test"
                  efsz <- effsize::cohen.d(group1_data, group2_data)$estimate
                  ef_type <- "Cohen's d"
                }, error = function(e) {
                  warning("Error in t-test: ", e$message)
                })
              }

              # Bayesian t-test
              if (bayesian && !nonparametric) {
                tryCatch({
                  bf_result <- BayesFactor::ttestBF(group1_data, group2_data)
                  # Extract BF from the S4 object properly
                  bf_extracted <- BayesFactor::extractBF(bf_result)
                  bf10 <- bf_extracted$bf[1]
                  bf_interpretation <- interpret_bf(bf10)
                }, error = function(e) {
                  warning("Error in Bayesian t-test: ", e$message)
                })
              }

              # Equivalence test (TOST)
              if (equivalence && !nonparametric) {
                tryCatch({
                  psd <- pooled_sd(group1_data, group2_data)
                  low_bound <- equivalence_bounds[1] * psd
                  high_bound <- equivalence_bounds[2] * psd

                  # Use dataTOSTtwo if available, otherwise tsum_TOST
                  tost_result <- TOSTER::tsum_TOST(
                    m1 = mean(group1_data),
                    m2 = mean(group2_data),
                    sd1 = stats::sd(group1_data),
                    sd2 = stats::sd(group2_data),
                    n1 = length(group1_data),
                    n2 = length(group2_data),
                    low_eqbound = low_bound,
                    high_eqbound = high_bound,
                    alpha = 0.05,
                    eqbound_type = "raw"
                  )

                  # Get the larger TOST p-value
                  tost_p <- max(tost_result$TOST$p.value, na.rm = TRUE)

                  if (tost_p < 0.05) {
                    equivalence_conclusion <- "Equivalent"
                  } else if (!is.na(p_value) && p_value < 0.05) {
                    equivalence_conclusion <- "Different"
                  } else {
                    equivalence_conclusion <- "Inconclusive"
                  }
                }, error = function(e) {
                  warning("Error in equivalence test: ", e$message)
                })
              }
            }
          }

          # ===================================================================
          # Multi-group comparisons (>2 groups)
          # ===================================================================
          else if (n_groups > 2 && nrow(data_subset) > 2) {
            # Storage for ANOVA details
            anova_details <- list()
            posthoc_results <- NULL
            anova_text_report <- NULL

            if (nonparametric) {
              tryCatch({
                test_result <- stats::kruskal.test(
                  data_subset[[comp_cat_name]] ~ data_subset[[category_name_str]]
                )
                p_value <- test_result$p.value
                test_type <- "Kruskal-Wallis"

                # Epsilon squared effect size
                H <- test_result$statistic
                f_statistic <- as.numeric(H)  # H statistic
                df1 <- n_groups - 1
                n <- nrow(data_subset)
                efsz <- H / (n - 1)  # Epsilon squared
                ef_type <- "epsilon_sq"

                # Store ANOVA details
                anova_details <- list(
                  test = "Kruskal-Wallis",
                  statistic = H,
                  df = n_groups - 1,
                  p_value = p_value,
                  effect_size = efsz,
                  effect_type = "epsilon_sq"
                )

                # Post-hoc: Pairwise Wilcoxon tests
                if (posthoc) {
                  ph_method <- if (p_adjust_method == "none") "bonferroni" else p_adjust_method
                  posthoc_raw <- stats::pairwise.wilcox.test(
                    data_subset[[comp_cat_name]],
                    data_subset[[category_name_str]],
                    p.adjust.method = ph_method
                  )

                  # Convert to data frame
                  posthoc_results <- reshape_posthoc_matrix(posthoc_raw$p.value, comp_cat_name, ph_method)
                }
              }, error = function(e) {
                warning("Error in Kruskal-Wallis test: ", e$message)
              })
            } else {
              tryCatch({
                # Fit ANOVA
                formula_aov <- stats::as.formula(paste(comp_cat_name, "~", category_name_str))
                aov_result <- stats::aov(formula_aov, data = data_subset)
                aov_summary <- summary(aov_result)[[1]]

                p_value <- aov_summary[["Pr(>F)"]][1]
                f_statistic <- aov_summary[["F value"]][1]
                df1 <- aov_summary[["Df"]][1]
                df2 <- aov_summary[["Df"]][2]
                test_type <- "ANOVA"

                # Extract eta squared properly
                es_result <- effectsize::eta_squared(aov_result)
                efsz <- es_result$Eta2[1]
                ef_type <- "eta_sq"

                # Effect size interpretation
                ef_interp <- if (efsz >= 0.14) "large" else if (efsz >= 0.06) "medium" else "small"

                # Store ANOVA details
                anova_details <- list(
                  test = "One-way ANOVA",
                  statistic = f_statistic,
                  df1 = df1,
                  df2 = df2,
                  p_value = p_value,
                  effect_size = efsz,
                  effect_type = "eta_sq",
                  effect_interp = ef_interp
                )

                # Post-hoc comparisons
                if (posthoc) {
                  ph_method <- if (p_adjust_method == "none") "bonferroni" else p_adjust_method

                  if (posthoc_method == "tukey") {
                    # Tukey HSD
                    tukey_result <- stats::TukeyHSD(aov_result)
                    tukey_df <- as.data.frame(tukey_result[[1]])
                    tukey_df$comparison <- rownames(tukey_df)

                    posthoc_results <- data.frame(
                      variable = comp_cat_name,
                      comparison = tukey_df$comparison,
                      diff = tukey_df$diff,
                      ci_lower = tukey_df$lwr,
                      ci_upper = tukey_df$upr,
                      p_adj = tukey_df$`p adj`,
                      method = "Tukey HSD",
                      stringsAsFactors = FALSE
                    )
                  } else {
                    # Games-Howell (using pairwise t-test as approximation with pooled=FALSE)
                    posthoc_raw <- stats::pairwise.t.test(
                      data_subset[[comp_cat_name]],
                      data_subset[[category_name_str]],
                      p.adjust.method = ph_method,
                      pool.sd = FALSE  # Games-Howell doesn't assume equal variances
                    )
                    posthoc_results <- reshape_posthoc_matrix(posthoc_raw$p.value, comp_cat_name, ph_method)

                    # Add mean differences
                    group_means <- tapply(data_subset[[comp_cat_name]],
                                         data_subset[[category_name_str]], mean, na.rm = TRUE)
                    posthoc_results$diff <- sapply(1:nrow(posthoc_results), function(i) {
                      groups <- strsplit(posthoc_results$comparison[i], " vs ")[[1]]
                      if (length(groups) == 2 && all(groups %in% names(group_means))) {
                        group_means[groups[1]] - group_means[groups[2]]
                      } else NA
                    })
                  }

                  # Add significance stars
                  posthoc_results$significance <- sapply(posthoc_results$p_adj, function(p) {
                    if (is.na(p)) ""
                    else if (p < 0.001) "***"
                    else if (p < 0.01) "**"
                    else if (p < 0.05) "*"
                    else ""
                  })
                }

                # Generate text report
                if (posthoc && !is.null(posthoc_results)) {
                  anova_text_report <- generate_anova_text_report(
                    variable = comp_cat_name,
                    category = category_name_str,
                    data = data_subset,
                    anova_details = anova_details,
                    posthoc_results = posthoc_results
                  )
                }

              }, error = function(e) {
                warning("Error in ANOVA: ", e$message)
              })
            }

            # Store extra results for this variable
            if (exists("anova_details") && length(anova_details) > 0) {
              summary_stats$anova_details <- list(anova_details)
            }
            if (!is.null(posthoc_results)) {
              summary_stats$posthoc_results <- list(posthoc_results)
            }
            if (!is.null(anova_text_report)) {
              summary_stats$anova_report <- anova_text_report
            }
          }

          # Add results to summary
          summary_stats$p_value <- p_value
          summary_stats$efsz <- efsz
          summary_stats$ef_type <- ef_type
          summary_stats$test_type <- test_type
          summary_stats$t_statistic <- t_statistic
          summary_stats$t_df <- t_df
          summary_stats$f_statistic <- f_statistic
          summary_stats$df1 <- df1
          summary_stats$df2 <- df2
          summary_stats$bf10 <- bf10
          summary_stats$bf_interpretation <- bf_interpretation
          summary_stats$tost_p <- tost_p
          summary_stats$equivalence_conclusion <- equivalence_conclusion

          summary_list[[comp_cat_name]] <- summary_stats
        }, error = function(e) {
          warning("Error calculating statistics for ", comp_cat_name, ": ", e$message)
        })
      }
    }

    # -------------------------------------------------------------------------
    # Combine summary data (more efficient than rbind in loop)
    # -------------------------------------------------------------------------
    summary_data <- if (length(summary_list) > 0) {
      dplyr::bind_rows(summary_list)
    } else {
      data.frame()
    }

    # -------------------------------------------------------------------------
    # Apply p-value adjustment
    # -------------------------------------------------------------------------
    if (p_adjust_method != "none" && nrow(summary_data) > 0) {
      # Get unique p-values (one per variable)
      p_values_by_var <- summary_data %>%
        dplyr::group_by(variable) %>%
        dplyr::summarise(p_value = dplyr::first(p_value), .groups = 'drop')

      adjusted_p <- stats::p.adjust(p_values_by_var$p_value, method = p_adjust_method)

      # Merge back
      p_values_by_var$p_adjusted <- adjusted_p
      summary_data <- merge(summary_data, p_values_by_var[, c("variable", "p_adjusted")],
                           by = "variable", all.x = TRUE)
    }

    # -------------------------------------------------------------------------
    # Create Plots Grid (only when not silent)
    # -------------------------------------------------------------------------
    if (plots && length(all_plots) > 0) {
      result$plots <- all_plots

      # Create grid plot (use arrangeGrob to avoid immediate display)
      n_plots <- length(all_plots)
      n_cols <- if (n_plots == 1) 1 else if (n_plots <= 4) 2 else if (n_plots <= 6) 3 else 4

      tryCatch({
        valid_plots <- all_plots[sapply(all_plots, function(x) {
          !is.null(x) && inherits(x, "ggplot")
        })]

        if (length(valid_plots) > 0) {
          # Use arrangeGrob (doesn't display) instead of grid.arrange
          result$grid_plot <- do.call(
            gridExtra::arrangeGrob,
            c(valid_plots, ncol = n_cols, top = group_label)
          )
          # Only display when not silent
          if (!silent && verbose) {
            gridExtra::grid.arrange(result$grid_plot)
          }
        }
      }, error = function(e) {
        warning("Error creating grid plot: ", e$message)
        result$grid_plot <- NULL
      })
    }

    # -------------------------------------------------------------------------
    # Create Summary Table
    # -------------------------------------------------------------------------
    if (table && nrow(summary_data) > 0) {
      result$summary_data <- summary_data

      tryCatch({
        formatted_table <- summary_data %>%
          dplyr::group_by(variable) %>%
          dplyr::mutate(
            mean_formatted = sprintf("%.2f", mean),
            sd_formatted = sprintf("%.2f", sd),
            ES_formatted = ifelse(is.na(efsz), "NA", sprintf("%.2f", efsz)),
            is_highest = mean == max(mean, na.rm = TRUE),
            p_formatted = dplyr::case_when(
              is.na(dplyr::first(p_value[!is.na(p_value)])) ~ "NA",
              dplyr::first(p_value[!is.na(p_value)]) < 0.001 ~ "< 0.001",
              TRUE ~ sprintf("%.3f", dplyr::first(p_value[!is.na(p_value)]))
            )
          ) %>%
          dplyr::ungroup()

        # Add adjusted p-values if applicable
        if ("p_adjusted" %in% names(summary_data)) {
          formatted_table <- formatted_table %>%
            dplyr::group_by(variable) %>%
            dplyr::mutate(
              p_adj_formatted = dplyr::case_when(
                is.na(dplyr::first(p_adjusted[!is.na(p_adjusted)])) ~ "NA",
                dplyr::first(p_adjusted[!is.na(p_adjusted)]) < 0.001 ~ "< 0.001",
                TRUE ~ sprintf("%.3f", dplyr::first(p_adjusted[!is.na(p_adjusted)]))
              )
            ) %>%
            dplyr::ungroup()
        }

        # Add Bayesian columns
        if (bayesian && "bf10" %in% names(summary_data)) {
          formatted_table <- formatted_table %>%
            dplyr::group_by(variable) %>%
            dplyr::mutate(
              bf_formatted = dplyr::case_when(
                is.na(dplyr::first(bf10[!is.na(bf10)])) ~ "NA",
                dplyr::first(bf10[!is.na(bf10)]) > 1000 ~ "> 1000",
                dplyr::first(bf10[!is.na(bf10)]) < 0.001 ~ "< 0.001",
                TRUE ~ sprintf("%.2f", dplyr::first(bf10[!is.na(bf10)]))
              ),
              bf_interp_formatted = dplyr::first(bf_interpretation[!is.na(bf_interpretation)])
            ) %>%
            dplyr::ungroup()
        }

        # Add equivalence columns
        if (equivalence && "tost_p" %in% names(summary_data)) {
          formatted_table <- formatted_table %>%
            dplyr::group_by(variable) %>%
            dplyr::mutate(
              tost_formatted = dplyr::case_when(
                is.na(dplyr::first(tost_p[!is.na(tost_p)])) ~ "NA",
                dplyr::first(tost_p[!is.na(tost_p)]) < 0.001 ~ "< 0.001",
                TRUE ~ sprintf("%.3f", dplyr::first(tost_p[!is.na(tost_p)]))
              ),
              equiv_concl_formatted = dplyr::first(equivalence_conclusion[!is.na(equivalence_conclusion)])
            ) %>%
            dplyr::ungroup()
        }

        # Build display columns
        display_cols <- c("Variable" = "variable")
        display_cols[category_name_str] <- category_name_str
        display_cols <- c(display_cols,
                         "Mean" = "mean_formatted",
                         "SD" = "sd_formatted",
                         "N" = "n",
                         "ES" = "ES_formatted",
                         "P-value" = "p_formatted")

        if ("p_adj_formatted" %in% names(formatted_table)) {
          display_cols <- c(display_cols, "P-adj" = "p_adj_formatted")
        }

        if (bayesian && "bf_formatted" %in% names(formatted_table)) {
          display_cols <- c(display_cols,
                           "BF10" = "bf_formatted",
                           "BF Interpretation" = "bf_interp_formatted")
        }

        if (equivalence && "tost_formatted" %in% names(formatted_table)) {
          display_cols <- c(display_cols,
                           "TOST p" = "tost_formatted",
                           "Equivalence" = "equiv_concl_formatted")
        }

        # Create final table - select by OLD column names (values of display_cols)
        final_table <- formatted_table %>%
          dplyr::select(dplyr::any_of(c(unname(display_cols), "is_highest")))

        # Rename columns
        col_names <- names(final_table)
        for (i in seq_along(display_cols)) {
          col_names[col_names == display_cols[i]] <- names(display_cols)[i]
        }
        names(final_table) <- col_names

        # Build subtitle
        test_info <- if (nonparametric) "Nonparametric tests" else "Parametric tests"
        subtitle_parts <- c(test_info)
        if (bayesian) subtitle_parts <- c(subtitle_parts, "Bayesian analysis")
        if (equivalence) subtitle_parts <- c(subtitle_parts,
                                             paste0("TOST (", equivalence_bounds[1],
                                                    " to ", equivalence_bounds[2], ")"))
        if (p_adjust_method != "none") subtitle_parts <- c(subtitle_parts,
                                                           paste0("p-adjust: ", p_adjust_method))

        table_title <- if (is.null(group_label)) {
          paste0("Summary: ", category_name_str, " Comparisons")
        } else {
          paste0("Summary: ", category_name_str, " (", group_label, ")")
        }
        table_subtitle <- paste(subtitle_parts, collapse = " | ")

        # Prepare display table (without internal columns)
        display_table <- final_table %>% dplyr::select(-is_highest)

        # ==================================================================
        # Handle non-GT formats using format_table utility
        # ==================================================================
        if (format %in% c("plain", "markdown", "latex", "kable")) {
          result$summary_table <- format_table(
            df = display_table,
            format = format,
            title = if (show_header) table_title else NULL,
            subtitle = if (show_header) table_subtitle else NULL,
            show_header = show_header,
            bold_cols = "Variable",
            align_left = "Variable"
          )

          if (verbose && !silent) {
            cat("\n")
            print(result$summary_table)
            cat("\n")
          }
        } else {
          # ==================================================================
          # Create gt table (default)
          # ==================================================================
          gt_table <- display_table %>%
            gt::gt()

          # Add header (respecting show_header)
          if (show_header) {
            gt_table <- gt_table %>%
              gt::tab_header(
                title = table_title,
                subtitle = table_subtitle
              )
          }

          gt_table <- gt_table %>%
            gt::tab_style(
              style = gt::cell_text(weight = "bold"),
              locations = gt::cells_column_labels()
            ) %>%
            gt::tab_style(
              style = gt::cell_text(weight = "bold"),
              locations = gt::cells_body(columns = "Variable")
            ) %>%
            gt::cols_align(align = "center") %>%
            gt::tab_options(
              table.font.size = gt::px(12),
              heading.title.font.size = gt::px(14),
              heading.subtitle.font.size = gt::px(11),
              column_labels.font.weight = "bold"
            )

          # Highlight significant p-values
          if ("P-value" %in% names(final_table)) {
            sig_rows <- which(
              final_table$`P-value` == "< 0.001" |
                (suppressWarnings(as.numeric(final_table$`P-value`)) < 0.05 &
                   !is.na(suppressWarnings(as.numeric(final_table$`P-value`))))
            )
            if (length(sig_rows) > 0) {
              gt_table <- gt_table %>%
                gt::tab_style(
                  style = gt::cell_text(color = "red"),
                  locations = gt::cells_body(columns = "P-value", rows = sig_rows)
                )
            }
          }

          # Highlight highest means
          highest_rows <- which(final_table$is_highest)
          if (length(highest_rows) > 0) {
            # Get columns that exist in the table for highlighting
            highlight_cols <- intersect(c(category_name_str, "Mean", "SD", "N"),
                                        names(final_table))
            if (length(highlight_cols) > 0) {
              gt_table <- gt_table %>%
                gt::tab_style(
                  style = gt::cell_text(weight = "bold"),
                  locations = gt::cells_body(
                    columns = highlight_cols,
                    rows = highest_rows
                  )
                )
            }
          }

          result$summary_table <- gt_table

          if (verbose && !silent) {
            cat("\n")
            print(result$summary_table)
            cat("\n")
          }
        }
      }, error = function(e) {
        warning("Error creating table: ", e$message)
        result$summary_table <- summary_data
        if (verbose && !silent) print(summary_data)
      })
    }

    return(result)
  }

  # ===========================================================================
  # Main Logic
  # ===========================================================================
  if (is.null(repeat_category_name_str)) {
    # No repeat category - single analysis
    result <- run_single_analysis(data)

    # Create pivot table if requested (variables as rows, category as columns)
    if (pivot && !is.null(result$summary_data)) {
      tryCatch({
        result$pivot_table <- create_pivot_table_simple(
          summary_data = result$summary_data,
          category_name = category_name_str,
          pivot_stat = pivot_stat,
          pivot_stars = pivot_stars,
          p_adjust_method = p_adjust_method,
          format = format,
          show_header = show_header,
          verbose = verbose
        )
        # Print pivot table if verbose
        if (verbose && !is.null(result$pivot_table)) {
          cat("\n--- Pivot Table ---\n")
          print(result$pivot_table)
          cat("\n")
        }
      }, error = function(e) {
        if (verbose) warning("Could not create pivot table: ", e$message)
      })
    }

    # AI interpretation if requested
    if (interpret) {
      n_groups <- length(unique(data[[category_name_str]]))
      test_type_str <- if (nonparametric) {
        if (n_groups == 2) "Mann-Whitney U" else "Kruskal-Wallis"
      } else {
        if (n_groups == 2) "t-test" else "ANOVA"
      }

      metadata <- list(
        test_type = test_type_str,
        n_groups = n_groups,
        variables = comparison_categories,
        grouping_var = category_name_str,
        groups = unique(data[[category_name_str]]),
        total_n = nrow(data),
        posthoc_method = posthoc_method
      )

      interpret_with_ai(result, analysis_type = "group_comparison", metadata = metadata, ...)
    }

    return(result)
  } else {
    # Stratified analysis
    total_n <- nrow(data)
    category_counts <- data %>%
      dplyr::count(.data[[repeat_category_name_str]]) %>%
      dplyr::mutate(
        percentage = n / total_n,
        above_threshold = percentage >= min_threshold
      ) %>%
      dplyr::arrange(dplyr::desc(percentage))

    # If repeat_levels specified, use those; otherwise use threshold filtering
    if (!is.null(repeat_levels)) {
      # Validate that specified levels exist in data
      available_levels <- unique(data[[repeat_category_name_str]])
      invalid_levels <- setdiff(repeat_levels, available_levels)
      if (length(invalid_levels) > 0) {
        warning("repeat_levels not found in data: ", paste(invalid_levels, collapse = ", "))
      }
      categories_to_include <- intersect(repeat_levels, available_levels)
      excluded_categories <- setdiff(available_levels, categories_to_include)
    } else {
      categories_to_include <- category_counts %>%
        dplyr::filter(above_threshold) %>%
        dplyr::pull(.data[[repeat_category_name_str]])

      excluded_categories <- category_counts %>%
        dplyr::filter(!above_threshold) %>%
        dplyr::pull(.data[[repeat_category_name_str]])
    }

    if (length(categories_to_include) == 0) {
      warning("No categories to analyze. Check repeat_levels or min_threshold.")
      return(list())
    }

    # Run analysis for each category (suppress individual output when combined_table = TRUE)
    results_by_group <- list()
    successful <- 0
    all_summary_data <- list()
    all_plots <- list()
    all_stats <- list()

    for (repeat_val in categories_to_include) {
      filtered_data <- data %>%
        dplyr::filter(.data[[repeat_category_name_str]] == repeat_val)

      # Run analysis silently when combined_table = TRUE
      analysis_result <- run_single_analysis(filtered_data, repeat_val, silent = combined_table)

      if (is.null(analysis_result$filtered_out) || !analysis_result$filtered_out) {
        successful <- successful + 1
        # Collect summary data for combined table
        if (!is.null(analysis_result$summary_data) && nrow(analysis_result$summary_data) > 0) {
          analysis_result$summary_data[[repeat_category_name_str]] <- repeat_val
          all_summary_data[[as.character(repeat_val)]] <- analysis_result$summary_data

          # Collect statistics for clean output
          for (var in comparison_categories) {
            var_data <- analysis_result$summary_data[analysis_result$summary_data$variable == var, ]
            if (nrow(var_data) > 0) {
              p_val <- var_data$p_value[1]
              ef <- var_data$efsz[1]
              ef_type <- var_data$ef_type[1]
              n_total <- sum(var_data$n)
              ttype <- var_data$test_type[1]
              t_stat <- var_data$t_statistic[1]
              t_df_val <- var_data$t_df[1]
              f_stat <- var_data$f_statistic[1]
              df1_val <- var_data$df1[1]
              df2_val <- var_data$df2[1]

              # Get post-hoc results if available
              posthoc_res <- NULL
              if (!is.null(var_data$posthoc_results) && length(var_data$posthoc_results) > 0) {
                posthoc_res <- var_data$posthoc_results[[1]]
              }

              all_stats[[length(all_stats) + 1]] <- list(
                group = repeat_val,
                variable = var,
                n = n_total,
                test_type = ttype,
                t_statistic = t_stat,
                t_df = t_df_val,
                f_statistic = f_stat,
                df1 = df1_val,
                df2 = df2_val,
                p_value = p_val,
                effect_size = ef,
                effect_type = ef_type,
                posthoc = posthoc_res
              )
            }
          }
        }

        # Collect plots for combined grid
        if (!is.null(analysis_result$plots)) {
          for (plot_name in names(analysis_result$plots)) {
            new_name <- paste0(repeat_val, "_", plot_name)
            all_plots[[new_name]] <- analysis_result$plots[[plot_name]]
          }
        }
      }

      results_by_group[[as.character(repeat_val)]] <- analysis_result
    }

    # Create combined table if requested
    if (combined_table && length(all_summary_data) > 0) {
      combined_data <- dplyr::bind_rows(all_summary_data)

      # Create combined formatted table
      tryCatch({
        formatted_combined <- combined_data %>%
          dplyr::group_by(.data[[repeat_category_name_str]], variable) %>%
          dplyr::mutate(
            mean_formatted = sprintf("%.2f", mean),
            sd_formatted = sprintf("%.2f", sd),
            ES_formatted = ifelse(is.na(efsz), "NA", sprintf("%.2f", efsz)),
            is_highest = mean == max(mean, na.rm = TRUE),
            p_formatted = dplyr::case_when(
              is.na(dplyr::first(p_value[!is.na(p_value)])) ~ "NA",
              dplyr::first(p_value[!is.na(p_value)]) < 0.001 ~ "< 0.001",
              TRUE ~ sprintf("%.3f", dplyr::first(p_value[!is.na(p_value)]))
            )
          ) %>%
          dplyr::ungroup()

        # Determine display name for repeat_category column
        repeat_col_display <- if (length(repeat_category_orig_vars) > 1) {
          paste(repeat_category_orig_vars, collapse = " x ")
        } else {
          repeat_category_name_str
        }

        # Build display table
        display_combined <- formatted_combined %>%
          dplyr::select(
            dplyr::all_of(c(repeat_category_name_str, "variable", category_name_str)),
            Mean = mean_formatted,
            SD = sd_formatted,
            N = n,
            ES = ES_formatted,
            `P-value` = p_formatted,
            is_highest
          )

        # Rename columns properly
        names(display_combined)[1] <- repeat_col_display
        names(display_combined)[2] <- "Variable"
        names(display_combined)[3] <- category_name_str

        # Identify rows with highest mean for bold formatting
        highest_rows <- which(display_combined$is_highest)

        # Identify significant p-values for red coloring
        sig_rows <- which(
          display_combined$`P-value` == "< 0.001" |
            (suppressWarnings(as.numeric(display_combined$`P-value`)) < 0.05 &
               !is.na(suppressWarnings(as.numeric(display_combined$`P-value`))))
        )

        # Build subtitle
        test_info <- if (nonparametric) "Nonparametric tests" else "Parametric tests"
        subtitle_parts <- c(test_info)
        if (bayesian) subtitle_parts <- c(subtitle_parts, "Bayesian analysis")
        if (p_adjust_method != "none") subtitle_parts <- c(subtitle_parts, paste0("p-adjust: ", p_adjust_method))

        # Create gt table
        gt_combined <- display_combined %>%
          dplyr::select(-is_highest) %>%
          gt::gt() %>%
          gt::tab_header(
            title = paste0("Comparison: ", category_name_str, " by ", repeat_col_display),
            subtitle = paste(subtitle_parts, collapse = " | ")
          ) %>%
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
          gt::cols_align(align = "center") %>%
          gt::cols_align(align = "left", columns = c(repeat_col_display, "Variable")) %>%
          gt::tab_options(
            table.border.top.width = gt::px(2),
            table.border.top.color = "black",
            table.border.bottom.width = gt::px(2),
            table.border.bottom.color = "black",
            column_labels.border.bottom.color = "black",
            table_body.border.bottom.color = "black"
          )

        # Bold highest mean rows
        if (length(highest_rows) > 0) {
          gt_combined <- gt_combined %>%
            gt::tab_style(
              style = gt::cell_text(weight = "bold"),
              locations = gt::cells_body(rows = highest_rows)
            )
        }

        # Red significant p-values
        if (length(sig_rows) > 0) {
          gt_combined <- gt_combined %>%
            gt::tab_style(
              style = gt::cell_text(color = "red"),
              locations = gt::cells_body(columns = "P-value", rows = sig_rows)
            )
        }

        results_by_group$combined_table <- gt_combined
        results_by_group$combined_data <- combined_data
      }, error = function(e) {
        warning("Error creating combined table: ", e$message)
      })
    }

    # =========================================================================
    # Create PIVOT table if requested
    # =========================================================================
    if (pivot && length(all_summary_data) > 0) {
      combined_data <- dplyr::bind_rows(all_summary_data)

      tryCatch({
        # Use simple pivot if no repeat_category, otherwise use full pivot
        if (is.null(repeat_category_name_str)) {
          # Simple pivot: variables as rows, category as columns
          results_by_group$pivot_table <- create_pivot_table_simple(
            summary_data = combined_data,
            category_name = category_name_str,
            pivot_stat = pivot_stat,
            pivot_stars = pivot_stars,
            p_adjust_method = p_adjust_method,
            format = format,
            show_header = show_header,
            verbose = verbose
          )
        } else {
          # Full pivot with repeat_category
          # Debug: check that required columns exist
          required_cols <- c(category_name_str, repeat_category_name_str, "variable", "mean", "sd", "n", "p_value")
          missing_cols <- setdiff(required_cols, names(combined_data))
          if (length(missing_cols) > 0 && verbose) {
            warning("Pivot table: missing columns in combined_data: ", paste(missing_cols, collapse = ", "))
          }

          results_by_group$pivot_table <- create_pivot_table(
            combined_data = combined_data,
            category_name = category_name_str,
            repeat_category_name = repeat_category_name_str,
            repeat_category_orig_vars = repeat_category_orig_vars,
            pivot_by = pivot_by,
            pivot_stat = pivot_stat,
            pivot_stars = pivot_stars,
            pivot_split_level = pivot_split_level,
            p_adjust_method = p_adjust_method,
            comparison_categories = comparison_categories,
            format = format,
            show_header = show_header,
            verbose = verbose
          )
        }

        # Print pivot table if verbose
        if (verbose && !is.null(results_by_group$pivot_table)) {
          cat("\n--- Pivot Table ---\n")
          print(results_by_group$pivot_table)
          cat("\n")
        }

        # Create post-hoc pivot tables if posthoc_table is TRUE
        if (posthoc_table && posthoc) {
          # Collect all post-hoc data
          all_posthoc_data <- data.frame()
          for (stat in all_stats) {
            if (!is.null(stat$posthoc) && nrow(stat$posthoc) > 0) {
              ph_data <- stat$posthoc
              ph_data[[repeat_category_name_str]] <- stat$group
              all_posthoc_data <- rbind(all_posthoc_data, ph_data)
            }
          }

          if (nrow(all_posthoc_data) > 0) {
            posthoc_pivot_tables <- create_posthoc_pivot_table(
              all_posthoc_data = all_posthoc_data,
              repeat_category_name = repeat_category_name_str,
              posthoc_format = posthoc_format,
              p_adjust_method = p_adjust_method,
              format = format,
              show_header = show_header,
              verbose = verbose
            )

            # Store tables based on format
            if (posthoc_format %in% c("wide", "both") && !is.null(posthoc_pivot_tables$wide)) {
              results_by_group$posthoc_pivot_wide <- posthoc_pivot_tables$wide
            }
            if (posthoc_format %in% c("long", "both") && !is.null(posthoc_pivot_tables$long)) {
              results_by_group$posthoc_pivot_long <- posthoc_pivot_tables$long
            }
          }
        }
      }, error = function(e) {
        warning("Error creating pivot table: ", e$message, "\nColumns in data: ",
                paste(names(combined_data), collapse = ", "))
      })
    }

    # Create combined plot grid
    if (plots && length(all_plots) > 0) {
      results_by_group$all_plots <- all_plots

      tryCatch({
        valid_plots <- all_plots[sapply(all_plots, function(x) {
          !is.null(x) && inherits(x, "ggplot")
        })]

        if (length(valid_plots) > 0) {
          n_plots <- length(valid_plots)
          n_cols <- if (n_plots == 1) 1 else if (n_plots <= 4) 2 else if (n_plots <= 6) 3 else 4

          # Use arrangeGrob to create without displaying
          results_by_group$combined_plot <- do.call(
            gridExtra::arrangeGrob,
            c(valid_plots, ncol = n_cols)
          )
        }
      }, error = function(e) {
        warning("Error creating combined plot: ", e$message)
      })
    }

    # Create post-hoc table if requested (before verbose block so it's available for printing)
    if (posthoc_table && posthoc) {
      # Collect all post-hoc results
      all_posthoc <- data.frame()
      for (stat in all_stats) {
        if (!is.null(stat$posthoc) && nrow(stat$posthoc) > 0) {
          ph_data <- stat$posthoc
          ph_data$Group <- stat$group
          ph_data$Variable <- stat$variable
          all_posthoc <- rbind(all_posthoc, ph_data)
        }
      }

      if (nrow(all_posthoc) > 0) {
        # Add diff column if missing (for nonparametric tests)
        if (!"diff" %in% names(all_posthoc)) {
          all_posthoc$diff <- NA_real_
        }

        # Format the post-hoc table
        posthoc_display <- all_posthoc %>%
          dplyr::mutate(
            Variable = variable,
            Comparison = comparison,
            Difference = ifelse(is.na(diff), "-", sprintf("%.2f", diff)),
            P_adj_fmt = dplyr::case_when(
              is.na(p_adj) ~ "NA",
              p_adj < 0.001 ~ "< .001",
              TRUE ~ sprintf("%.3f", p_adj)
            ),
            Sig = dplyr::case_when(
              is.na(p_adj) ~ "",
              p_adj < 0.001 ~ "***",
              p_adj < 0.01 ~ "**",
              p_adj < 0.05 ~ "*",
              TRUE ~ ""
            )
          ) %>%
          dplyr::select(Group, Variable, Comparison, Difference, `P-adj` = P_adj_fmt, Sig)

        # Identify significant rows
        sig_rows <- which(posthoc_display$Sig != "")

        # Create table based on format
        if (format %in% c("plain", "markdown", "latex", "kable")) {
          results_by_group$posthoc_table <- posthoc_display
        } else {
          # Create gt table
          gt_posthoc <- posthoc_display %>%
            gt::gt() %>%
            gt::tab_header(
              title = "Post-hoc Pairwise Comparisons",
              subtitle = paste0("Method: ", posthoc_method, " | Adjustment: ",
                               if (p_adjust_method == "none") "bonferroni" else p_adjust_method)
            ) %>%
            gt::tab_style(
              style = list(
                gt::cell_text(weight = "bold"),
                gt::cell_borders(sides = "bottom", color = "black", weight = gt::px(2))
              ),
              locations = gt::cells_column_labels()
            ) %>%
            gt::cols_align(align = "center") %>%
            gt::cols_align(align = "left", columns = c("Group", "Variable", "Comparison")) %>%
            gt::tab_options(
              table.border.top.width = gt::px(2),
              table.border.top.color = "black",
              table.border.bottom.width = gt::px(2),
              table.border.bottom.color = "black"
            )

          # Highlight significant rows
          if (length(sig_rows) > 0) {
            gt_posthoc <- gt_posthoc %>%
              gt::tab_style(
                style = gt::cell_text(weight = "bold"),
                locations = gt::cells_body(rows = sig_rows)
              ) %>%
              gt::tab_style(
                style = gt::cell_text(color = "red"),
                locations = gt::cells_body(columns = "P-adj", rows = sig_rows)
              )
          }

          results_by_group$posthoc_table <- gt_posthoc
        }
      }
    }

    # Print clean output
    if (verbose) {
      # Print header
      cat("\n")
      cat(paste(rep("=", 60), collapse = ""), "\n")
      # Use original variable names for display if multiple repeat_category vars
      repeat_display_name <- if (length(repeat_category_orig_vars) > 1) {
        paste(repeat_category_orig_vars, collapse = " x ")
      } else {
        repeat_category_name_str
      }
      cat("  COMPARISON ANALYSIS: ", category_name_str, " by ", repeat_display_name, "\n", sep = "")
      cat(paste(rep("=", 60), collapse = ""), "\n\n")

      # Print sample info
      cat("Sample: N =", total_n, "\n")
      cat("Groups:", paste(categories_to_include, collapse = ", "), "\n")
      cat("Variables:", paste(comparison_categories, collapse = ", "), "\n")
      cat("Test:", if (nonparametric) "Nonparametric (Mann-Whitney/Kruskal-Wallis)" else "Parametric (t-test/ANOVA)", "\n\n")

      # Print statistical results
      cat(paste(rep("-", 70), collapse = ""), "\n")
      cat("  STATISTICAL RESULTS\n")
      cat(paste(rep("-", 70), collapse = ""), "\n\n")

      for (stat in all_stats) {
        sig_marker <- if (!is.na(stat$p_value) && stat$p_value < 0.05) "*" else ""
        p_str <- if (is.na(stat$p_value)) "NA" else if (stat$p_value < 0.001) "< .001" else sprintf("%.3f", stat$p_value)
        ef_str <- if (is.na(stat$effect_size)) "NA" else sprintf("%.3f", stat$effect_size)

        # Format test statistic based on test type
        if (!is.na(stat$test_type) && stat$test_type == "t-test") {
          test_str <- sprintf("t(%.1f) = %.2f", stat$t_df, stat$t_statistic)
        } else if (!is.na(stat$test_type) && stat$test_type == "Mann-Whitney U") {
          test_str <- sprintf("W = %.0f", stat$t_statistic)
        } else if (!is.na(stat$test_type) && stat$test_type == "ANOVA") {
          test_str <- sprintf("F(%d, %d) = %.2f", as.integer(stat$df1), as.integer(stat$df2), stat$f_statistic)
        } else if (!is.na(stat$test_type) && stat$test_type == "Kruskal-Wallis") {
          test_str <- sprintf("H(%d) = %.2f", as.integer(stat$df1), stat$f_statistic)
        } else {
          test_str <- "NA"
        }

        cat(sprintf("  %s: %s\n", stat$group, stat$variable))
        cat(sprintf("    %s, p = %s, %s = %s %s\n",
                    test_str, p_str, stat$effect_type, ef_str, sig_marker))

        # Print significant post-hoc comparisons if available
        if (!is.null(stat$posthoc) && nrow(stat$posthoc) > 0) {
          sig_posthoc <- stat$posthoc[stat$posthoc$p_adj < 0.05, , drop = FALSE]
          if (nrow(sig_posthoc) > 0) {
            # Format each significant comparison
            ph_strs <- sapply(1:nrow(sig_posthoc), function(i) {
              comp <- sig_posthoc$comparison[i]
              p_adj <- sig_posthoc$p_adj[i]
              diff_val <- if ("diff" %in% names(sig_posthoc)) sig_posthoc$diff[i] else NA

              # Determine direction from diff or comparison
              if (!is.na(diff_val) && diff_val != 0) {
                groups <- strsplit(comp, " vs ")[[1]]
                if (length(groups) == 2) {
                  if (diff_val > 0) {
                    comp_str <- paste0(groups[1], " > ", groups[2])
                  } else {
                    comp_str <- paste0(groups[2], " > ", groups[1])
                  }
                } else {
                  comp_str <- comp
                }
              } else {
                comp_str <- comp
              }

              p_str_ph <- if (p_adj < 0.001) "< .001" else sprintf("%.3f", p_adj)
              sprintf("%s (p = %s)", comp_str, p_str_ph)
            })
            cat(sprintf("    Post-hoc: %s\n", paste(ph_strs, collapse = ", ")))
          }
        }
        cat("\n")
      }

      cat("  * p < .05\n")
      cat(paste(rep("=", 70), collapse = ""), "\n\n")

      # Print pivot table if pivot = TRUE
      if (pivot && !is.null(results_by_group$pivot_table)) {
        print(results_by_group$pivot_table)
        cat("\n")

        # Print post-hoc pivot tables
        if (!is.null(results_by_group$posthoc_pivot_wide)) {
          cat("\n")
          print(results_by_group$posthoc_pivot_wide)
        }
        if (!is.null(results_by_group$posthoc_pivot_long)) {
          cat("\n")
          print(results_by_group$posthoc_pivot_long)
        }
      } else {
        # If pivot was requested but failed, show a message
        if (pivot && is.null(results_by_group$pivot_table)) {
          cat("Note: Pivot table creation failed. Showing combined table instead.\n\n")
        }
        # Print combined table (original format)
        if (!is.null(results_by_group$combined_table)) {
          print(results_by_group$combined_table)
          cat("\n")
        }

        # Print post-hoc table if it exists (original format)
        if (!is.null(results_by_group$posthoc_table)) {
          cat("\n")
          print(results_by_group$posthoc_table)
          cat("\n*** p < .001, ** p < .01, * p < .05\n")
        }
      }

      # Display combined plot
      if (!is.null(results_by_group$combined_plot)) {
        gridExtra::grid.arrange(results_by_group$combined_plot)
      }
    }

    # Add metadata
    results_by_group$metadata <- list(
      total_observations = total_n,
      min_threshold = min_threshold,
      category_counts = category_counts,
      included_categories = categories_to_include,
      excluded_categories = excluded_categories,
      variables_analyzed = comparison_categories,
      test_type = if (nonparametric) "nonparametric" else "parametric",
      bayesian_analysis = bayesian,
      equivalence_testing = equivalence,
      equivalence_bounds = if (equivalence) equivalence_bounds else NULL,
      p_adjust_method = p_adjust_method,
      pivot = pivot,
      pivot_by = if (pivot) pivot_by else NULL,
      pivot_stat = if (pivot) pivot_stat else NULL,
      pivot_stars = if (pivot) pivot_stars else NULL,
      pivot_split_level = if (pivot) pivot_split_level else NULL,
      posthoc_format = if (pivot && posthoc_table) posthoc_format else NULL
    )

    # AI interpretation if requested (for stratified analysis)
    if (interpret && !is.null(results_by_group$combined_data)) {
      n_groups <- length(unique(data[[category_name_str]]))
      test_type_str <- if (nonparametric) {
        if (n_groups == 2) "Mann-Whitney U" else "Kruskal-Wallis"
      } else {
        if (n_groups == 2) "t-test" else "ANOVA"
      }

      # Create results object for interpretation
      interp_results <- list(summary_data = results_by_group$combined_data)

      metadata <- list(
        test_type = test_type_str,
        n_groups = n_groups,
        variables = comparison_categories,
        grouping_var = category_name_str,
        groups = unique(data[[category_name_str]]),
        total_n = total_n,
        posthoc_method = posthoc_method,
        stratified_by = repeat_category_name_str,
        strata = categories_to_include
      )

      interpret_with_ai(interp_results, analysis_type = "group_comparison", metadata = metadata, ...)
    }

    return(results_by_group)
  }
}
