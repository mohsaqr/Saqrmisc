# Saqrmisc Package: Comparison Analysis Functions
# 
# This file contains functions for statistical comparison analysis and visualization.
# Functions: generate_comparison_plots

#' @title Generate ggbetweenstats comparison plots and summary statistics
#'
#' @description
#' This function creates a series of `ggbetweenstats` plots to compare a
#' primary `category` across any set of comparison variables defined by `Vars`.
#' When a `repeat_category` is provided, the function runs separate analyses
#' for each unique level, automatically excluding categories that represent
#' less than 5% of the total data to avoid analyzing many small categories.
#'
#' @param data A data frame containing the variables.
#' @param category The unquoted name of the column to be used as the
#'   x-axis variable (e.g., `gender`). This should be a categorical variable.
#' @param Vars A character vector of variable names to compare against the category.
#'   These should be column names in the data frame (e.g., c("csu", "ta", "iv", "se", "sr")).
#' @param repeat_category The unquoted name of an optional column. If provided,
#'   separate analyses will be performed for each unique level of this column.
#'   Categories with less than 5% of the total data will be automatically excluded.
#'   (e.g., `source`). This should be a categorical variable. Defaults to `NULL`.
#' @param plots Logical. If TRUE, generates and displays plots. Defaults to TRUE.
#' @param table Logical. If TRUE, generates and displays a summary table. Defaults to TRUE.
#' @param min_threshold Numeric. Minimum percentage threshold (as decimal) for including
#'   repeat_category groups. Defaults to 0.05 (5%). Only applies when repeat_category is specified.
#' @param min_subcategory Numeric. Minimum number of observations required for each
#'   subcategory within repeat_category groups. Defaults to 5. Categories with fewer
#'   observations will be excluded from analysis within each repeat_category group.
#'
#' @return A list containing:
#'   - If no repeat_category: plots, grid_plot, summary_table, summary_data
#'   - If repeat_category: named list with results for each repeat category level (above threshold)
#'
#' @examples
#' # Create some dummy data for demonstration
#' Tokens <- data.frame(
#'   gender = sample(c("Male", "Female"), 100, replace = TRUE),
#'   csu = rnorm(100, mean = c(5, 7)[factor(sample(c("Male", "Female"), 100, replace = TRUE))]),
#'   ta = rnorm(100, mean = c(10, 12)[factor(sample(c("Male", "Female"), 100, replace = TRUE))]),
#'   iv = rnorm(100, mean = c(3, 4)[factor(sample(c("Male", "Female"), 100, replace = TRUE))]),
#'   se = rnorm(100, mean = c(8, 9)[factor(sample(c("Male", "Female"), 100, replace = TRUE))]),
#'   sr = rnorm(100, mean = c(6, 6.5)[factor(sample(c("Male", "Female"), 100, replace = TRUE))]),
#'   source = sample(c("A", "B", "C", "D", "E"), 100, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.08, 0.02))
#' )
#'
#' # Example 1: Generate both plots and table (no repeat category)
#' results <- generate_comparison_plots(
#'   data = Tokens,
#'   category = gender,
#'   Vars = c("csu", "ta", "iv", "se", "sr"),
#'   plots = TRUE,
#'   table = TRUE
#' )
#'
#' # Example 2: Generate with repeat category (separate analysis per source)
#' # Categories with < 5% of data will be automatically excluded
#' results_by_source <- generate_comparison_plots(
#'   data = Tokens,
#'   category = gender,
#'   Vars = c("csu", "ta", "iv"),
#'   repeat_category = source,
#'   plots = TRUE,
#'   table = TRUE
#' )
#'
#' # Example 3: Different variables for psychological measures
#' psych_data <- data.frame(
#'   group = sample(c("Control", "Treatment"), 100, replace = TRUE),
#'   anxiety = rnorm(100, mean = c(3, 2)[factor(sample(c("Control", "Treatment"), 100, replace = TRUE))]),
#'   depression = rnorm(100, mean = c(2, 1.5)[factor(sample(c("Control", "Treatment"), 100, replace = TRUE))]),
#'   stress = rnorm(100, mean = c(4, 3)[factor(sample(c("Control", "Treatment"), 100, replace = TRUE))])
#' )
#'
#' results_psych <- generate_comparison_plots(
#'   data = psych_data,
#'   category = group,
#'   Vars = c("anxiety", "depression", "stress"),
#'   plots = TRUE,
#'   table = TRUE
#' )

generate_comparison_plots <- function(data, category, Vars, repeat_category = NULL, plots = TRUE, table = TRUE, min_threshold = 0.05, min_subcategory = 5) {
  # Input validation
  if (missing(Vars) || is.null(Vars) || length(Vars) == 0) {
    stop("Vars parameter is required and must be a non-empty character vector of variable names")
  }
  
  if (!is.character(Vars)) {
    stop("Vars must be a character vector of variable names")
  }
  
  # Check if all variables in Vars exist in the data
  missing_vars <- setdiff(Vars, names(data))
  if (length(missing_vars) > 0) {
    stop("The following variables in Vars are not found in the data: ", paste(missing_vars, collapse = ", "))
  }
  
  # Capture the unquoted category argument name as a string using deparse(substitute())
  category_name_str <- deparse(substitute(category))
  # Convert the string name to a symbol for use with !!
  category_sym <- rlang::sym(category_name_str)
  
  # Check if category exists in the data
  if (!category_name_str %in% names(data)) {
    stop("Category variable '", category_name_str, "' not found in the data")
  }
  
  # Capture the unquoted repeat_category argument name as a string, if provided
  repeat_category_name_str <- if (!is.null(substitute(repeat_category))) {
    deparse(substitute(repeat_category))
  } else {
    NULL
  }
  
  # Check if repeat_category exists in the data (if provided)
  if (!is.null(repeat_category_name_str) && !repeat_category_name_str %in% names(data)) {
    stop("Repeat category variable '", repeat_category_name_str, "' not found in the data")
  }
  
  # Use the provided Vars instead of fixed comparison categories
  comparison_categories <- Vars
  
  # Helper function to run analysis for a single dataset
  run_single_analysis <- function(data_subset, group_label = NULL) {
    # Initialize result list
    result <- list()
    
    # Filter subcategories within this group if min_subcategory is specified
    if (!is.null(repeat_category_name_str) && min_subcategory > 0) {
      # Check subcategory counts within this group
      subcategory_counts <- data_subset %>%
        count(.data[[category_name_str]]) %>%
        filter(n >= min_subcategory)
      
      # Report filtering for this group
      original_subcats <- unique(data_subset[[category_name_str]])
      kept_subcats <- subcategory_counts[[category_name_str]]
      excluded_subcats <- setdiff(original_subcats, kept_subcats)
      
      if (length(excluded_subcats) > 0) {
        cat("  Excluded subcategories (< ", min_subcategory, " obs): ",
            paste(excluded_subcats, collapse = ", "), "\n")
      }
      
      if (length(kept_subcats) > 0) {
        cat("  Included subcategories: ", paste(kept_subcats, collapse = ", "), "\n")
      }
      
      # Filter the data to keep only subcategories with sufficient observations
      data_subset <- data_subset %>%
        filter(.data[[category_name_str]] %in% kept_subcats)
      
      # Check if we have enough data left for analysis
      if (nrow(data_subset) < 2 || length(unique(data_subset[[category_name_str]])) < 2) {
        cat("  Warning: Insufficient data remaining after filtering subcategories\n")
        return(list(
          plots = NULL,
          grid_plot = NULL,
          summary_table = NULL,
          summary_data = NULL,
          filtered_out = TRUE,
          reason = "Insufficient subcategory data"
        ))
      }
    }
    
    # Initialize lists for plots and summary data
    all_plots <- list()
    summary_data <- data.frame()
    
    # Process each comparison category
    for (comp_cat_name in comparison_categories) {
      plot_title <- if (is.null(group_label)) {
        paste0("Comparison: ", category_name_str, " vs ", comp_cat_name)
      } else {
        paste0("Comparison: ", category_name_str, " vs ", comp_cat_name, " (", group_label, ")")
      }
      
      # Convert comp_cat_name to a symbol using rlang::sym()
      comp_cat_sym <- rlang::sym(comp_cat_name)
      
      if (plots) {
        # Generate the ggbetweenstats plot with error handling
        tryCatch({
          # Check if we have enough data for the plot
          if (nrow(data_subset) < 2) {
            warning("Insufficient data for plot: ", plot_title)
            next
          }
          
          # Check if we have valid categories
          if (length(unique(data_subset[[category_name_str]])) < 2) {
            warning("Need at least 2 categories for comparison in: ", plot_title)
            next
          }
          
          # Check for missing values in key variables
          if (all(is.na(data_subset[[comp_cat_name]]))) {
            warning("All values are NA for variable: ", comp_cat_name)
            next
          }
          
          p <- ggstatsplot::ggbetweenstats(
            data = data_subset, bf.message = FALSE,
            x = !!category_sym, xlab = "",
            y = !!comp_cat_sym, conf.level = NULL,
            title = ""
          ) + ggplot2::scale_color_manual(values = c("#EF476F", "#26547C"))
          
          plot_name <- paste0(category_name_str, "_vs_", comp_cat_name)
          all_plots[[plot_name]] <- p
        }, error = function(e) {
          warning("Error creating plot for ", plot_title, ": ", e$message)
        })
      }
      
      if (table) {
        # Calculate summary statistics with error handling
        tryCatch({
          # Check if we have valid data
          if (nrow(data_subset) < 2 || all(is.na(data_subset[[comp_cat_name]]))) {
            warning("Insufficient valid data for statistics: ", comp_cat_name)
            next
          }
          
          summary_stats <- data_subset %>%
            group_by(!!category_sym) %>%
            summarise(
              mean = mean(!!comp_cat_sym, na.rm = TRUE),
              sd = sd(!!comp_cat_sym, na.rm = TRUE),
              n = n(),
              .groups = 'drop'
            ) %>%
            mutate(
              variable = comp_cat_name,
              category = category_name_str
            )
          
          # Perform statistical test
          if (length(unique(data_subset[[category_name_str]])) == 2 &&
              nrow(data_subset) > 2) {
            tryCatch({
              test_result <- t.test(data_subset[[comp_cat_name]] ~ data_subset[[category_name_str]])
              efsz <- effectsize::effectsize(test_result)$d
              p_value <- test_result$p.value
            }, error = function(e) {
              warning("Error in t-test for ", comp_cat_name, ": ", e$message)
              efsz <- NA
              p_value <- NA
            })
          } else if (length(unique(data_subset[[category_name_str]])) > 2 &&
                     nrow(data_subset) > 2) {
            # For more than 2 groups, use ANOVA
            tryCatch({
              test_result <- aov(data_subset[[comp_cat_name]] ~ data_subset[[category_name_str]])
              efsz <- effectsize::effectsize(test_result)$Eta2_partial
              p_value <- summary(test_result)[[1]][["Pr(>F)"]][1]
            }, error = function(e) {
              warning("Error in ANOVA for ", comp_cat_name, ": ", e$message)
              efsz <- NA
              p_value <- NA
            })
          } else {
            efsz <- NA
            p_value <- NA
          }
          summary_stats$efsz <- efsz
          summary_stats$p_value <- p_value
          summary_data <- rbind(summary_data, summary_stats)
        }, error = function(e) {
          warning("Error calculating statistics for ", comp_cat_name, ": ", e$message)
        })
      }
    }
    
    # Add plots to result if requested
    if (plots && length(all_plots) > 0) {
      result$plots <- all_plots
      
      # Create grid arrangement - dynamic based on number of variables
      n_plots <- length(all_plots)
      if (n_plots == 1) {
        n_cols <- 1
      } else if (n_plots <= 2) {
        n_cols <- 2
      } else if (n_plots <= 4) {
        n_cols <- 2
      } else if (n_plots <= 6) {
        n_cols <- 3
      } else {
        n_cols <- 4  # For more than 6 plots, use 4 columns
      }
      
      # Create grid plot with error handling
      tryCatch({
        # Ensure we have valid plots before arranging
        valid_plots <- all_plots[sapply(all_plots, function(x) !is.null(x) && inherits(x, "ggplot"))]
        
        if (length(valid_plots) > 0) {
          if(is.null(group_label)) {
            result$grid_plot <- do.call(gridExtra::grid.arrange, c(valid_plots, ncol = n_cols, top = group_label))
          } else {
            result$grid_plot <- do.call(gridExtra::grid.arrange, c(valid_plots, ncol = n_cols))
          }
        } else {
          warning("No valid plots to arrange in grid.")
          result$grid_plot <- NULL
        }
      }, error = function(e) {
        warning("Error creating grid plot: ", e$message)
        result$grid_plot <- NULL
        # Store individual plots even if grid fails
        result$plots <- all_plots
      })
    }
    
    # Add table to result if requested
    if (table && nrow(summary_data) > 0) {
      result$summary_data <- summary_data
      
      # Create formatted table with error handling
      tryCatch({
        # Create a properly formatted table with exact decimal control
        formatted_table <- summary_data %>%
          select(variable, !!category_sym, mean, sd, n, p_value, ES = efsz) %>%
          group_by(variable) %>%
          mutate(
            # Format numbers to exactly 2 decimal places as strings
            mean_formatted = sprintf("%.2f", mean),
            sd_formatted = sprintf("%.2f", sd),
            ES_formatted = sprintf("%.2f", ES),
            # Identify highest mean within each variable group
            is_highest = mean == max(mean, na.rm = TRUE),
            # Format p-value (same for all rows within a variable group)
            p_formatted = case_when(
              is.na(p_value) ~ "NA",
              p_value < 0.001 ~ "< 0.001",
              TRUE ~ sprintf("%.3f", p_value)
            )
          ) %>%
          ungroup() %>%
          select(Variable = variable, Category = !!category_sym, Mean = mean_formatted,
                 SD = sd_formatted, N = n, ES = ES_formatted, `P-value` = p_formatted, is_highest)
        
        table_title <- if (is.null(group_label)) {
          paste0("Summary Statistics: ", category_name_str, " Comparisons")
        } else {
          paste0("Summary Statistics: ", category_name_str, " Comparisons (", group_label, ")")
        }
        
        # Create gt table
        result$summary_table <- formatted_table %>%
          select(-is_highest) %>%
          gt() %>%
          tab_header(title = table_title) %>%
          tab_style(
            style = cell_text(weight = "bold"),
            locations = cells_column_labels()
          ) %>%
          tab_style(
            style = cell_text(weight = "bold"),
            locations = cells_body(columns = Variable)
          ) %>%
          # Highlight highest means within each variable group
          tab_style(
            style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = c(Category, Mean, SD, N, ES),
              rows = which(formatted_table$is_highest)
            )
          ) %>%
          # Color p-values red if significant
          tab_style(
            style = cell_text(color = "red"),
            locations = cells_body(
              columns = `P-value`,
              rows = which(formatted_table$`P-value` == "< 0.001" |
                             (suppressWarnings(as.numeric(formatted_table$`P-value`)) < 0.05 &
                                !is.na(suppressWarnings(as.numeric(formatted_table$`P-value`)))))
            )
          ) %>%
          cols_align(align = "center") %>%
          tab_options(
            table.font.size = px(12),
            heading.title.font.size = px(14),
            column_labels.font.weight = "bold"
          )
        
        # Print the table
        cat("\n")
        print(result$summary_table)
        cat("\n")
      }, error = function(e) {
        warning("Error creating formatted table: ", e$message)
        # Fallback to simple table
        result$summary_table <- summary_data
        print(summary_data)
      })
    }
    
    return(result)
  }
  
  # Main logic: check if repeat_category is provided
  if (is.null(repeat_category_name_str)) {
    # Case 1: No repeat_category specified - run single analysis
    return(run_single_analysis(data))
  } else {
    # Case 2: repeat_category is specified - run separate analyses with filtering
    
    # Calculate frequency and percentage for each repeat_category value
    total_n <- nrow(data)
    category_counts <- data %>%
      count(.data[[repeat_category_name_str]]) %>%
      mutate(
        percentage = n / total_n,
        above_threshold = percentage >= min_threshold
      ) %>%
      arrange(desc(percentage))
    
    # Report filtering results
    cat("=== Category Filtering Results ===\n")
    cat("Total observations:", total_n, "\n")
    cat("Minimum threshold:", paste0(min_threshold * 100, "%"), "\n\n")
    
    # Show all categories with their counts and percentages
    cat("All categories:\n")
    for (i in 1:nrow(category_counts)) {
      status <- if (category_counts$above_threshold[i]) "INCLUDED" else "EXCLUDED"
      cat(sprintf("  %s: %d obs (%.1f%%) - %s\n",
                  category_counts[[repeat_category_name_str]][i],
                  category_counts$n[i],
                  category_counts$percentage[i] * 100,
                  status))
    }
    
    # Get categories above threshold
    categories_to_include <- category_counts %>%
      filter(above_threshold) %>%
      pull(.data[[repeat_category_name_str]])
    
    excluded_categories <- category_counts %>%
      filter(!above_threshold) %>%
      pull(.data[[repeat_category_name_str]])
    
    if (length(excluded_categories) > 0) {
      cat("\nExcluded categories (below", paste0(min_threshold * 100, "%"), "threshold):",
          paste(excluded_categories, collapse = ", "), "\n")
    }
    
    cat("\nIncluded categories:", paste(categories_to_include, collapse = ", "), "\n")
    cat("Proceeding with", length(categories_to_include), "categories...\n\n")
    
    # Check if we have any categories to analyze
    if (length(categories_to_include) == 0) {
      cat("Warning: No categories meet the minimum threshold criteria.\n")
      cat("Consider lowering the min_threshold parameter or check your data.\n")
      return(list())
    }
    
    # Run analyses only for categories above threshold
    results_by_group <- list()
    successful_analyses <- 0
    
    for (repeat_val in categories_to_include) {
      cat("=== Analysis for", repeat_category_name_str, ":", repeat_val, "===\n")
      
      filtered_data <- data %>%
        dplyr::filter(.data[[repeat_category_name_str]] == repeat_val)
      
      cat("  Total observations in this group:", nrow(filtered_data), "\n")
      
      group_label <- repeat_val
      analysis_result <- run_single_analysis(filtered_data, group_label)
      
      # Check if analysis was successful
      if (!is.null(analysis_result$filtered_out) && analysis_result$filtered_out) {
        cat("  Analysis skipped for this group\n")
      } else {
        successful_analyses <- successful_analyses + 1
      }
      
      results_by_group[[as.character(repeat_val)]] <- analysis_result
      cat("\n")
    }
    
    cat("Successfully completed", successful_analyses, "out of", length(categories_to_include), "analyses\n\n")
    
    # Add metadata about filtering to results
    results_by_group$filtering_info <- list(
      total_observations = total_n,
      min_threshold = min_threshold,
      category_counts = category_counts,
      included_categories = categories_to_include,
      excluded_categories = excluded_categories,
      variables_analyzed = comparison_categories
    )
    
    return(results_by_group)
  }
}
