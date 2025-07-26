# Saqrmisc Package: Mosaic Analysis Functions
# 
# This file contains functions for categorical variable analysis using mosaic plots.
# Functions: mosaic_analysis

#' @title Perform a comprehensive mosaic plot analysis
#' @description This function conducts a complete mosaic plot analysis for two categorical variables. It includes filtering by minimum count, chi-square testing, calculation of Cramér's V, and generation of a detailed summary table with observed/expected counts and residuals.
#' @param data A data frame containing the variables.
#' @param var1 The unquoted name of the first categorical variable.
#' @param var2 The unquoted name of the second categorical variable.
#' @param min_count The minimum number of observations required for a category to be included in the analysis. Defaults to 10.
#' @param fontsize The font size for the mosaic plot labels. Defaults to 8.
#' @param title The title for the mosaic plot. Defaults to "".
#' @param var1_label The label for the first variable in the plot. Defaults to the variable name.
#' @param var2_label The label for the second variable in the plot. Defaults to the variable name.
#' @param show_percentages Logical. If TRUE, includes percentages in the summary table. Defaults to TRUE.
#' @param percentage_base The base for calculating percentages ("total", "row", or "column"). Defaults to "total".
#' @return A list containing the mosaic plot, consolidated frequency table, standardized residuals, chi-square test results, Cramér's V, a statistical summary, the filtered data, and original vs. filtered sample sizes.
#' @export
mosaic_analysis <- function(data, var1, var2, min_count = 10, 
                            fontsize = 8, title = "", 
                            var1_label = "", var2_label = NULL,
                            show_percentages = TRUE,
                            percentage_base = "total") {  # "total", "row", "column"
  
  # Convert variable names to strings if they're symbols
  var1_name <- deparse(substitute(var1))
  var2_name <- deparse(substitute(var2))
  
  # If var2_label is NULL, use var2_name
  if (is.null(var2_label)) {
    var2_label <- var2_name
  }
  
  # Remove missing values
  clean_data <- data[!is.na(data[[var1_name]]) & !is.na(data[[var2_name]]), ]
  
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
  
  # Perform chi-square test
  chi_test <- chisq.test(filtered_table)
  
  # Calculate effect size (Cramér's V)
  cramers_v <- sqrt(chi_test$statistic / (sum(filtered_table) * 
                                            (min(nrow(filtered_table), ncol(filtered_table)) - 1)))
  
  # Create the mosaic plot using the formula approach
  temp_data <- filtered_data
  formula_str <- paste("~", var1_name, "+", var2_name)
  formula_obj <- as.formula(formula_str)
  
  # Create set_varnames for proper labeling
  set_varnames_vector <- c(var1_label, var2_label)
  names(set_varnames_vector) <- c(var1_name, var2_name)
  
  # Create the mosaic plot
  mosaic_plot <- mosaic(formula_obj, 
                        data = temp_data,
                        main = title,
                        shade = TRUE,
                        legend = TRUE,
                        labeling = labeling_values,
                        labeling_args = list(
                          varnames = c(left = FALSE, top = TRUE),
                          varnames_label = c(left = "", top = ""),
                          rot_labels = c(left = 0, top = 0),
                          offset_labels = c(left = 2.5, top = 0.5),
                          set_varnames = set_varnames_vector,
                          gp_text = gpar(fontsize = fontsize)
                        ))
  
  # Create consolidated table
  create_consolidated_table <- function(observed, expected, show_percentages = TRUE, percentage_base = "total") {
    
    # Convert to data frames
    obs_df <- as.data.frame.matrix(observed)
    exp_df <- as.data.frame.matrix(expected)
    
    # Calculate percentages if requested
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
    consolidated <- tibble()
    
    for (i in 1:nrow(obs_df)) {
      row_name <- rownames(obs_df)[i]
      
      # Observed row
      obs_row <- tibble(
        Variable = row_name,
        Type = "Observed",
        !!!obs_df[i, ],
        Total = sum(obs_df[i, ])
      )
      
      # Expected row
      exp_row <- tibble(
        Variable = "",
        Type = "Expected",
        !!!round(exp_df[i, ], 1),
        Total = round(sum(exp_df[i, ]), 1)
      )
      
      # Percentage row (if requested)
      if (show_percentages) {
        pct_row <- tibble(
          Variable = "",
          Type = paste0("% (", percentage_base, ")"),
          !!!pct_df[i, ],
          Total = if (percentage_base == "row") 100.0 else round(sum(pct_df[i, ]), 1)
        )
        consolidated <- bind_rows(consolidated, obs_row, exp_row, pct_row)
      } else {
        consolidated <- bind_rows(consolidated, obs_row, exp_row)
      }
    }
    
    # Add totals row
    total_obs <- tibble(
      Variable = "Total",
      Type = "Observed",
      !!!colSums(obs_df),
      Total = sum(obs_df)
    )
    
    total_exp <- tibble(
      Variable = "",
      Type = "Expected",
      !!!round(colSums(exp_df), 1),
      Total = round(sum(exp_df), 1)
    )
    
    if (show_percentages) {
      if (percentage_base == "total") {
        total_pct <- tibble(
          Variable = "",
          Type = paste0("% (", percentage_base, ")"),
          !!!round(colSums(observed) / sum(observed) * 100, 1),
          Total = 100.0
        )
      } else if (percentage_base == "column") {
        total_pct <- tibble(
          Variable = "",
          Type = paste0("% (", percentage_base, ")"),
          !!!rep(100.0, ncol(observed)),
          Total = 100.0
        )
      } else {
        total_pct <- tibble(
          Variable = "",
          Type = paste0("% (", percentage_base, ")"),
          !!!round(colSums(pct_df), 1),
          Total = round(sum(pct_df), 1)
        )
      }
      consolidated <- bind_rows(consolidated, total_obs, total_exp, total_pct)
    } else {
      consolidated <- bind_rows(consolidated, total_obs, total_exp)
    }
    
    return(consolidated)
  }
  
  # Create the consolidated table
  consolidated_table <- create_consolidated_table(filtered_table, chi_test$expected, 
                                                  show_percentages, percentage_base)
  
  # Create standardized residuals table
  residuals_df <- as.data.frame.matrix(round(chi_test$stdres, 2))
  residuals_df$Variable <- rownames(residuals_df)
  residuals_df <- residuals_df %>% dplyr::select(Variable, everything())
  
  # Statistical summary
  removed_var1 <- setdiff(levels(factor(clean_data[[var1_name]])), levels(filtered_data[[var1_name]]))
  removed_var2 <- setdiff(levels(factor(clean_data[[var2_name]])), levels(filtered_data[[var2_name]]))
  
  removed_text <- paste0(
    if(length(removed_var1) > 0) paste0(var1_name, ": ", paste(removed_var1, collapse = ", ")) else "",
    if(length(removed_var1) > 0 && length(removed_var2) > 0) "; " else "",
    if(length(removed_var2) > 0) paste0(var2_name, ": ", paste(removed_var2, collapse = ", ")) else ""
  )
  
  if(removed_text == "") removed_text <- "None"
  
  stats_summary <- tibble(
    Statistic = c("Chi-square", "Degrees of freedom", "p-value", "Cramér's V", 
                  "Sample size", "Categories removed"),
    Value = c(
      round(chi_test$statistic, 3),
      chi_test$parameter,
      ifelse(chi_test$p.value < 0.001, "< 0.001", round(chi_test$p.value, 3)),
      round(cramers_v, 3),
      sum(filtered_table),
      removed_text
    )
  )
  
  # Print results
  cat("\n=== MOSAIC ANALYSIS RESULTS ===\n")
  cat("Variables:", var1_name, "×", var2_name, "\n")
  cat("Minimum count threshold:", min_count, "\n")
  if (show_percentages) {
    cat("Percentages based on:", percentage_base, "\n")
  }
  cat("\n")
  
  # Print consolidated table
  cat("CONSOLIDATED FREQUENCY TABLE\n")
  cat("===========================\n")
  print(consolidated_table, n = Inf)
  cat("\n")
  
  # Print standardized residuals
  cat("STANDARDIZED RESIDUALS\n")
  cat("=====================\n")
  print(residuals_df, n = Inf)
  cat("\n")
  
  # Print statistical summary
  cat("STATISTICAL SUMMARY\n")
  cat("==================\n")
  print(stats_summary, n = Inf)
  cat("\n")
  
  # Return results as a list
  results <- list(
    plot = mosaic_plot,
    consolidated_table = consolidated_table,
    residuals = residuals_df,
    chi_test = chi_test,
    cramers_v = cramers_v,
    stats_summary = stats_summary,
    filtered_data = filtered_data,
    original_n = nrow(clean_data),
    filtered_n = nrow(filtered_data)
  )
  
  return(invisible(results))
}

# Usage examples:
# Basic usage:
# results <- mosaic_analysis(WithMeans, overarching_discipline, gender, 
#                           min_count = 10, var1_label = "", var2_label = "gender")

# With row percentages:
# results <- mosaic_analysis(WithMeans, overarching_discipline, gender, 
#                           min_count = 10, var1_label = "", var2_label = "gender",
#                           show_percentages = TRUE, percentage_base = "row")

# With column percentages:
# results <- mosaic_analysis(WithMeans, overarching_discipline, gender, 
#                           min_count = 10, var1_label = "", var2_label = "gender",
#                           show_percentages = TRUE, percentage_base = "column")

# Without percentages:
# results <- mosaic_analysis(WithMeans, overarching_discipline, gender, 
#                           min_count = 10, var1_label = "", var2_label = "gender",
#                           show_percentages = FALSE)