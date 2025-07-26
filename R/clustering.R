# ===================================================================
#  Full MoEClust Comprehensive Analysis Script
# ===================================================================
#
# AUTHOR: Your AI Assistant
# DATE: July 26, 2025
#
# DESCRIPTION:
# This script provides a robust and automated framework for performing
# model-based clustering using the MoEClust package. It is designed to
# find the best clustering solution by iterating through all available
# models, handling potential errors, and generating detailed outputs
# for both original and scaled data to facilitate interpretation.
#
# SECTIONS:
#   A. Main Analysis Function (`run_full_moe_analysis`)
#   B. Helper Function to View Results (`view_results`)
#   C. Complete Workflow Example with Sample Data
#
# ===================================================================


# Saqrmisc Package: Clustering Analysis Functions
# 
# This file contains functions for model-based clustering analysis using MoEClust.
# Functions: run_full_moe_analysis, view_results


# ===================================================================
#  SECTION A: MAIN ANALYSIS FUNCTION
# ===================================================================

#' @title Run a Full, Multi-Model MoEClust Analysis
#' @author Your AI Assistant
#' @date July 26, 2025
#'
#' @description
#' This is the core function of the script. It systematically performs a
#' comprehensive clustering analysis by iterating through all 14 possible covariance
#' models provided by the `MoEClust` package for a given number of clusters (G).
#' It incorporates robust error handling to prevent the entire process from
#' stopping if a single model fails to converge.
#'
#' For each successfully fitted model, the function generates a rich set of outputs,
#' critically providing results based on both the **original data scale** (for
#' interpretability) and the **scaled data** (on which the clustering was performed).
#'
#' @param input_data A data frame or tibble containing the dataset. It should
#'   include at least the variables specified in `cluster_vars`.
#' @param cluster_vars A character vector of column names from `input_data` to be
#'   used for the clustering analysis.
#' @param n_clusters An integer specifying the number of clusters (G) to fit.
#' @param scaling_method A character string specifying the preprocessing method
#'   to be applied to the data before clustering. Defaults to "standardize".
#'   Possible options are:
#'   \itemize{
#'     \item{"`standardize`"}: Z-score transformation (mean 0, sd 1). Recommended for most cases.
#'     \item{"`center`"}: Subtracts the mean from each variable (mean 0).
#'     \item{"`minmax`"}: Scales the data to a range of [0, 1].
#'     \item{"`none`"}: No scaling is applied. Use with caution.
#'   }
#'
#' @return A deeply nested list containing the results for every successfully
#'   fitted model. The structure for each model (e.g., `results$VEV`) is:
#'   \itemize{
#'     \item{`model`}: The raw `MoEClust` model object, containing BIC, log-likelihood, etc.
#'     \item{`outputs_original`}: A list of results based on the original data scale.
#'       \itemize{
#'         \item{`means_table`}: A tibble of cluster means for each variable in their original units.
#'         \item{`profile_plot`}: A ggplot object showing the cluster profiles on the original scale.
#'       }
#'     \item{`outputs_scaled`}: A list of results based on the scaled data.
#'       \itemize{
#'         \item{`means_table`}: A tibble of cluster means for the scaled variables.
#'         \item{`profile_plot`}: A ggplot object showing the cluster profiles on the scaled data.
#'       }
#'     \item{`size_table`}: A `janitor::tabyl` object showing the number and percentage of observations in each cluster.
#'   }

run_full_moe_analysis <- function(input_data, cluster_vars, n_clusters, scaling_method = "standardize") {
  
  # --- Step 1.1: Data Preparation ---
  # Select only the columns specified by the user for clustering.
  original_data_subset <- dplyr::select(input_data, all_of(cluster_vars))
  
  # --- Step 1.2: Data Scaling ---
  # Define a helper function for Min-Max scaling.
  min_max_scale <- function(x) { (x - min(x)) / (max(x) - min(x)) }
  
  # Apply the chosen scaling method using a switch statement for clarity.
  scaled_data <- switch(scaling_method,
                        standardize = as.data.frame(scale(original_data_subset)),
                        center      = as.data.frame(scale(original_data_subset, scale = FALSE)),
                        minmax      = as.data.frame(lapply(original_data_subset, min_max_scale)),
                        original_data_subset # If "none", return the data as is.
  )
  
  # --- Step 2: Iterate Through All 14 MoEClust Models ---
  # Retrieve all possible model names from the mclust package options.
  all_model_names <- mclust.options("emModelNames")
  all_results <- list() # Initialize an empty list to store results.
  
  message(paste("--- Running Analysis for", n_clusters, "Clusters ---"))
  
  for (model_name in all_model_names) {
    message(paste("Fitting model:", model_name))
    
    # Use a tryCatch block for robust error handling. If MoE_clust fails
    # for a given model (e.g., due to a singular covariance matrix), it will
    # print an error message and continue to the next model instead of crashing.
    moe_model <- tryCatch({
      MoE_clust(scaled_data, G = n_clusters, modelNames = model_name, algo = "EM")
    }, error = function(e) {
      message(paste("  -> Failed to fit model", model_name, ". Skipping."))
      return(NULL) # Return NULL on failure.
    })
    
    # Proceed only if the model was fitted successfully (is not NULL).
    if (!is.null(moe_model)) {
      # --- Step 3: Generate Dual-Scale Outputs ---
      # Extract the final cluster assignments for each observation.
      cluster_id <- moe_model$classification
      
      # A) Generate outputs based on the ORIGINAL data scale for interpretability.
      plot_data_orig <- bind_cols(original_data_subset, cluster = factor(cluster_id)) %>% relocate(cluster)
      means_table_orig <- plot_data_orig %>% group_by(cluster) %>% summarise(across(where(is.numeric), mean))
      profile_plot_orig <- prcr::plot_profiles(plot_data_orig) + labs(subtitle = "Original Data Scale")
      
      # B) Generate outputs based on the SCALED data on which clustering was performed.
      plot_data_scaled <- bind_cols(scaled_data, cluster = factor(cluster_id)) %>% relocate(cluster)
      means_table_scaled <- plot_data_scaled %>% group_by(cluster) %>% summarise(across(where(is.numeric), mean))
      profile_plot_scaled <- prcr::plot_profiles(plot_data_scaled) + labs(subtitle = paste0("Scaled Data (", scaling_method, ")"))
      
      # C) Generate common output (cluster sizes are scale-independent).
      size_table <- janitor::tabyl(plot_data_orig, cluster)
      
      # Store all generated objects in a deeply nested list, named by the model type.
      all_results[[model_name]] <- list(
        model = moe_model,
        outputs_original = list(means_table = means_table_orig, profile_plot = profile_plot_orig),
        outputs_scaled = list(means_table = means_table_scaled, profile_plot = profile_plot_scaled),
        size_table = size_table
      )
    }
  }
  
  message(paste("--- Finished. Successfully fitted", length(all_results), "of 14 models. ---"))
  return(all_results)
}


# ===================================================================
#  SECTION B: HELPER FUNCTION TO VIEW RESULTS
# ===================================================================

#' @title View Plots or Tables from Full Analysis Results
#' @author Your AI Assistant
#' @date July 26, 2025
#'
#' @description
#' A convenience function to easily print plots or tables from the large list
#' object generated by `run_full_moe_analysis`. It iterates through all
#' successfully fitted models in the results list and prints the desired output,
#' saving the user from writing complex loops for inspection.
#'
#' @param results A list object generated by `run_full_moe_analysis`.
#' @param what A character string specifying what to view.
#'   Options: "`plots`" or "`tables`".
#' @param scale A character string specifying which data scale to view.
#'   Options: "`original`" or "`scaled`".
#'
#' @return This function does not return a value. It is used for its side
#'   effect of printing output to the console.

view_results <- function(results, what = "plots", scale = "original") {
  
  # Determine which sub-list to access ('outputs_original' or 'outputs_scaled').
  output_type <- if (scale == "original") "outputs_original" else "outputs_scaled"
  
  # Check if any models were successfully fitted before proceeding.
  if (length(results) == 0) {
    message("The results list is empty. No models were successfully fitted.")
    return(invisible(NULL))
  }
  
  # Loop through each successfully fitted model in the results list.
  for (model_name in names(results)) {
    model_result <- results[[model_name]]
    
    if (what == "plots") {
      cat(paste("\n--- Profile Plot for Model:", model_name, "--- (", scale, " scale)\n"))
      print(model_result[[output_type]]$profile_plot)
    } else if (what == "tables") {
      cat(paste("\n--- Mean Table for Model:", model_name, "--- (", scale, " scale)\n"))
      print(as.data.frame(model_result[[output_type]]$means_table))
      cat("\n--- Size Table ---\n")
      print(model_result$size_table)
    }
  }
}



