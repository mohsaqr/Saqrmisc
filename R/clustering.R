# Saqrmisc Package: Clustering Analysis Functions
#
# This file contains functions for model-based clustering analysis using MoEClust.
# Main function: clustering()
# Utility functions: plot(), summary(), get_cluster_assignments(), assess_cluster_stability(),
#                   model_comparison_table(), generate_cluster_report()

#' @importFrom dplyr select group_by summarise across mutate arrange bind_cols relocate filter all_of
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_tile geom_col geom_text labs theme_minimal
#'   theme element_text scale_fill_gradient scale_fill_gradient2 scale_fill_viridis_d scale_color_viridis_d
#' @importFrom tidyr pivot_longer
#' @importFrom stats complete.cases var
#' @importFrom utils packageVersion
NULL

# =============================================================================
# CONSTANTS
# =============================================================================

#' Available MoEClust model names
#' @noRd
MOECLUST_MODELS <- c("EII", "VII", "EEI", "VEI", "EVI", "VVI",
                     "EEE", "EVE", "VEE", "VVE", "EEV", "VEV", "EVV", "VVV")

#' Valid scaling methods
#' @noRd
VALID_SCALING_METHODS <- c("standardize", "center", "minmax", "none")

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Safe min-max scaling that handles constant variables
#' @param x Numeric vector
#' @return Scaled numeric vector
#' @noRd
safe_minmax_scale <- function(x) {
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (rng == 0) {
    return(rep(0.5, length(x)))  # Return midpoint for constant variables
  }
  (x - min(x, na.rm = TRUE)) / rng
}

#' Apply scaling to data
#' @param data Data frame to scale
#' @param method Scaling method
#' @return Scaled data frame
#' @noRd
apply_scaling <- function(data, method) {
  switch(method,
         standardize = as.data.frame(scale(data, center = TRUE, scale = TRUE)),
         center = as.data.frame(scale(data, center = TRUE, scale = FALSE)),  # FIXED: center only
         minmax = as.data.frame(lapply(data, safe_minmax_scale)),
         none = data,
         stop("Invalid scaling method")
  )
}

#' Create heatmap for cluster means
#'
#' @param model_data Model results containing means data
#' @param cluster_vars Variable names for clustering
#' @param scale_type "original" or "scaled"
#' @param model_name Name of the model for title
#' @return ggplot heatmap object
#' @noRd
create_cluster_heatmap <- function(model_data, cluster_vars, scale_type, model_name) {
  # Get the appropriate means data
  if (scale_type == "original") {
    means_data <- model_data$original_scale$means
  } else {
    means_data <- model_data$scaled_data$means
  }

  # Prepare data for heatmap
  heatmap_data <- means_data %>%
    tidyr::pivot_longer(cols = dplyr::all_of(cluster_vars),
                        names_to = "variable", values_to = "value") %>%
    dplyr::mutate(
      cluster = factor(cluster),
      variable = gsub("_", " ", variable)
    )

  # Calculate data range
  data_min <- min(heatmap_data$value, na.rm = TRUE)
  data_max <- max(heatmap_data$value, na.rm = TRUE)

  # Create base plot
  p <- ggplot2::ggplot(heatmap_data,
                       ggplot2::aes(x = variable, y = cluster, fill = value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = round(value, 2)),
                       color = "grey90", size = 3.5, fontface = "bold")

  # Apply correct color scale based on data range
  if (data_min >= 0) {
    p <- p + ggplot2::scale_fill_gradient(
      low = "white", high = "darkblue",
      limits = c(0, data_max), name = "Mean\nValue"
    )
  } else if (data_max <= 0) {
    p <- p + ggplot2::scale_fill_gradient(
      low = "darkred", high = "white",
      limits = c(data_min, 0), name = "Mean\nValue"
    )
  } else {
    max_abs <- max(abs(data_min), abs(data_max))
    p <- p + ggplot2::scale_fill_gradient2(
      low = "darkred", mid = "white", high = "darkblue",
      midpoint = 0, limits = c(-max_abs, max_abs), name = "Mean\nValue"
    )
  }

  # Add theme and labels
  p + ggplot2::labs(
    title = paste("Cluster Means Heatmap -", model_name, "(", scale_type, "scale)"),
    x = "Variables", y = "Cluster"
  ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 10),
      plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
      panel.grid = ggplot2::element_blank()
    )
}

#' Create bar chart for cluster means
#'
#' @param model_data Model results
#' @param cluster_vars Variable names
#' @param scale_type "original" or "scaled"
#' @param model_name Model name for title
#' @return ggplot bar chart object
#' @noRd
create_cluster_barchart <- function(model_data, cluster_vars, scale_type, model_name) {
  # Get the appropriate means data
  if (scale_type == "original") {
    means_data <- model_data$original_scale$means
  } else {
    means_data <- model_data$scaled_data$means
  }

  # Prepare data for bar chart
  bar_data <- means_data %>%
    tidyr::pivot_longer(cols = -cluster, names_to = "variable", values_to = "value") %>%
    dplyr::mutate(
      cluster = factor(cluster),
      variable = gsub("_", " ", variable)
    )

  # Create bar chart
  ggplot2::ggplot(bar_data,
                  ggplot2::aes(fill = variable, y = value, x = cluster)) +
    ggplot2::geom_col(position = "dodge", alpha = 0.8) +
    ggplot2::scale_fill_brewer("", type = "qual", palette = 8) +
    ggplot2::labs(
      title = paste("Cluster Means Bar Chart -", model_name, "(", scale_type, "scale)"),
      x = "Cluster", y = "Mean Value"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(size = 10),
      plot.title = ggplot2::element_text(size = 12, hjust = 0.5)
    )
}

#' Create cluster size bar chart
#'
#' @param model_data Model results
#' @param model_name Model name for title
#' @param colors Optional custom colors
#' @return ggplot object
#' @noRd
create_cluster_sizes_chart <- function(model_data, model_name, colors = NULL) {
  cluster_sizes <- model_data$original_scale$sizes

  p <- ggplot2::ggplot(cluster_sizes,
                       ggplot2::aes(x = factor(cluster), y = size, fill = factor(cluster))) +
    ggplot2::geom_col(alpha = 0.8, color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(size, "\n(", percentage, "%)")),
                       vjust = 0.5, color = "black", size = 3.5, fontface = "bold") +
    ggplot2::labs(title = paste("Cluster Sizes -", model_name),
                  x = "Cluster", y = "Number of Members") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
      axis.text.x = ggplot2::element_text(size = 10),
      axis.text.y = ggplot2::element_text(size = 10)
    )

  if (!is.null(colors)) {
    p <- p + ggplot2::scale_fill_manual(values = colors)
  } else {
    p <- p + ggplot2::scale_fill_viridis_d(name = "Cluster")
  }

  p
}

# =============================================================================
# MAIN ANALYSIS FUNCTION
# =============================================================================

#' Model-Based Clustering Analysis
#'
#' @description
#' Performs comprehensive model-based clustering analysis using the MoEClust package.
#' Systematically tests multiple covariance models and provides results on both
#' original and scaled data scales for interpretation.
#'
#' @param data A data frame containing the dataset.
#' @param vars A character vector of column names to use for clustering.
#' @param n_clusters An integer or vector specifying the number of clusters (G) to fit.
#' @param scaling Scaling method: "standardize" (z-score), "center" (mean only),
#'   "minmax" (0-1 range), or "none". Defaults to "standardize".
#' @param models Character vector of model names to test, or "all" for all 14 models.
#'   Valid models: EII, VII, EEI, VEI, EVI, VVI, EEE, EVE, VEE, VVE, EEV, VEV, EVV, VVV.
#' @param verbose Logical. If TRUE, prints progress messages. Defaults to TRUE.
#' @param na_action How to handle NAs: "omit" (remove rows) or "fail" (stop with error).
#'   Defaults to "omit".
#'
#' @return An object of class "moe_analysis" containing:
#' \itemize{
#'   \item{\code{models}}: List of fitted models with results
#'   \item{\code{data}}: Original and scaled data used
#'   \item{\code{parameters}}: Analysis parameters
#'   \item{\code{summary}}: Summary statistics including best model
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with iris data
#' results <- clustering(
#'   data = iris,
#'   vars = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'   n_clusters = 3
#' )
#'
#' # Test specific models
#' results <- clustering(
#'   data = iris,
#'   vars = c("Sepal.Length", "Sepal.Width"),
#'   n_clusters = 2:4,
#'   models = c("EEE", "VVV", "VEV")
#' )
#'
#' # View results using plot()
#' plot(results, type = "profile")
#' plot(results, type = "heatmap")
#' plot(results, type = "all")
#'
#' # Get cluster assignments
#' data_clustered <- get_cluster_assignments(results)
#'
#' # Assess stability
#' stability <- assess_cluster_stability(results)
#'
#' # Generate report
#' generate_cluster_report(results)
#' }
#'
#' @export
clustering <- function(data,
                       vars,
                       n_clusters,
                       scaling = "standardize",
                       models = "all",
                       verbose = TRUE,
                       na_action = "omit") {

  # -------------------------------------------------------------------------
  # Input Validation
  # -------------------------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!all(vars %in% names(data))) {
    missing <- setdiff(vars, names(data))
    stop("Variables not found in data: ", paste(missing, collapse = ", "))
  }

  if (!is.numeric(n_clusters) || any(n_clusters < 2)) {
    stop("n_clusters must be numeric value(s) >= 2")
  }

  if (!scaling %in% VALID_SCALING_METHODS) {
    stop("scaling must be one of: ", paste(VALID_SCALING_METHODS, collapse = ", "))
  }

  # -------------------------------------------------------------------------
  # Data Preparation
  # -------------------------------------------------------------------------
  cluster_data <- data[, vars, drop = FALSE]
  input_data <- data  # Keep reference to original
  cluster_vars <- vars  # Keep reference for internal use
  scaling_method <- scaling
  model_names <- models

  # Handle missing values
  complete_rows <- stats::complete.cases(cluster_data)
  n_removed <- sum(!complete_rows)

  if (n_removed > 0) {
    if (na_action == "fail") {
      stop("Data contains ", n_removed, " rows with missing values")
    }
    if (verbose) {
      message("Removed ", n_removed, " rows with missing values")
    }
    cluster_data <- cluster_data[complete_rows, ]
    input_data <- input_data[complete_rows, ]
  }

  if (nrow(cluster_data) < 10) {
    stop("Insufficient data: need at least 10 complete observations")
  }

  # Check for constant variables
  var_sds <- sapply(cluster_data, stats::sd, na.rm = TRUE)
  if (any(var_sds == 0)) {
    const_vars <- names(var_sds)[var_sds == 0]
    warning("Constant variable(s) detected: ", paste(const_vars, collapse = ", "),
            ". These may cause issues with some models.")
  }

  # Apply scaling
  scaled_data <- apply_scaling(cluster_data, scaling_method)

  # -------------------------------------------------------------------------
  # Determine Models to Test
  # -------------------------------------------------------------------------
  if (length(model_names) == 1 && model_names[1] == "all") {
    models_to_test <- MOECLUST_MODELS
  } else {
    invalid_models <- setdiff(model_names, MOECLUST_MODELS)
    if (length(invalid_models) > 0) {
      stop("Invalid model names: ", paste(invalid_models, collapse = ", "),
           "\nValid models: ", paste(MOECLUST_MODELS, collapse = ", "))
    }
    models_to_test <- model_names
  }

  # -------------------------------------------------------------------------
  # Check Dependencies
  # -------------------------------------------------------------------------
  if (!requireNamespace("MoEClust", quietly = TRUE)) {
    stop("MoEClust package required. Install with: install.packages('MoEClust')")
  }

  # -------------------------------------------------------------------------
  # Run Analysis
  # -------------------------------------------------------------------------
  results <- list()
  successful_models <- 0
  failed_models <- character(0)
  detailed_errors <- list()

  if (verbose) {
    cat("Starting MoE clustering analysis...\n")
    cat("Clusters (G):", paste(n_clusters, collapse = ", "), "\n")
    cat("Scaling:", scaling_method, "\n")
    cat("Variables:", paste(cluster_vars, collapse = ", "), "\n")
    cat("Sample size:", nrow(cluster_data), "\n")
    cat("Models:", paste(models_to_test, collapse = ", "), "\n\n")
  }

  for (G in n_clusters) {
    if (verbose) cat("Testing G =", G, "clusters:\n")

    for (model_name in models_to_test) {
      if (verbose) cat("  Fitting:", model_name, "... ")

      # Model identifier
      model_id <- if (length(n_clusters) > 1) {
        paste0(model_name, "_G", G)
      } else {
        model_name
      }

      # Try to fit model
      fit_result <- tryCatch({
        model_fit <- MoEClust::MoE_clust(
          data = scaled_data,
          G = G,
          modelNames = model_name,
          verbose = FALSE
        )

        if (is.null(model_fit) || is.null(model_fit$classification)) {
          stop("Model did not converge")
        }

        # Check cluster usage
        unique_clusters <- length(unique(model_fit$classification))
        if (unique_clusters < G) {
          stop(paste("Only", unique_clusters, "of", G, "clusters used"))
        }

        model_fit
      }, error = function(e) {
        if (verbose) cat("FAILED -", e$message, "\n")
        failed_models <<- c(failed_models, model_id)
        detailed_errors[[model_id]] <<- e$message
        return(NULL)
      })

      if (is.null(fit_result)) next

      # -----------------------------------------------------------------------
      # Generate Results
      # -----------------------------------------------------------------------
      cluster_assignments <- fit_result$classification

      # Original scale results
      original_results <- tryCatch({
        orig_with_cluster <- cbind(cluster_data, cluster = cluster_assignments)

        orig_means <- orig_with_cluster %>%
          dplyr::group_by(cluster) %>%
          dplyr::summarise(dplyr::across(dplyr::all_of(cluster_vars),
                                         \(x) mean(x, na.rm = TRUE)),
                          .groups = 'drop') %>%
          as.data.frame()

        cluster_sizes <- table(cluster_assignments)
        sizes_df <- data.frame(
          cluster = as.numeric(names(cluster_sizes)),
          size = as.numeric(cluster_sizes),
          percentage = round(as.numeric(cluster_sizes) / length(cluster_assignments) * 100, 1)
        )

        # Enhanced means with sizes
        enhanced_means <- merge(sizes_df, orig_means, by = "cluster") %>%
          dplyr::arrange(cluster)
        names(enhanced_means)[2:3] <- c("n_members", "percent")

        # Profile plot
        plot_data <- orig_with_cluster %>%
          tidyr::pivot_longer(cols = dplyr::all_of(cluster_vars),
                             names_to = "variable", values_to = "value") %>%
          dplyr::group_by(cluster, variable) %>%
          dplyr::summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')

        orig_plot <- ggplot2::ggplot(plot_data,
                                     ggplot2::aes(x = variable, y = mean_value,
                                                  group = cluster, color = factor(cluster))) +
          ggplot2::geom_line(linewidth = 1.2) +
          ggplot2::geom_point(size = 3) +
          ggplot2::labs(title = paste("Profile Plot -", model_id, "(Original Scale)"),
                       x = "Variables", y = "Mean Value", color = "Cluster") +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
          ggplot2::scale_color_viridis_d()

        list(means = orig_means, enhanced_means = enhanced_means,
             sizes = sizes_df, plot = orig_plot)
      }, error = function(e) {
        warning("Failed to generate original scale results: ", e$message)
        NULL
      })

      # Scaled data results
      scaled_results <- tryCatch({
        scaled_with_cluster <- cbind(scaled_data, cluster = cluster_assignments)

        scaled_means <- scaled_with_cluster %>%
          dplyr::group_by(cluster) %>%
          dplyr::summarise(dplyr::across(dplyr::all_of(cluster_vars),
                                         \(x) mean(x, na.rm = TRUE)),
                          .groups = 'drop') %>%
          as.data.frame()

        # Profile plot
        plot_data <- scaled_with_cluster %>%
          tidyr::pivot_longer(cols = dplyr::all_of(cluster_vars),
                             names_to = "variable", values_to = "value") %>%
          dplyr::group_by(cluster, variable) %>%
          dplyr::summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')

        scaled_plot <- ggplot2::ggplot(plot_data,
                                       ggplot2::aes(x = variable, y = mean_value,
                                                    group = cluster, color = factor(cluster))) +
          ggplot2::geom_line(linewidth = 1.2) +
          ggplot2::geom_point(size = 3) +
          ggplot2::labs(title = paste("Profile Plot -", model_id, "(Scaled Data)"),
                       x = "Variables", y = "Mean Value", color = "Cluster") +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
          ggplot2::scale_color_viridis_d()

        list(means = scaled_means, plot = scaled_plot)
      }, error = function(e) {
        warning("Failed to generate scaled results: ", e$message)
        NULL
      })

      # Store results
      if (!is.null(original_results) && !is.null(scaled_results)) {
        results[[model_id]] <- list(
          model_fit = fit_result,
          model_info = list(
            model_name = model_name,
            n_clusters = G,
            n_observations = nrow(cluster_data),
            variables = cluster_vars,
            scaling_method = scaling_method,
            loglik = fit_result$loglik,
            aic = fit_result$aic,
            bic = fit_result$bic,
            icl = fit_result$icl
          ),
          original_scale = original_results,
          scaled_data = scaled_results,
          cluster_assignments = cluster_assignments
        )

        successful_models <- successful_models + 1
        if (verbose) cat("SUCCESS\n")
      } else {
        if (verbose) cat("FAILED - Error generating results\n")
        failed_models <- c(failed_models, model_id)
      }
    }
  }

  # -------------------------------------------------------------------------
  # Summary
  # -------------------------------------------------------------------------
  if (verbose) {
    cat("\n", rep("=", 50), "\n")
    cat("ANALYSIS COMPLETE\n")
    cat(rep("=", 50), "\n")
    cat("Successful:", successful_models, "of",
        length(models_to_test) * length(n_clusters), "\n")

    if (length(failed_models) > 0) {
      cat("Failed:", paste(failed_models, collapse = ", "), "\n")
    }
  }

  # -------------------------------------------------------------------------
  # Build Output Object
  # -------------------------------------------------------------------------
  output <- list(
    models = results,
    data = list(
      original_data = cluster_data,
      scaled_data = scaled_data,
      full_input_data = input_data,
      n_removed = n_removed
    ),
    parameters = list(
      cluster_vars = cluster_vars,
      n_clusters = n_clusters,
      scaling_method = scaling_method,
      models_tested = models_to_test,
      sample_size = nrow(cluster_data)
    ),
    call = list(
      function_call = match.call(),
      analysis_date = Sys.time(),
      r_version = R.version.string
    ),
    summary = list(
      n_successful = successful_models,
      n_failed = length(failed_models),
      failed_models = failed_models,
      successful_models = names(results),
      detailed_errors = detailed_errors
    )
  )

  # Find best model by BIC
  if (successful_models > 0) {
    bic_values <- sapply(results, function(x) {
      bic <- x$model_info$bic
      if (is.null(bic) || is.na(bic)) Inf else bic
    })
    if (any(is.finite(bic_values))) {
      output$summary$best_model_bic <- names(which.min(bic_values))
      output$summary$best_bic_value <- min(bic_values, na.rm = TRUE)

      if (verbose) {
        cat("Best model (BIC):", output$summary$best_model_bic,
            "(BIC =", round(output$summary$best_bic_value, 2), ")\n")
      }
    }
  }

  class(output) <- c("moe_analysis", "list")
  return(output)
}

# =============================================================================
# VIEW RESULTS FUNCTION
# =============================================================================

#' View Results from MoEClust Analysis (Deprecated)
#'
#' @description
#' Display plots or tables from the analysis results. Supports multiple
#' visualization types including profile plots, heatmaps, bar charts, and
#' cluster size charts.
#'
#' Note: Consider using the S3 plot() method instead: plot(results, type = "...")
#'
#' @param results Object from clustering()
#' @param what What to display: "plots", "tables", "heatmap", "barchart",
#'   "cluster_sizes", or "all"
#' @param scale Data scale: "original" or "scaled"
#' @param model_name Specific model(s) to view, or "all"
#' @param cluster_range Optional cluster range to filter
#' @param plot_type Plot type: "profile", "heatmap", "barchart", "cluster_sizes", "all"
#' @param verbose Print headers and metadata
#' @param colors Optional custom colors for plots
#'
#' @return NULL (invisibly). Used for side effects.
#'
#' @export
view_results <- function(results,
                         what = "plots",
                         scale = "original",
                         model_name = "all",
                         cluster_range = NULL,
                         plot_type = "profile",
                         verbose = FALSE,
                         colors = NULL) {

  # Validation
  if (!inherits(results, "moe_analysis") && !is.list(results)) {
    stop("results must be from clustering()")
  }

  valid_what <- c("plots", "tables", "heatmap", "barchart", "cluster_sizes", "all")
  if (!what %in% valid_what) {
    stop("what must be one of: ", paste(valid_what, collapse = ", "))
  }

  if (!scale %in% c("original", "scaled")) {
    stop("scale must be 'original' or 'scaled'")
  }

  # Get models list
  models_data <- if ("models" %in% names(results)) results$models else results
  available_models <- names(models_data)

  if (length(available_models) == 0) {
    message("No successful models found.")
    return(invisible(NULL))
  }

  # Filter models
  if (length(model_name) == 1 && model_name[1] == "all") {
    models_to_display <- available_models
  } else {
    models_to_display <- intersect(model_name, available_models)
    if (length(models_to_display) == 0) {
      stop("No matching models. Available: ", paste(available_models, collapse = ", "))
    }
  }

  # Filter by cluster range
  if (!is.null(cluster_range)) {
    models_to_display <- models_to_display[sapply(models_to_display, function(m) {
      models_data[[m]]$model_info$n_clusters %in% cluster_range
    })]
  }

  # Display based on 'what' parameter
  for (model in models_to_display) {
    model_result <- models_data[[model]]
    cluster_vars <- model_result$model_info$variables

    if (verbose) {
      cat("\n--- Model:", model, "(G =", model_result$model_info$n_clusters, ") ---\n")
    }

    # Tables
    if (what %in% c("tables", "all")) {
      if (scale == "original") {
        cat("\nCluster Summary (Original Scale):\n")
        print(model_result$original_scale$enhanced_means)
      } else {
        cat("\nCluster Summary (Scaled):\n")
        print(model_result$scaled_data$means)
      }
      cat("\nCluster Sizes:\n")
      print(model_result$original_scale$sizes)
    }

    # Plots
    if (what %in% c("plots", "all") || plot_type == "profile") {
      if (scale == "original") {
        print(model_result$original_scale$plot)
      } else {
        print(model_result$scaled_data$plot)
      }
    }

    if (what == "heatmap" || plot_type == "heatmap" || (what == "all" && plot_type == "all")) {
      heatmap <- create_cluster_heatmap(model_result, cluster_vars, scale, model)
      print(heatmap)
    }

    if (what == "barchart" || plot_type == "barchart" || (what == "all" && plot_type == "all")) {
      barchart <- create_cluster_barchart(model_result, cluster_vars, scale, model)
      print(barchart)
    }

    if (what == "cluster_sizes" || plot_type == "cluster_sizes" || (what == "all" && plot_type == "all")) {
      sizes_chart <- create_cluster_sizes_chart(model_result, model, colors)
      print(sizes_chart)
    }
  }

  invisible(NULL)
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Compare Models by Information Criteria
#'
#' @param results Object from run_full_moe_analysis
#' @param sort_by Criterion to sort by: "bic", "aic", or "icl"
#'
#' @return Data frame with model comparison
#' @export
compare_models <- function(results, sort_by = "bic") {
  models_data <- if ("models" %in% names(results)) results$models else results

  if (length(models_data) == 0) {
    message("No models to compare")
    return(NULL)
  }

  comparison <- data.frame(
    model = names(models_data),
    n_clusters = sapply(models_data, function(x) x$model_info$n_clusters),
    loglik = sapply(models_data, function(x) x$model_info$loglik),
    aic = sapply(models_data, function(x) x$model_info$aic),
    bic = sapply(models_data, function(x) x$model_info$bic),
    icl = sapply(models_data, function(x) x$model_info$icl),
    stringsAsFactors = FALSE
  )

  comparison <- comparison[order(comparison[[sort_by]]), ]
  rownames(comparison) <- NULL

  comparison
}

#' Get Best Model
#'
#' @param results Object from run_full_moe_analysis
#' @param criterion Selection criterion: "bic", "aic", or "icl"
#'
#' @return Name of best model
#' @export
get_best_model <- function(results, criterion = "bic") {
  if (!criterion %in% c("bic", "aic", "icl")) {
    stop("criterion must be 'bic', 'aic', or 'icl'")
  }

  models_data <- if ("models" %in% names(results)) results$models else results
  values <- sapply(models_data, function(x) x$model_info[[criterion]])

  names(which.min(values))
}

#' List Available Models
#'
#' @param results Object from run_full_moe_analysis
#'
#' @return Character vector of model names (invisibly)
#' @export
list_models <- function(results) {
  models_data <- if ("models" %in% names(results)) results$models else results

  if (length(models_data) == 0) {
    cat("No models available.\n")
    return(invisible(character(0)))
  }

  cat("Available models:\n")
  for (i in seq_along(models_data)) {
    m <- names(models_data)[i]
    bic <- round(models_data[[m]]$model_info$bic, 2)
    cat(sprintf("  %d. %s (BIC: %.2f)\n", i, m, bic))
  }

  invisible(names(models_data))
}

# =============================================================================
# S3 METHODS
# =============================================================================

#' Print method for moe_analysis objects
#'
#' @param x An moe_analysis object
#' @param ... Additional arguments (ignored)
#' @export
print.moe_analysis <- function(x, ...) {
  cat("MoEClust Analysis Results\n")
  cat("=========================\n")
  cat("Sample size:", x$parameters$sample_size, "\n")
  cat("Variables:", paste(x$parameters$cluster_vars, collapse = ", "), "\n")
  cat("Scaling:", x$parameters$scaling_method, "\n")
  cat("Successful models:", x$summary$n_successful, "\n")

  if (!is.null(x$summary$best_model_bic)) {
    cat("Best model (BIC):", x$summary$best_model_bic,
        "(BIC =", round(x$summary$best_bic_value, 2), ")\n")
  }

  cat("\nUse plot() to visualize results:\n")
  cat("  plot(x)                    # Profile plot\n")
  cat("  plot(x, type = 'heatmap')  # Heatmap\n")
  cat("  plot(x, type = 'all')      # All plot types\n")
  cat("Use compare_models() or model_comparison_table() for model comparison\n")

  invisible(x)
}

#' Summary method for moe_analysis objects
#'
#' @param object An moe_analysis object
#' @param ... Additional arguments (ignored)
#' @export
summary.moe_analysis <- function(object, ...) {
  cat("\n=== MoEClust Analysis Summary ===\n\n")

  cat("Analysis Parameters:\n")
  cat("  Date:", as.character(object$call$analysis_date), "\n")
  cat("  Variables:", paste(object$parameters$cluster_vars, collapse = ", "), "\n")
  cat("  Scaling:", object$parameters$scaling_method, "\n")
  cat("  Clusters tested:", paste(object$parameters$n_clusters, collapse = ", "), "\n")
  cat("  Sample size:", object$parameters$sample_size, "\n")

  cat("\nResults:\n")
  cat("  Successful models:", object$summary$n_successful, "\n")
  cat("  Failed models:", object$summary$n_failed, "\n")

  if (!is.null(object$summary$best_model_bic)) {
    cat("  Best model (BIC):", object$summary$best_model_bic, "\n")
    cat("  Best BIC value:", round(object$summary$best_bic_value, 2), "\n")
  }

  if (length(object$summary$failed_models) > 0) {
    cat("\nFailed models:", paste(object$summary$failed_models, collapse = ", "), "\n")
  }

  cat("\n")
  invisible(compare_models(object))
}

#' Plot method for moe_analysis objects
#'
#' @description
#' Display visualizations from the clustering analysis. Supports multiple
#' plot types including profile plots, heatmaps, bar charts, cluster sizes,
#' and model comparison charts.
#'
#' @param x An moe_analysis object
#' @param type Type of plot: "profile" (default), "heatmap", "barchart",
#'   "sizes", "comparison", or "all" (displays all plot types)
#' @param model Model to plot. If NULL (default), uses best model by BIC.
#' @param scale Data scale for plots: "original" (default) or "scaled"
#' @param ... Additional arguments (currently ignored)
#'
#' @return The plot object(s) invisibly. When type = "all", returns a list
#'   of all generated plots.
#'
#' @examples
#' \dontrun{
#' results <- clustering(data, vars, n_clusters = 3)
#'
#' # Profile plot (default)
#' plot(results)
#'
#' # Heatmap
#' plot(results, type = "heatmap")
#'
#' # All plot types
#' plot(results, type = "all")
#'
#' # Specific model with scaled data
#' plot(results, type = "profile", model = "VVV", scale = "scaled")
#' }
#'
#' @export
plot.moe_analysis <- function(x, type = "profile", model = NULL, scale = "original", ...) {

  # Validate type

  valid_types <- c("profile", "heatmap", "barchart", "sizes", "comparison", "all")
  if (!type %in% valid_types) {
    stop("type must be one of: ", paste(valid_types, collapse = ", "))
  }

  # Validate scale
  if (!scale %in% c("original", "scaled")) {
    stop("scale must be 'original' or 'scaled'")
  }

  # Get model name
  if (is.null(model)) {
    model <- x$summary$best_model_bic
    if (is.null(model)) {
      model <- names(x$models)[1]
    }
  }

  # Validate model exists
  if (!model %in% names(x$models)) {
    stop("Model '", model, "' not found. Available models: ",
         paste(names(x$models), collapse = ", "))
  }

  model_result <- x$models[[model]]
  cluster_vars <- x$parameters$cluster_vars

  # Store plots for return
  plots <- list()

  # Profile plot
  if (type %in% c("profile", "all")) {
    if (scale == "original") {
      plots$profile <- model_result$original_scale$plot
    } else {
      plots$profile <- model_result$scaled_data$plot
    }
    if (type == "profile" || type == "all") {
      print(plots$profile)
    }
  }

  # Heatmap
  if (type %in% c("heatmap", "all")) {
    plots$heatmap <- create_cluster_heatmap(model_result, cluster_vars, scale, model)
    print(plots$heatmap)
  }

  # Bar chart
  if (type %in% c("barchart", "all")) {
    plots$barchart <- create_cluster_barchart(model_result, cluster_vars, scale, model)
    print(plots$barchart)
  }

  # Cluster sizes
  if (type %in% c("sizes", "all")) {
    plots$sizes <- create_cluster_sizes_chart(model_result, model)
    print(plots$sizes)
  }

  # Model comparison (BIC across models)
  if (type %in% c("comparison", "all")) {
    comparison <- compare_models(x, sort_by = "bic")
    if (!is.null(comparison) && nrow(comparison) > 1) {
      comparison$model <- factor(comparison$model, levels = comparison$model)

      plots$comparison <- ggplot2::ggplot(comparison,
                                          ggplot2::aes(x = model, y = bic, fill = factor(n_clusters))) +
        ggplot2::geom_col(alpha = 0.8, color = "white") +
        ggplot2::geom_text(ggplot2::aes(label = round(bic, 0)),
                           vjust = -0.3, size = 3) +
        ggplot2::labs(title = "Model Comparison by BIC",
                      subtitle = "Lower BIC indicates better fit",
                      x = "Model", y = "BIC", fill = "Clusters") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
          plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5, color = "gray40")
        ) +
        ggplot2::scale_fill_viridis_d()

      print(plots$comparison)
    } else if (type == "comparison") {
      message("Model comparison requires more than one successful model")
    }
  }

  # Return
  if (type == "all") {
    invisible(plots)
  } else if (length(plots) == 1) {
    invisible(plots[[1]])
  } else {
    invisible(NULL)
  }
}

# =============================================================================
# MODEL COMPARISON TABLE (GT)
# =============================================================================

#' Create Formatted Model Comparison Table
#'
#' @description
#' Generates a publication-ready gt table comparing all fitted models
#' by BIC, AIC, ICL, and log-likelihood. Highlights the best model
#' and provides interpretation guidance.
#'
#' @param results Object from clustering()
#' @param sort_by Criterion to sort by: "bic" (default), "aic", or "icl"
#' @param top_n Number of top models to display. NULL shows all. Defaults to NULL.
#' @param highlight_best Logical. Highlight the best model row. Defaults to TRUE.
#'
#' @return A gt table object
#'
#' @examples
#' \dontrun{
#' results <- clustering(data, vars, n_clusters = 3)
#' model_comparison_table(results)
#' model_comparison_table(results, sort_by = "aic", top_n = 5)
#' }
#'
#' @export
model_comparison_table <- function(results, sort_by = "bic", top_n = NULL,
                                   highlight_best = TRUE) {

  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("gt package required. Install with: install.packages('gt')")
  }

  # Get comparison data
  comparison <- compare_models(results, sort_by = sort_by)

  if (is.null(comparison) || nrow(comparison) == 0) {
    message("No models to compare")
    return(NULL)
  }

  # Limit to top_n if specified
  if (!is.null(top_n) && top_n < nrow(comparison)) {
    comparison <- comparison[1:top_n, ]
  }

  # Add rank column
  comparison$rank <- 1:nrow(comparison)

  # Calculate delta from best
  best_bic <- min(comparison$bic, na.rm = TRUE)
  best_aic <- min(comparison$aic, na.rm = TRUE)
  best_icl <- min(comparison$icl, na.rm = TRUE)

  comparison$delta_bic <- comparison$bic - best_bic
  comparison$delta_aic <- comparison$aic - best_aic
  comparison$delta_icl <- comparison$icl - best_icl

  # Identify best model
  comparison$is_best <- comparison$rank == 1

  # Reorder columns
  comparison <- comparison[, c("rank", "model", "n_clusters", "loglik",
                               "bic", "delta_bic", "aic", "delta_aic",
                               "icl", "delta_icl", "is_best")]

  # Create gt table
  gt_table <- comparison %>%
    dplyr::select(-is_best) %>%
    gt::gt() %>%
    gt::tab_header(
      title = "Model Comparison Summary",
      subtitle = paste0("Sorted by ", toupper(sort_by), " (lower is better)")
    ) %>%
    gt::cols_label(
      rank = "Rank",
      model = "Model",
      n_clusters = "G",
      loglik = "Log-Lik",
      bic = "BIC",
      delta_bic = "\u0394BIC",
      aic = "AIC",
      delta_aic = "\u0394AIC",
      icl = "ICL",
      delta_icl = "\u0394ICL"
    ) %>%
    gt::fmt_number(
      columns = c(loglik, bic, aic, icl),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns = c(delta_bic, delta_aic, delta_icl),
      decimals = 1
    ) %>%
    gt::cols_align(align = "center") %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) %>%
    gt::tab_options(
      table.font.size = gt::px(12),
      heading.title.font.size = gt::px(16),
      heading.subtitle.font.size = gt::px(12),
      column_labels.font.weight = "bold"
    ) %>%
    gt::tab_spanner(
      label = "Information Criteria",
      columns = c(bic, delta_bic, aic, delta_aic, icl, delta_icl)
    ) %>%
    gt::tab_footnote(
      footnote = paste0("G = number of clusters; \u0394 = difference from best model; ",
                        "BIC = Bayesian IC; AIC = Akaike IC; ICL = Integrated Complete-data Likelihood"),
      locations = gt::cells_title(groups = "subtitle")
    )

  # Highlight best model
  if (highlight_best) {
    gt_table <- gt_table %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#E8F5E9"),
          gt::cell_text(weight = "bold")
        ),
        locations = gt::cells_body(rows = 1)
      )
  }

  # Color code delta columns (green = good, red = bad) if scales is available
  if (requireNamespace("scales", quietly = TRUE)) {
    gt_table <- gt_table %>%
      gt::data_color(
        columns = c(delta_bic, delta_aic, delta_icl),
        fn = function(x) {
          scales::col_numeric(
            palette = c("#E8F5E9", "#FFEBEE"),
            domain = c(0, max(x, na.rm = TRUE))
          )(x)
        }
      )
  }

  return(gt_table)
}

# =============================================================================
# GET CLUSTER ASSIGNMENTS
# =============================================================================

#' Get Cluster Assignments with Original Data
#'
#' @description
#' Extracts cluster assignments from a fitted model and adds them to the
#' original data frame. Optionally includes cluster membership probabilities.
#'
#' @param results Object from clustering()
#' @param model_name Name of the model to use. If NULL (default), uses the best
#'   model by BIC.
#' @param include_probabilities Logical. If TRUE, includes probability of
#'   membership for each cluster. Defaults to FALSE.
#' @param cluster_col_name Name for the cluster assignment column.
#'   Defaults to "cluster".
#'
#' @return A data frame containing the original data plus cluster assignments
#'   (and optionally probabilities).
#'
#' @examples
#' \dontrun{
#' results <- clustering(data, vars, n_clusters = 3)
#'
#' # Get data with cluster assignments
#' data_with_clusters <- get_cluster_assignments(results)
#'
#' # Specify a particular model
#' data_with_clusters <- get_cluster_assignments(results, model_name = "VVV")
#'
#' # Include membership probabilities
#' data_with_clusters <- get_cluster_assignments(results, include_probabilities = TRUE)
#' }
#'
#' @export
get_cluster_assignments <- function(results, model_name = NULL,
                                    include_probabilities = FALSE,
                                    cluster_col_name = "cluster") {

  if (!inherits(results, "moe_analysis")) {
    stop("results must be from clustering()")
  }

  # Get model name
  if (is.null(model_name)) {
    model_name <- results$summary$best_model_bic
    if (is.null(model_name)) {
      model_name <- names(results$models)[1]
    }
    message("Using model: ", model_name)
  }

  # Validate model exists
  if (!model_name %in% names(results$models)) {
    stop("Model '", model_name, "' not found. Available models: ",
         paste(names(results$models), collapse = ", "))
  }

  # Get original data and assignments
  original_data <- results$data$full_input_data
  model_result <- results$models[[model_name]]
  assignments <- model_result$cluster_assignments

  # Add cluster column
  output_data <- original_data
  output_data[[cluster_col_name]] <- assignments

  # Add probabilities if requested
  if (include_probabilities) {
    model_fit <- model_result$model_fit

    if (!is.null(model_fit$z)) {
      probs <- as.data.frame(model_fit$z)
      n_clusters <- ncol(probs)
      names(probs) <- paste0("prob_cluster_", 1:n_clusters)

      # Add max probability (certainty)
      probs$cluster_certainty <- apply(probs, 1, max)

      output_data <- cbind(output_data, probs)
    } else {
      warning("Membership probabilities not available for this model")
    }
  }

  # Add metadata as attributes
  attr(output_data, "model_used") <- model_name
  attr(output_data, "n_clusters") <- model_result$model_info$n_clusters
  attr(output_data, "cluster_vars") <- results$parameters$cluster_vars

  return(output_data)
}

# =============================================================================
# BOOTSTRAP STABILITY ASSESSMENT
# =============================================================================

#' Assess Cluster Stability via Bootstrap
#'
#' @description
#' Evaluates the stability of cluster assignments for a SINGLE selected model
#' using bootstrap resampling. For each bootstrap sample, the clustering is
#' re-run with the same model specification and assignments are compared to
#' the original using the Adjusted Rand Index.
#'
#' Note: This function assesses one model at a time. If you tested multiple
#' models, use the model_name parameter to specify which model to assess,
#' or leave NULL to use the best model by BIC.
#'
#' @param results Object from clustering()
#' @param model_name Model to assess (single model). If NULL, uses best model by BIC.
#' @param n_boot Number of bootstrap iterations. Defaults to 100.
#' @param verbose Print progress. Defaults to TRUE.
#' @param seed Random seed for reproducibility. Defaults to NULL.
#'
#' @return A list of class "cluster_stability" containing:
#' \itemize{
#'   \item{\code{overall_stability}}: Mean Adjusted Rand Index across bootstraps
#'   \item{\code{stability_sd}}: Standard deviation of ARI
#'   \item{\code{bootstrap_ari}}: Vector of ARI values for each bootstrap
#'   \item{\code{observation_stability}}: Proportion of times each observation
#'     was assigned to the same cluster as in the original
#'   \item{\code{cluster_stability}}: Stability score for each cluster
#'   \item{\code{interpretation}}: Text interpretation of stability
#' }
#'
#' @examples
#' \dontrun{
#' results <- clustering(data, vars, n_clusters = 3)
#'
#' # Assess stability of best model (default)
#' stability <- assess_cluster_stability(results, n_boot = 100)
#'
#' # Assess a specific model
#' stability <- assess_cluster_stability(results, model_name = "VVV", n_boot = 50)
#'
#' # View results
#' print(stability)
#' stability$overall_stability
#' stability$interpretation
#'
#' # Plot observation stability
#' hist(stability$observation_stability, main = "Observation Stability")
#' }
#'
#' @export
assess_cluster_stability <- function(results, model_name = NULL, n_boot = 100,
                                     verbose = TRUE, seed = NULL) {

  if (!inherits(results, "moe_analysis")) {
    stop("results must be from clustering()")
  }

  # Set seed if provided
  if (!is.null(seed)) set.seed(seed)

  # Get model
  if (is.null(model_name)) {
    model_name <- results$summary$best_model_bic
    if (is.null(model_name)) model_name <- names(results$models)[1]
  }

  if (!model_name %in% names(results$models)) {
    stop("Model not found: ", model_name)
  }

  model_result <- results$models[[model_name]]
  original_assignments <- model_result$cluster_assignments
  n_obs <- length(original_assignments)
  n_clusters <- model_result$model_info$n_clusters
  scaling_method <- results$parameters$scaling_method
  cluster_vars <- results$parameters$cluster_vars

  # Get scaled data
  scaled_data <- results$data$scaled_data

  if (verbose) {
    cat("Assessing cluster stability for model:", model_name, "\n")
    cat("Bootstrap iterations:", n_boot, "\n")
    cat("Sample size:", n_obs, "\n")
    cat("Number of clusters:", n_clusters, "\n\n")
  }

  # Storage for results
  boot_ari <- numeric(n_boot)
  obs_same_cluster <- matrix(0, nrow = n_obs, ncol = n_boot)
  cluster_matches <- vector("list", n_boot)

  # Progress tracking
  if (verbose) cat("Running bootstrap iterations:\n")

  for (b in 1:n_boot) {
    if (verbose && b %% 10 == 0) cat("  Iteration", b, "of", n_boot, "\n")

    # Bootstrap sample (with replacement)
    boot_indices <- sample(1:n_obs, n_obs, replace = TRUE)
    boot_data <- scaled_data[boot_indices, , drop = FALSE]

    # Fit model to bootstrap sample
    boot_fit <- tryCatch({
      MoEClust::MoE_clust(
        data = boot_data,
        G = n_clusters,
        modelNames = gsub("_G[0-9]+$", "", model_name),
        verbose = FALSE
      )
    }, error = function(e) NULL)

    if (is.null(boot_fit) || is.null(boot_fit$classification)) {
      boot_ari[b] <- NA
      next
    }

    boot_assignments <- boot_fit$classification

    # Calculate Adjusted Rand Index
    # Only for observations that appear in bootstrap sample
    original_boot <- original_assignments[boot_indices]

    boot_ari[b] <- tryCatch({
      # Simple ARI calculation
      calc_ari(original_boot, boot_assignments)
    }, error = function(e) NA)

    # Track which observations were assigned to same cluster
    # Map bootstrap clusters to original clusters
    cluster_map <- find_cluster_mapping(original_boot, boot_assignments, n_clusters)

    for (i in 1:n_obs) {
      if (i %in% boot_indices) {
        boot_idx <- which(boot_indices == i)[1]
        mapped_cluster <- cluster_map[boot_assignments[boot_idx]]
        if (!is.na(mapped_cluster) && mapped_cluster == original_assignments[i]) {
          obs_same_cluster[i, b] <- 1
        }
      }
    }
  }

  # Calculate observation-level stability
  obs_stability <- rowMeans(obs_same_cluster, na.rm = TRUE)

  # Calculate cluster-level Jaccard similarity
  cluster_jaccard <- sapply(1:n_clusters, function(k) {
    orig_in_k <- which(original_assignments == k)
    prop_stable <- mean(obs_stability[orig_in_k], na.rm = TRUE)
    prop_stable
  })
  names(cluster_jaccard) <- paste0("Cluster_", 1:n_clusters)

  # Overall stability
  overall_ari <- mean(boot_ari, na.rm = TRUE)
  ari_sd <- sd(boot_ari, na.rm = TRUE)

  # Interpretation
  interpretation <- if (is.na(overall_ari)) {
    "Unable to assess stability (bootstrap failures)"
  } else if (overall_ari >= 0.9) {
    "Excellent stability: Cluster solution is highly reproducible"
  } else if (overall_ari >= 0.75) {
    "Good stability: Cluster solution is reasonably stable"
  } else if (overall_ari >= 0.5) {
    "Moderate stability: Some uncertainty in cluster assignments"
  } else {
    "Poor stability: Cluster solution may not be reliable"
  }

  # Build output
  output <- list(
    overall_stability = overall_ari,
    stability_sd = ari_sd,
    stability_ci = c(
      lower = overall_ari - 1.96 * ari_sd,
      upper = overall_ari + 1.96 * ari_sd
    ),
    bootstrap_ari = boot_ari,
    observation_stability = obs_stability,
    cluster_stability = cluster_jaccard,
    n_successful_boots = sum(!is.na(boot_ari)),
    model_name = model_name,
    n_boot = n_boot,
    interpretation = interpretation
  )

  class(output) <- c("cluster_stability", "list")

  if (verbose) {
    cat("\n=== Stability Results ===\n")
    cat("Overall ARI:", round(overall_ari, 3), "+/-", round(ari_sd, 3), "\n")
    cat("95% CI: [", round(output$stability_ci[1], 3), ",",
        round(output$stability_ci[2], 3), "]\n")
    cat("\nCluster-level stability:\n")
    for (k in 1:n_clusters) {
      cat("  Cluster", k, ":", round(cluster_jaccard[k], 3), "\n")
    }
    cat("\n", interpretation, "\n")
  }

  return(output)
}

#' Calculate Adjusted Rand Index
#' @noRd
calc_ari <- function(labels1, labels2) {
  # Contingency table
  tab <- table(labels1, labels2)
  n <- sum(tab)

  # Sum of combinations
  sum_comb_rows <- sum(choose(rowSums(tab), 2))
  sum_comb_cols <- sum(choose(colSums(tab), 2))
  sum_comb_tab <- sum(choose(tab, 2))
  comb_n <- choose(n, 2)

  # Expected index
  expected <- (sum_comb_rows * sum_comb_cols) / comb_n
  max_index <- (sum_comb_rows + sum_comb_cols) / 2

  # ARI
  if (max_index == expected) return(1)
  (sum_comb_tab - expected) / (max_index - expected)
}

#' Find optimal cluster mapping between two solutions
#' @noRd
find_cluster_mapping <- function(original, bootstrap, n_clusters) {
  # Create contingency table
  tab <- table(original, bootstrap)

  # Find best mapping using Hungarian algorithm (greedy approximation)
  mapping <- rep(NA, n_clusters)
  used_orig <- rep(FALSE, n_clusters)

  for (i in 1:min(n_clusters, ncol(tab))) {
    # Find maximum remaining
    remaining <- tab
    remaining[, which(1:ncol(tab) %in% mapping)] <- 0
    remaining[used_orig, ] <- 0

    if (max(remaining) == 0) break

    idx <- which(remaining == max(remaining), arr.ind = TRUE)[1, ]
    mapping[idx[2]] <- idx[1]
    used_orig[idx[1]] <- TRUE
  }

  mapping
}

#' Print method for cluster_stability
#' @param x A cluster_stability object
#' @param ... Additional arguments (ignored)
#' @export
print.cluster_stability <- function(x, ...) {
  cat("Cluster Stability Assessment\n")
  cat("============================\n")
  cat("Model:", x$model_name, "\n")
  cat("Bootstrap iterations:", x$n_boot, "(", x$n_successful_boots, "successful)\n\n")
  cat("Overall Adjusted Rand Index:", round(x$overall_stability, 3), "\n")
  cat("Standard Deviation:", round(x$stability_sd, 3), "\n")
  cat("95% CI: [", round(x$stability_ci[1], 3), ",", round(x$stability_ci[2], 3), "]\n\n")
  cat(x$interpretation, "\n")
  invisible(x)
}

# =============================================================================
# GENERATE CLUSTER REPORT
# =============================================================================

#' Generate Interpretable Cluster Report
#'
#' @description
#' Creates a comprehensive, interpretable report of the clustering results
#' including model selection summary, cluster profiles, and practical
#' interpretations.
#'
#' @param results Object from clustering()
#' @param model_name Model to report on. If NULL, uses best model by BIC.
#' @param output_format Output format: "console" (default), "gt" (returns gt tables),
#'   or "markdown" (returns markdown text).
#' @param include_recommendations Logical. Include interpretation guidelines. Defaults to TRUE.
#'
#' @return Depending on output_format:
#' \itemize{
#'   \item{"console"}: Prints report and returns NULL invisibly
#'   \item{"gt"}: Returns a list of gt table objects
#'   \item{"markdown"}: Returns markdown text as character string
#' }
#'
#' @examples
#' \dontrun{
#' results <- clustering(data, vars, n_clusters = 3)
#'
#' # Print to console
#' generate_cluster_report(results)
#'
#' # Get gt tables
#' tables <- generate_cluster_report(results, output_format = "gt")
#'
#' # Get markdown
#' md_text <- generate_cluster_report(results, output_format = "markdown")
#' }
#'
#' @export
generate_cluster_report <- function(results, model_name = NULL,
                                    output_format = "console",
                                    include_recommendations = TRUE) {

  if (!inherits(results, "moe_analysis")) {
    stop("results must be from clustering()")
  }

  # Get model
  if (is.null(model_name)) {
    model_name <- results$summary$best_model_bic
    if (is.null(model_name)) model_name <- names(results$models)[1]
  }

  if (!model_name %in% names(results$models)) {
    stop("Model not found: ", model_name)
  }

  model_result <- results$models[[model_name]]
  cluster_vars <- results$parameters$cluster_vars
  n_clusters <- model_result$model_info$n_clusters

  # Build report sections
  report <- list()

  # -------------------------------------------------------------------------
  # Section 1: Analysis Overview
  # -------------------------------------------------------------------------
  overview <- data.frame(
    Parameter = c("Analysis Date", "Sample Size", "Variables", "Scaling Method",
                  "Models Tested", "Successful Models", "Selected Model",
                  "Number of Clusters", "BIC", "AIC", "ICL"),
    Value = c(
      as.character(results$call$analysis_date),
      results$parameters$sample_size,
      paste(cluster_vars, collapse = ", "),
      results$parameters$scaling_method,
      length(results$parameters$models_tested),
      results$summary$n_successful,
      model_name,
      n_clusters,
      round(model_result$model_info$bic, 1),
      round(model_result$model_info$aic, 1),
      round(model_result$model_info$icl, 1)
    ),
    stringsAsFactors = FALSE
  )

  # -------------------------------------------------------------------------
  # Section 2: Cluster Sizes
  # -------------------------------------------------------------------------
  sizes <- model_result$original_scale$sizes
  sizes$interpretation <- sapply(sizes$percentage, function(p) {
    if (p < 5) "Very small (< 5%)"
    else if (p < 15) "Small (5-15%)"
    else if (p < 30) "Medium (15-30%)"
    else if (p < 50) "Large (30-50%)"
    else "Dominant (> 50%)"
  })

  # -------------------------------------------------------------------------
  # Section 3: Cluster Profiles (Original Scale)
  # -------------------------------------------------------------------------
  means <- model_result$original_scale$means

  # Calculate which clusters are highest/lowest on each variable
  profile_description <- list()
  for (var in cluster_vars) {
    var_means <- means[[var]]
    highest <- which.max(var_means)
    lowest <- which.min(var_means)
    range_val <- max(var_means) - min(var_means)

    profile_description[[var]] <- data.frame(
      Variable = var,
      Highest = paste0("Cluster ", highest, " (", round(var_means[highest], 2), ")"),
      Lowest = paste0("Cluster ", lowest, " (", round(var_means[lowest], 2), ")"),
      Range = round(range_val, 2),
      stringsAsFactors = FALSE
    )
  }
  profile_df <- do.call(rbind, profile_description)

  # -------------------------------------------------------------------------
  # Section 4: Cluster Characterizations
  # -------------------------------------------------------------------------
  # For each cluster, describe its distinctive features
  scaled_means <- model_result$scaled_data$means

  characterizations <- lapply(1:n_clusters, function(k) {
    cluster_profile <- scaled_means[k, cluster_vars]

    # Find variables where this cluster is notably high (> 0.5 SD) or low (< -0.5 SD)
    high_vars <- names(cluster_profile)[cluster_profile > 0.5]
    low_vars <- names(cluster_profile)[cluster_profile < -0.5]

    list(
      cluster = k,
      size = sizes$size[k],
      percent = sizes$percentage[k],
      high_on = if(length(high_vars) > 0) paste(high_vars, collapse = ", ") else "None notably",
      low_on = if(length(low_vars) > 0) paste(low_vars, collapse = ", ") else "None notably"
    )
  })

  # -------------------------------------------------------------------------
  # Output based on format
  # -------------------------------------------------------------------------
  if (output_format == "console") {
    cat("\n")
    cat(strrep("=", 70), "\n")
    cat("                    CLUSTER ANALYSIS REPORT\n")
    cat(strrep("=", 70), "\n\n")

    # Overview
    cat("1. ANALYSIS OVERVIEW\n")
    cat(strrep("-", 40), "\n")
    for (i in 1:nrow(overview)) {
      cat(sprintf("%-20s: %s\n", overview$Parameter[i], overview$Value[i]))
    }
    cat("\n")

    # Cluster sizes
    cat("2. CLUSTER SIZES\n")
    cat(strrep("-", 40), "\n")
    for (i in 1:nrow(sizes)) {
      cat(sprintf("Cluster %d: %d members (%.1f%%) - %s\n",
                  sizes$cluster[i], sizes$size[i], sizes$percentage[i],
                  sizes$interpretation[i]))
    }
    cat("\n")

    # Variable summary
    cat("3. VARIABLE DIFFERENTIATION\n")
    cat(strrep("-", 40), "\n")
    print(profile_df, row.names = FALSE)
    cat("\n")

    # Cluster characterizations
    cat("4. CLUSTER CHARACTERIZATIONS\n")
    cat(strrep("-", 40), "\n")
    for (char in characterizations) {
      cat(sprintf("\nCluster %d (%d members, %.1f%%):\n",
                  char$cluster, char$size, char$percent))
      cat(sprintf("  High on: %s\n", char$high_on))
      cat(sprintf("  Low on:  %s\n", char$low_on))
    }

    # Recommendations
    if (include_recommendations) {
      cat("\n")
      cat("5. INTERPRETATION GUIDELINES\n")
      cat(strrep("-", 40), "\n")
      cat("- 'High on' means > 0.5 SD above the sample mean\n")
      cat("- 'Low on' means > 0.5 SD below the sample mean\n")
      cat("- Consider cluster sizes when interpreting: very small clusters\n")
      cat("  may not be reliable or generalizable\n")
      cat("- Use assess_cluster_stability() to evaluate solution robustness\n")
      cat("- Consider validating clusters against external variables\n")
    }

    cat("\n")
    cat(strrep("=", 70), "\n")
    cat("Report generated:", as.character(Sys.time()), "\n")
    cat(strrep("=", 70), "\n")

    return(invisible(NULL))

  } else if (output_format == "gt") {
    if (!requireNamespace("gt", quietly = TRUE)) {
      stop("gt package required for gt output")
    }

    tables <- list()

    # Overview table
    tables$overview <- overview %>%
      gt::gt() %>%
      gt::tab_header(title = "Analysis Overview") %>%
      gt::cols_label(Parameter = "Parameter", Value = "Value") %>%
      gt::tab_options(table.font.size = gt::px(11))

    # Sizes table
    tables$sizes <- sizes %>%
      gt::gt() %>%
      gt::tab_header(title = "Cluster Sizes") %>%
      gt::fmt_number(columns = percentage, decimals = 1) %>%
      gt::tab_options(table.font.size = gt::px(11))

    # Profile table
    tables$profiles <- profile_df %>%
      gt::gt() %>%
      gt::tab_header(title = "Variable Differentiation Across Clusters") %>%
      gt::tab_options(table.font.size = gt::px(11))

    # Means table
    tables$means <- means %>%
      gt::gt() %>%
      gt::tab_header(title = "Cluster Means (Original Scale)") %>%
      gt::fmt_number(columns = dplyr::all_of(cluster_vars), decimals = 2) %>%
      gt::tab_options(table.font.size = gt::px(11))

    return(tables)

  } else if (output_format == "markdown") {
    md <- character()

    md <- c(md, "# Cluster Analysis Report\n")
    md <- c(md, paste0("*Generated: ", Sys.time(), "*\n\n"))

    md <- c(md, "## 1. Analysis Overview\n")
    md <- c(md, "| Parameter | Value |")
    md <- c(md, "|-----------|-------|")
    for (i in 1:nrow(overview)) {
      md <- c(md, paste0("| ", overview$Parameter[i], " | ", overview$Value[i], " |"))
    }
    md <- c(md, "\n")

    md <- c(md, "## 2. Cluster Sizes\n")
    md <- c(md, "| Cluster | N | % | Interpretation |")
    md <- c(md, "|---------|---|---|----------------|")
    for (i in 1:nrow(sizes)) {
      md <- c(md, paste0("| ", sizes$cluster[i], " | ", sizes$size[i], " | ",
                         sizes$percentage[i], "% | ", sizes$interpretation[i], " |"))
    }
    md <- c(md, "\n")

    md <- c(md, "## 3. Cluster Characterizations\n")
    for (char in characterizations) {
      md <- c(md, paste0("### Cluster ", char$cluster,
                         " (", char$size, " members, ", char$percent, "%)\n"))
      md <- c(md, paste0("- **High on:** ", char$high_on, "\n"))
      md <- c(md, paste0("- **Low on:** ", char$low_on, "\n\n"))
    }

    return(paste(md, collapse = "\n"))
  }
}
