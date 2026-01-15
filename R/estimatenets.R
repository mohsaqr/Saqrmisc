# Saqrmisc Package: Network Estimation Functions
#
# This file contains functions for network estimation and analysis using bootnet/mgm/qgraph.
# Functions: estimate_single_network, estimate_grouped_networks, compare_networks

#' @importFrom stats na.omit predict
#' @importFrom grDevices dev.off png pdf svg
NULL

# =============================================================================
# CONSTANTS
# =============================================================================

#' Default node color
#' @noRd
DEFAULT_NODE_COLOR <- "#EEEEEE"

#' Valid network estimation methods
#' @noRd
VALID_NETWORK_METHODS <- c("EBICglasso", "ggmModSelect", "pcor", "IsingFit",
                           "IsingSampler", "huge", "adalasso", "mgm")

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Detect variable types for MGM
#'
#' @description
#' Automatically detects whether variables are Gaussian (continuous),
#' categorical, or Poisson (count) based on their properties.
#'
#' @param df Data frame to analyze
#' @param categorical_threshold Maximum unique values for automatic categorical detection
#' @return Character vector of types ("g", "c", "p")
#' @noRd
detect_var_types <- function(df, categorical_threshold = 5) {
  sapply(df, function(x) {
    # Remove NAs for detection
    x_clean <- x[!is.na(x)]

    if (length(x_clean) == 0) return("g")

    # Check if already factor or character
    if (is.factor(x) || is.character(x)) {
      return("c")  # Categorical
    }

    # Check number of unique values
    n_unique <- length(unique(x_clean))

    if (n_unique <= categorical_threshold) {
      # Check if values are integer-like
      if (all(x_clean == floor(x_clean))) {
        if (min(x_clean) >= 0) {
          return("p")  # Poisson (count data)
        }
      }
      return("c")  # Categorical
    }

    return("g")  # Gaussian (continuous)
  })
}

#' Validate network inputs
#'
#' @param df Data frame
#' @param Vars Variable names
#' @param method Network estimation method
#' @noRd
validate_network_inputs <- function(df, Vars, method) {
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  if (nrow(df) < 10) {
    stop("Need at least 10 observations for network estimation")
  }

  if (!is.null(Vars)) {
    missing_vars <- setdiff(Vars, names(df))
    if (length(missing_vars) > 0) {
      stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
    }
  }

  if (!method %in% VALID_NETWORK_METHODS) {
    warning("Method '", method, "' may not be supported. ",
            "Standard methods: ", paste(VALID_NETWORK_METHODS, collapse = ", "))
  }
}

# =============================================================================
# MAIN FUNCTIONS
# =============================================================================

#' Estimate a Single Network
#'
#' @description
#' Estimates a single psychological network from a data frame using regularized
#' partial correlations (via bootnet) and Mixed Graphical Models (via mgm).
#' Provides network visualization with predictability metrics (R-squared pie charts).
#'
#' @param df A data frame containing the variables to be analyzed.
#' @param Vars A character vector specifying the names of variables to include.
#'   If NULL (default), all columns of the data frame are used.
#' @param layout Character string specifying the layout algorithm for the network plot.
#'   Options: "circle", "spring", "groups", etc. Defaults to "circle".
#' @param color Character string or vector specifying node colors. Defaults to "#EEEEEE".
#' @param title Title for the network plot. Defaults to "Between-person network".
#' @param default Network estimation method for bootnet::estimateNetwork.
#'   Options: "EBICglasso" (default), "ggmModSelect", "pcor", etc.
#' @param var_types Character vector of variable types for MGM ("g" = Gaussian,
#'   "c" = categorical, "p" = Poisson). If NULL (default), types are auto-detected.
#' @param qgraph_args List of additional arguments passed to qgraph::qgraph.
#' @param network_args List of additional arguments passed to bootnet::estimateNetwork.
#' @param verbose Logical. If TRUE, prints progress messages. Defaults to TRUE.
#' @param compute_centrality Logical. If TRUE, computes centrality measures. Defaults to TRUE.
#'
#' @return A list of class "network_analysis" containing:
#' \itemize{
#'   \item{\code{network_object}}: Full network object from bootnet
#'   \item{\code{network_matrix}}: Adjacency/weight matrix
#'   \item{\code{prediction}}: MGM prediction results (R2, RMSE)
#'   \item{\code{centrality}}: Centrality measures (if compute_centrality = TRUE)
#'   \item{\code{qgraph}}: The qgraph plot object
#'   \item{\code{var_types}}: Variable types used for MGM
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' result <- estimate_single_network(
#'   df = my_data,
#'   Vars = c("var1", "var2", "var3", "var4", "var5")
#' )
#'
#' # Access network matrix
#' result$network_matrix
#'
#' # View predictability
#' result$prediction
#'
#' # View centrality
#' result$centrality
#'
#' # Custom settings
#' result <- estimate_single_network(
#'   df = my_data,
#'   Vars = c("var1", "var2", "var3"),
#'   layout = "spring",
#'   default = "ggmModSelect",
#'   var_types = c("g", "g", "c"),  # Manual type specification
#'   verbose = FALSE
#' )
#' }
#'
#' @export
estimate_single_network <- function(df,
                                    Vars = NULL,
                                    layout = "circle",
                                    color = DEFAULT_NODE_COLOR,
                                    title = "Between-person network",
                                    default = "EBICglasso",
                                    var_types = NULL,
                                    qgraph_args = list(),
                                    network_args = list(),
                                    verbose = TRUE,
                                    compute_centrality = TRUE) {

  # ===========================================================================
  # Check Dependencies
  # ===========================================================================
  required_pkgs <- c("bootnet", "mgm", "qgraph")
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required. Install with: install.packages('", pkg, "')")
    }
  }

  # ===========================================================================
  # Input Validation
  # ===========================================================================
  if (is.null(Vars)) {
    Vars <- names(df)
  }

  validate_network_inputs(df, Vars, default)

  # ===========================================================================
  # Data Preparation
  # ===========================================================================
  subgroup_data <- df[, Vars, drop = FALSE]
  labels <- colnames(subgroup_data)
  num_vars <- ncol(subgroup_data)

  # Handle missing values
  complete_rows <- complete.cases(subgroup_data)
  n_removed <- sum(!complete_rows)

  if (n_removed > 0) {
    if (verbose) message("Removed ", n_removed, " rows with missing values")
    subgroup_data <- subgroup_data[complete_rows, ]
  }

  if (nrow(subgroup_data) < 10) {
    stop("Insufficient complete cases: ", nrow(subgroup_data))
  }

  # ===========================================================================
  # Variable Type Detection
  # ===========================================================================
  if (is.null(var_types)) {
    var_types <- detect_var_types(subgroup_data)
    if (verbose) {
      cat("Auto-detected variable types:\n")
      for (i in seq_along(labels)) {
        type_name <- switch(var_types[i],
                           "g" = "Gaussian",
                           "c" = "Categorical",
                           "p" = "Poisson")
        cat("  ", labels[i], ": ", type_name, "\n", sep = "")
      }
    }
  } else {
    if (length(var_types) != num_vars) {
      stop("var_types length (", length(var_types), ") must match number of variables (", num_vars, ")")
    }
  }

  # ===========================================================================
  # MGM Model for Predictability
  # ===========================================================================
  if (verbose) cat("\nEstimating MGM model for predictability...\n")

  mgm_result <- tryCatch({
    mgm_fit <- mgm::mgm(
      data = as.matrix(subgroup_data),
      type = var_types,
      level = rep(1, num_vars),  # All continuous for now
      pbar = FALSE,
      warnings = FALSE
    )

    prediction <- stats::predict(mgm_fit, subgroup_data)

    if (verbose) {
      cat("Predictability metrics:\n")
      cat("  Mean R2:", round(mean(prediction$error$R2, na.rm = TRUE), 3), "\n")
      cat("  Mean RMSE:", round(mean(prediction$error$RMSE, na.rm = TRUE), 3), "\n")
    }

    list(fit = mgm_fit, prediction = prediction)
  }, error = function(e) {
    warning("MGM estimation failed: ", e$message, "\nProceeding without predictability metrics.")
    list(fit = NULL, prediction = list(error = data.frame(R2 = rep(NA, num_vars))))
  })

  # ===========================================================================
  # Network Estimation
  # ===========================================================================
  if (verbose) cat("\nEstimating network structure...\n")

  network_result <- tryCatch({
    network_args_combined <- c(
      list(data = subgroup_data, default = default),
      network_args
    )

    network <- do.call(bootnet::estimateNetwork, network_args_combined)
    network
  }, error = function(e) {
    stop("Network estimation failed: ", e$message)
  })

  network_matrix <- network_result$graph

  # ===========================================================================
  # Centrality Measures
  # ===========================================================================
  centrality <- NULL
  if (compute_centrality) {
    if (verbose) cat("Computing centrality measures...\n")

    centrality <- tryCatch({
      cent <- qgraph::centrality_auto(network_matrix)

      # Create summary data frame
      data.frame(
        node = labels,
        strength = cent$node.centrality$Strength,
        closeness = cent$node.centrality$Closeness,
        betweenness = cent$node.centrality$Betweenness,
        expected_influence = cent$node.centrality$ExpectedInfluence
      )
    }, error = function(e) {
      warning("Centrality computation failed: ", e$message)
      NULL
    })
  }

  # ===========================================================================
  # Network Visualization
  # ===========================================================================
  if (verbose) cat("Creating network visualization...\n")

  # Calculate layout
  layout_obj <- tryCatch({
    qgraph::averageLayout(list(network_result), layout = layout, repulsion = 1)
  }, error = function(e) {
    warning("Layout calculation failed, using default")
    layout
  })

  # Get R2 values for pie charts
  r2_values <- mgm_result$prediction$error$R2
  if (all(is.na(r2_values))) r2_values <- NULL

  # Default qgraph arguments
  default_qgraph_args <- list(
    input = network_matrix,
    layout = layout_obj,
    labels = labels,
    theme = "colorblind",
    pie = r2_values,
    vsize = 10,
    minimum = 0.05,
    border.color = '#555555',
    label.color = "#555555",
    color = color,
    edge.labels = TRUE,
    edge.label.cex = 0.55,
    edge.label.color = "black",
    cut = 0.1,
    title = title
  )

  # Remove input from custom args to avoid conflicts
  qgraph_args$input <- NULL

  # Combine arguments
  qgraph_args_combined <- modifyList(default_qgraph_args, qgraph_args)

  # Create plot
  qgraph_plot <- tryCatch({
    do.call(qgraph::qgraph, qgraph_args_combined)
  }, error = function(e) {
    warning("qgraph visualization failed: ", e$message)
    NULL
  })

  # ===========================================================================
  # Build Output
  # ===========================================================================
  output <- list(
    network_object = network_result,
    network_matrix = network_matrix,
    prediction = mgm_result$prediction$error,
    centrality = centrality,
    qgraph = qgraph_plot,
    var_types = var_types,
    labels = labels,
    n_observations = nrow(subgroup_data),
    n_removed = n_removed,
    method = default,
    call = match.call()
  )

  class(output) <- c("network_analysis", "list")

  if (verbose) cat("\nNetwork estimation complete.\n")

  return(output)
}


#' Estimate Grouped Networks
#'
#' @description
#' Estimates networks for different groups within a data frame, using
#' `estimate_single_network` internally. Produces separate network visualizations
#' and metrics for each group.
#'
#' @param df A data frame containing the variables and group variable.
#' @param Vars Character vector of variable names to include in the network.
#'   If NULL (default), all columns except GroupVar are used.
#' @param GroupVar Character string specifying the grouping variable name.
#' @param layout Layout algorithm for network plots. Defaults to "circle".
#' @param color Node color(s). Defaults to "#EEEEEE".
#' @param default Network estimation method. Defaults to "EBICglasso".
#' @param var_types Variable types for MGM. If NULL, auto-detected.
#' @param qgraph_args Additional arguments for qgraph.
#' @param network_args Additional arguments for bootnet::estimateNetwork.
#' @param verbose Print progress messages. Defaults to TRUE.
#' @param min_group_size Minimum group size required. Groups with fewer
#'   observations are skipped. Defaults to 30.
#'
#' @return A list of class "grouped_network_analysis" containing:
#' \itemize{
#'   \item{\code{networks}}: List of network objects per group
#'   \item{\code{network_matrices}}: List of adjacency matrices per group
#'   \item{\code{predictions}}: List of prediction results per group
#'   \item{\code{centralities}}: List of centrality measures per group
#'   \item{\code{qgraphs}}: List of qgraph plot objects per group
#'   \item{\code{group_sizes}}: Sample sizes per group
#'   \item{\code{failed_groups}}: Groups that failed estimation
#' }
#'
#' @examples
#' \dontrun{
#' # Estimate networks by group
#' results <- estimate_grouped_networks(
#'   df = my_data,
#'   Vars = c("var1", "var2", "var3", "var4"),
#'   GroupVar = "group"
#' )
#'
#' # Access results for specific group
#' results$networks$GroupA
#' results$predictions$GroupA
#'
#' # Compare networks
#' compare_networks(results, groups = c("GroupA", "GroupB"))
#' }
#'
#' @export
estimate_grouped_networks <- function(df,
                                      Vars = NULL,
                                      GroupVar,
                                      layout = "circle",
                                      color = DEFAULT_NODE_COLOR,
                                      default = "EBICglasso",
                                      var_types = NULL,
                                      qgraph_args = list(),
                                      network_args = list(),
                                      verbose = TRUE,
                                      min_group_size = 30) {

  # ===========================================================================
  # Input Validation
  # ===========================================================================
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  if (missing(GroupVar) || is.null(GroupVar) || !(GroupVar %in% names(df))) {
    stop("GroupVar must be specified and present in the data frame")
  }

  # If Vars is not provided, use all columns except GroupVar
  if (is.null(Vars)) {
    Vars <- setdiff(names(df), GroupVar)
  }

  # Validate Vars
  missing_vars <- setdiff(Vars, names(df))
  if (length(missing_vars) > 0) {
    stop("Variables not found: ", paste(missing_vars, collapse = ", "))
  }

  # ===========================================================================
  # Get Groups
  # ===========================================================================
  unique_groups <- unique(df[[GroupVar]])
  unique_groups <- unique_groups[!is.na(unique_groups)]

  if (verbose) {
    cat("Found", length(unique_groups), "groups:", paste(unique_groups, collapse = ", "), "\n\n")
  }

  # ===========================================================================
  # Initialize Storage
  # ===========================================================================
  network_list <- list()
  network_matrix_list <- list()
  prediction_list <- list()
  centrality_list <- list()
  qgraph_list <- list()
  group_sizes <- list()
  failed_groups <- character(0)

  # ===========================================================================
  # Estimate Network for Each Group
  # ===========================================================================
  for (group in unique_groups) {
    group_label <- as.character(group)

    if (verbose) cat("=== Processing group:", group_label, "===\n")

    # Subset data for this group
    subgroup_data <- df[df[[GroupVar]] == group, , drop = FALSE]
    n_group <- nrow(subgroup_data)

    if (verbose) cat("  N =", n_group, "\n")

    # Check minimum group size
    if (n_group < min_group_size) {
      if (verbose) cat("  Skipping: group size <", min_group_size, "\n\n")
      failed_groups <- c(failed_groups, group_label)
      next
    }

    # Estimate network
    result <- tryCatch({
      estimate_single_network(
        df = subgroup_data,
        Vars = Vars,
        layout = layout,
        color = color,
        title = group_label,
        default = default,
        var_types = var_types,
        qgraph_args = qgraph_args,
        network_args = network_args,
        verbose = FALSE,
        compute_centrality = TRUE
      )
    }, error = function(e) {
      if (verbose) cat("  FAILED:", e$message, "\n")
      failed_groups <<- c(failed_groups, group_label)
      return(NULL)
    })

    if (!is.null(result)) {
      network_list[[group_label]] <- result$network_object
      network_matrix_list[[group_label]] <- result$network_matrix
      prediction_list[[group_label]] <- result$prediction
      centrality_list[[group_label]] <- result$centrality
      qgraph_list[[group_label]] <- result$qgraph
      group_sizes[[group_label]] <- n_group

      if (verbose) cat("  SUCCESS\n\n")
    }
  }

  # ===========================================================================
  # Build Output
  # ===========================================================================
  output <- list(
    networks = network_list,
    network_matrices = network_matrix_list,
    predictions = prediction_list,
    centralities = centrality_list,
    qgraphs = qgraph_list,
    group_sizes = group_sizes,
    failed_groups = failed_groups,
    variables = Vars,
    group_var = GroupVar,
    n_successful = length(network_list),
    n_failed = length(failed_groups),
    call = match.call()
  )

  class(output) <- c("grouped_network_analysis", "list")

  if (verbose) {
    cat("=== Summary ===\n")
    cat("Successful:", length(network_list), "groups\n")
    if (length(failed_groups) > 0) {
      cat("Failed:", paste(failed_groups, collapse = ", "), "\n")
    }
  }

  return(output)
}


#' Compare Two Networks
#'
#' @description
#' Performs Network Comparison Test (NCT) to compare network structure
#' and global strength between two groups.
#'
#' @param results Object from estimate_grouped_networks
#' @param groups Character vector of length 2 specifying which groups to compare
#' @param it Number of permutations for NCT. Defaults to 1000.
#' @param test Which tests to perform: "network" (structure), "global" (strength),
#'   "both" (default).
#' @param verbose Print progress. Defaults to TRUE.
#'
#' @return A list containing NCT results with p-values for network structure
#'   and global strength differences.
#'
#' @examples
#' \dontrun{
#' # First estimate grouped networks
#' grouped_results <- estimate_grouped_networks(df, Vars, GroupVar = "group")
#'
#' # Then compare two groups
#' comparison <- compare_networks(
#'   grouped_results,
#'   groups = c("GroupA", "GroupB"),
#'   it = 500
#' )
#' }
#'
#' @export
compare_networks <- function(results,
                             groups,
                             it = 1000,
                             test = "both",
                             verbose = TRUE) {

  # Validate inputs
  if (!inherits(results, "grouped_network_analysis")) {
    stop("results must be from estimate_grouped_networks")
  }

  if (length(groups) != 2) {
    stop("groups must be a character vector of length 2")
  }

  available_groups <- names(results$networks)
  missing_groups <- setdiff(groups, available_groups)
  if (length(missing_groups) > 0) {
    stop("Groups not found: ", paste(missing_groups, collapse = ", "),
         "\nAvailable: ", paste(available_groups, collapse = ", "))
  }

  # Check for NCT package
  if (!requireNamespace("NetworkComparisonTest", quietly = TRUE)) {
    stop("NetworkComparisonTest package required. Install with: install.packages('NetworkComparisonTest')")
  }

  if (verbose) {
    cat("Comparing networks:", groups[1], "vs", groups[2], "\n")
    cat("Permutations:", it, "\n")
  }

  # Get network matrices
  net1 <- results$network_matrices[[groups[1]]]
  net2 <- results$network_matrices[[groups[2]]]

  # Determine which tests to run
  test_network <- test %in% c("network", "both")
  test_global <- test %in% c("global", "both")

  # Run NCT
  nct_result <- tryCatch({
    NetworkComparisonTest::NCT(
      data1 = net1,
      data2 = net2,
      it = it,
      test.edges = test_network,
      test.centrality = FALSE,
      progressbar = verbose
    )
  }, error = function(e) {
    stop("NCT failed: ", e$message)
  })

  if (verbose) {
    cat("\n=== Network Comparison Results ===\n")
    cat("Groups:", groups[1], "(n=", results$group_sizes[[groups[1]]], ") vs ",
        groups[2], "(n=", results$group_sizes[[groups[2]]], ")\n\n")

    cat("Network structure difference p-value:", round(nct_result$pval, 4), "\n")

    if (!is.null(nct_result$glstrinv.pval)) {
      cat("Global strength difference p-value:", round(nct_result$glstrinv.pval, 4), "\n")
    }
  }

  return(nct_result)
}


# =============================================================================
# S3 METHODS
# =============================================================================

#' Print method for network_analysis objects
#'
#' @param x A network_analysis object
#' @param ... Additional arguments (ignored)
#' @export
print.network_analysis <- function(x, ...) {
  cat("Network Analysis Results\n")
  cat("========================\n")
  cat("Method:", x$method, "\n")
  cat("Variables:", length(x$labels), "\n")
  cat("Observations:", x$n_observations, "\n")

  if (!is.null(x$prediction) && !all(is.na(x$prediction$R2))) {
    cat("Mean predictability (R2):", round(mean(x$prediction$R2, na.rm = TRUE), 3), "\n")
  }

  cat("\nUse $network_matrix to access the adjacency matrix\n")
  cat("Use $centrality to view centrality measures\n")

  invisible(x)
}

#' Print method for grouped_network_analysis objects
#'
#' @param x A grouped_network_analysis object
#' @param ... Additional arguments (ignored)
#' @export
print.grouped_network_analysis <- function(x, ...) {
  cat("Grouped Network Analysis Results\n")
  cat("================================\n")
  cat("Groups analyzed:", x$n_successful, "\n")
  cat("Groups failed:", x$n_failed, "\n")
  cat("Variables:", length(x$variables), "\n")

  if (length(x$group_sizes) > 0) {
    cat("\nGroup sizes:\n")
    for (g in names(x$group_sizes)) {
      cat("  ", g, ": n =", x$group_sizes[[g]], "\n")
    }
  }

  cat("\nUse compare_networks() to compare two groups\n")

  invisible(x)
}

#' Summary method for network_analysis objects
#'
#' @param object A network_analysis object
#' @param ... Additional arguments (ignored)
#' @export
summary.network_analysis <- function(object, ...) {
  cat("\n=== Network Analysis Summary ===\n\n")

  cat("Estimation method:", object$method, "\n")
  cat("Number of nodes:", length(object$labels), "\n")
  cat("Sample size:", object$n_observations, "\n")

  if (object$n_removed > 0) {
    cat("Observations removed (missing):", object$n_removed, "\n")
  }

  cat("\nPredictability (R2):\n")
  if (!is.null(object$prediction) && !all(is.na(object$prediction$R2))) {
    r2_summary <- summary(object$prediction$R2)
    print(r2_summary)
  } else {
    cat("  Not available\n")
  }

  if (!is.null(object$centrality)) {
    cat("\nTop 5 nodes by Strength:\n")
    top_strength <- object$centrality[order(-object$centrality$strength), ][1:min(5, nrow(object$centrality)), ]
    print(top_strength[, c("node", "strength")], row.names = FALSE)
  }

  cat("\n")
  invisible(object)
}
