# Saqrmisc Package: Clustering Analysis via latent profile analysis
#
# Thin wrapper around the `latents` package.
# clustering() -> latents::lpa() / latents::enumerate_classes()
# plot() / plot_clustering() -> latents::plot.multilpa()

#' @importFrom stats complete.cases fitted
#' @importFrom utils packageVersion
NULL

# =============================================================================
# CONSTANTS
# =============================================================================

#' Covariance structure codes (mclust naming)
#' @noRd
COVARIANCE_MODELS <- c("EII", "VII", "EEI", "VEI", "EVI", "VVI",
                        "EEE", "VEE", "EVE", "VVE", "EEV", "VEV", "EVV", "VVV")

#' Valid scaling methods
#' @noRd
VALID_SCALING_METHODS <- c("standardize", "center", "minmax", "none")

# =============================================================================
# SCALING HELPERS
# =============================================================================

#' Safe min-max scaling that handles constant variables
#' @param x Numeric vector
#' @return Scaled numeric vector
#' @noRd
safe_minmax_scale <- function(x) {
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (rng == 0) return(rep(0.5, length(x)))
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
         center = as.data.frame(scale(data, center = TRUE, scale = FALSE)),
         minmax = as.data.frame(lapply(data, safe_minmax_scale)),
         none = data,
         stop("Invalid scaling method"))
}

# =============================================================================
# INTERNAL: translate 3-letter code to latents args
# =============================================================================

#' @noRd
.structure_to_latents_args <- function(model_code) {
  stopifnot(
    "`model_code` must be a supported covariance code" =
      model_code %in% COVARIANCE_MODELS
  )
  spherical <- model_code %in% c("EII", "VII")
  letter <- function(pos) {
    if (substr(model_code, pos, pos) == "E") "equal" else "varying"
  }
  args <- list(volume = letter(1L),
               shape = if (spherical) "spherical" else letter(2L))
  if (spherical) return(args)
  third <- substr(model_code, 3L, 3L)
  args$orientation <- if (third == "I") "axis" else letter(3L)
  args
}

# =============================================================================
# MAIN VERB
# =============================================================================

#' Latent Profile Clustering
#'
#' @md
#' @description
#' Fits latent profile models via the \pkg{latents} package. A single
#' `n_profiles` value fits one model with [latents::lpa()]; multiple values
#' or covariance `models` enumerate candidates with
#' [latents::enumerate_classes()] and select the best by BIC.
#'
#' @param data A data frame.
#' @param vars Character vector of column names to cluster on.
#' @param n_profiles Number of profiles (clusters). A single integer for one
#'   model, or a range like `2:5` for enumeration. Alias: `n_clusters`.
#' @param n_clusters Alias for `n_profiles`.
#' @param scaling Scaling applied before fitting: `"standardize"` (default),
#'   `"center"`, `"minmax"`, or `"none"`.
#' @param models Covariance structure(s) to fit. A character vector of
#'   three-letter mclust codes (e.g. `"EEE"`, `"VVI"`) or `"all"` for all 14.
#'   Defaults to `"VVI"`.
#' @param n_starts Number of random starts per model. Defaults to 10.
#' @param seed Random seed for reproducibility.
#' @param verbose Print progress. Defaults to `TRUE`.
#' @param na_action How to handle missing values: `"omit"` (default) drops
#'   incomplete rows, `"fail"` raises an error.
#'
#' @return An object of class `"saqr_clustering"` containing:
#' \describe{
#'   \item{fit}{The best `multilpa` object (from \pkg{latents}).}
#'   \item{enumeration}{The `multilpa_enumeration` grid, or `NULL` when only
#'     one candidate was fitted.}
#'   \item{data}{List with `original_data`, `scaled_data`, `full_input_data`,
#'     `complete_rows`, and `n_removed`.}
#'   \item{parameters}{List with `cluster_vars`, `n_profiles`, `scaling_method`,
#'     `models_tested`, and `sample_size`.}
#' }
#'
#' Use `plot()` to visualise, `as.data.frame()` for the profile means,
#' `fitted()` for observations with assignments, and `summary()` for the
#' enumeration table.
#'
#' @seealso [plot_clustering()], [latents::lpa()],
#'   [latents::enumerate_classes()]
#'
#' @examples
#' \donttest{
#' fit <- clustering(iris,
#'   vars = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'   n_profiles = 3, models = "EEE", seed = 1)
#' fit
#' plot(fit)
#' as.data.frame(fit)
#' fitted(fit)
#' }
#'
#' @export
clustering <- function(data,
                       vars,
                       n_profiles = NULL,
                       n_clusters = NULL,
                       scaling = "standardize",
                       models = "VVI",
                       n_starts = 10L,
                       seed = NULL,
                       verbose = TRUE,
                       na_action = "omit") {

  # -- resolve n_profiles / n_clusters alias --------------------------------
  n_profiles <- n_profiles %||% n_clusters
  stopifnot(
    "`data` must be a data frame" = is.data.frame(data),
    "`vars` must be column names in `data`" =
      is.character(vars) && all(vars %in% names(data)),
    "`n_profiles` (or `n_clusters`) is required" = !is.null(n_profiles),
    "`n_profiles` must be integer(s) >= 2" =
      is.numeric(n_profiles) && all(n_profiles >= 2),
    "`scaling` must be one of: standardize, center, minmax, none" =
      scaling %in% VALID_SCALING_METHODS
  )

  if (!requireNamespace("latents", quietly = TRUE)) {
    stop(errorCondition(
      "The `latents` package is required. Install with: pak::pak(\"mohsaqr/latents\")",
      class = "saqrmisc_missing_dep", call = NULL))
  }

  # -- resolve models -------------------------------------------------------
  if (length(models) == 1L && identical(models, "all")) {
    models_to_test <- COVARIANCE_MODELS
  } else {
    invalid <- setdiff(models, COVARIANCE_MODELS)
    if (length(invalid) > 0L) {
      stop(errorCondition(
        sprintf("Invalid model name(s): %s\nValid: %s",
                paste(invalid, collapse = ", "),
                paste(COVARIANCE_MODELS, collapse = ", ")),
        class = "saqrmisc_bad_input", call = NULL))
    }
    models_to_test <- models
  }

  # -- data preparation -----------------------------------------------------
  full_input_data <- data
  cluster_data <- data[, vars, drop = FALSE]
  complete_rows <- stats::complete.cases(cluster_data)
  n_removed <- sum(!complete_rows)

  if (n_removed > 0L) {
    if (identical(na_action, "fail")) {
      stop(errorCondition(
        sprintf("Data contains %d rows with missing values", n_removed),
        class = "saqrmisc_bad_input", call = NULL))
    }
    if (verbose) message("Removed ", n_removed, " rows with missing values")
    cluster_data <- cluster_data[complete_rows, , drop = FALSE]
  }

  if (nrow(cluster_data) < 10L) {
    stop(errorCondition(
      "Insufficient data: need at least 10 complete observations",
      class = "saqrmisc_bad_input", call = NULL))
  }

  scaled_data <- apply_scaling(cluster_data, scaling)

  # -- fit ------------------------------------------------------------------
  enumerate <- length(n_profiles) > 1L || length(models_to_test) > 1L

  if (verbose) {
    cat("Latent profile analysis\n")
    cat("  Profiles:", paste(n_profiles, collapse = ", "), "\n")
    cat("  Models:", paste(models_to_test, collapse = ", "), "\n")
    cat("  Scaling:", scaling, " | n:", nrow(scaled_data),
        " | vars:", length(vars), "\n")
  }

  enumeration <- NULL
  best_fit <- NULL

  if (enumerate) {
    enumeration <- latents::enumerate_classes(
      data = scaled_data,
      vars = vars,
      id = NULL,
      n_profiles = n_profiles,
      model = models_to_test,
      n_starts = n_starts,
      seed = seed
    )
    # find best converged model by BIC (bic_individual for single-level)
    tab <- enumeration$table
    converged <- tab$converged & !is.na(tab$bic_individual)
    if (!any(converged)) {
      stop(errorCondition(
        "No model converged. Try simpler models or fewer profiles.",
        class = "saqrmisc_no_converged", call = NULL))
    }
    best_row <- which(converged)[which.min(tab$bic_individual[converged])]
    best_fit <- latents::candidate_fit(
      enumeration,
      n_profiles = tab$n_profiles[best_row],
      model = tab$model[best_row]
    )
    if (verbose) {
      n_ok <- sum(converged)
      cat(sprintf("  %d of %d converged; best: %s with %d profiles (BIC = %.1f)\n",
                  n_ok, nrow(tab), tab$model[best_row],
                  tab$n_profiles[best_row], tab$bic_individual[best_row]))
    }
  } else {
    struct_args <- .structure_to_latents_args(models_to_test)
    best_fit <- do.call(latents::lpa, c(
      list(data = scaled_data, vars = vars,
           n_profiles = n_profiles, n_starts = n_starts, seed = seed),
      struct_args
    ))
    if (!best_fit$converged) {
      warning("Model did not converge. Consider more starts or a simpler model.",
              call. = FALSE)
    }
    if (verbose) {
      cat(sprintf("  %s with %d profiles: logLik = %.1f, BIC = %.1f, converged = %s\n",
                  best_fit$covariance_structure, n_profiles,
                  best_fit$log_likelihood, best_fit$bic,
                  best_fit$converged))
    }
  }

  structure(
    list(
      fit = best_fit,
      enumeration = enumeration,
      data = list(
        original_data = cluster_data,
        scaled_data = scaled_data,
        full_input_data = full_input_data,
        complete_rows = complete_rows,
        n_removed = n_removed
      ),
      parameters = list(
        cluster_vars = vars,
        n_profiles = n_profiles,
        scaling_method = scaling,
        models_tested = models_to_test,
        sample_size = nrow(cluster_data)
      )
    ),
    class = "saqr_clustering"
  )
}

#' @rdname clustering
#' @export
cluster <- clustering

# =============================================================================
# S3 METHODS
# =============================================================================

#' @export
print.saqr_clustering <- function(x, ...) {
  fit <- x$fit
  cat("Latent Profile Clustering\n")
  cat(sprintf("  n = %d, %d variables, scaling = %s\n",
              x$parameters$sample_size, length(x$parameters$cluster_vars),
              x$parameters$scaling_method))
  cat(sprintf("  Best model: %s with %d profiles\n",
              fit$covariance_structure, fit$n_profiles))
  cat(sprintf("  BIC = %.1f, logLik = %.1f, converged = %s\n",
              fit$bic, fit$log_likelihood, fit$converged))
  if (!is.null(x$enumeration)) {
    n_converged <- sum(x$enumeration$table$converged, na.rm = TRUE)
    cat(sprintf("  Enumeration: %d candidates, %d converged\n",
                nrow(x$enumeration$table), n_converged))
  }
  cat("\nUse plot() to visualise, as.data.frame() for profile means,\n")
  cat("fitted() for observations with assignments, summary() for the grid.\n")
  invisible(x)
}

#' Summarise a Clustering Result
#'
#' @param object A `saqr_clustering` object.
#' @param ... Ignored.
#' @return When an enumeration was fitted, a data.frame with one row per
#'   candidate model. Otherwise a one-row summary of the single fit.
#' @export
summary.saqr_clustering <- function(object, ...) {

  if (!is.null(object$enumeration)) {
    tab <- object$enumeration$table
    best_bic <- min(tab$bic_individual[tab$converged], na.rm = TRUE)
    tab$delta_bic <- tab$bic_individual - best_bic
    tab$best <- !is.na(tab$bic_individual) & tab$converged &
      vapply(tab$bic_individual, \(v) isTRUE(all.equal(v, best_bic)),
             logical(1L))
    out <- tab[, c("n_profiles", "model", "log_likelihood", "n_parameters",
                    "aic", "bic_individual", "delta_bic", "icl_individual",
                    "profile_entropy", "converged", "best"),
               drop = FALSE]
    names(out)[names(out) == "bic_individual"] <- "bic"
    names(out)[names(out) == "icl_individual"] <- "icl"
    rownames(out) <- NULL
    return(out)
  }

  fit <- object$fit
  data.frame(
    n_profiles = fit$n_profiles,
    model = fit$covariance_structure,
    log_likelihood = fit$log_likelihood,
    n_parameters = fit$n_parameters,
    aic = fit$aic,
    bic = fit$bic,
    icl = if ("icl_individual" %in% names(fit)) fit$icl_individual else NA_real_,
    profile_entropy = if (fit$n_profiles > 1L) {
      .lpa_relative_entropy(fit$subject_posteriors)
    } else NA_real_,
    converged = fit$converged,
    best = TRUE,
    stringsAsFactors = FALSE
  )
}

#' @noRd
.lpa_relative_entropy <- function(posteriors) {
  if (is.null(posteriors) || ncol(posteriors) <= 1L) return(NA_real_)
  positive <- posteriors > 0
  e <- -sum(posteriors[positive] * log(posteriors[positive]))
  1 - e / (nrow(posteriors) * log(ncol(posteriors)))
}

#' Extract Profile Means
#'
#' @param x A `saqr_clustering` object.
#' @param row.names,optional Ignored.
#' @param ... Ignored.
#' @return A data.frame with one row per profile and indicator, with columns
#'   `profile`, `indicator`, `mean`, `variance`, `standard_deviation`.
#' @export
as.data.frame.saqr_clustering <- function(x, row.names = NULL,
                                          optional = FALSE, ...) {
  as.data.frame(x$fit)
}

#' Extract Fitted Observations with Profile Assignments
#'
#' @param object A `saqr_clustering` object.
#' @param ... Ignored.
#' @return A data.frame with all columns of the input data plus `profile`,
#'   `uncertainty`, and posterior probability columns. Rows removed during
#'   fitting get `NA` fitted values.
#' @export
fitted.saqr_clustering <- function(object, ...) {
  assignments <- latents::get_results(object$fit, what = "assignments")
  complete_rows <- object$data$complete_rows
  full_data <- object$data$full_input_data

  if (all(complete_rows)) return(cbind(full_data, assignments[, -(seq_along(object$parameters$cluster_vars)), drop = FALSE]))

  n_full <- nrow(full_data)
  extra_cols <- setdiff(names(assignments), object$parameters$cluster_vars)
  pad <- as.data.frame(
    matrix(NA_real_, nrow = n_full, ncol = length(extra_cols),
           dimnames = list(NULL, extra_cols))
  )
  pad[complete_rows, ] <- assignments[, extra_cols, drop = FALSE]
  cbind(full_data, pad)
}

# =============================================================================
# BACKWARD-COMPATIBLE ACCESSORS
# =============================================================================

#' Get Cluster Assignments with Original Data
#'
#' @param results A `saqr_clustering` object.
#' @param model_name Ignored (kept for backward compatibility).
#' @param include_probabilities Logical. Include posterior probabilities.
#' @param cluster_col_name Name for the assignment column.
#'
#' @return A data.frame with the original data plus a cluster/profile column.
#' @export
get_cluster_assignments <- function(results, model_name = NULL,
                                    include_probabilities = FALSE,
                                    cluster_col_name = "cluster") {
  if (inherits(results, "saqr_clustering")) {
    out <- fitted(results)
    names(out)[names(out) == "profile"] <- cluster_col_name
    if (!include_probabilities) {
      post_cols <- grep("^posterior_profile_|^uncertainty$", names(out))
      if (length(post_cols) > 0L) out <- out[, -post_cols, drop = FALSE]
    }
    return(out)
  }
  stop(errorCondition(
    "`results` must be a saqr_clustering object from clustering()",
    class = "saqrmisc_bad_input", call = NULL))
}

#' Compare Models from an Enumeration
#'
#' @param results A `saqr_clustering` object.
#' @param sort_by Criterion to sort by: `"bic"` (default), `"aic"`, or
#'   `"icl"`.
#'
#' @return A data.frame, or `NULL` if no enumeration was run.
#' @export
compare_models <- function(results, sort_by = "bic") {
  if (!inherits(results, "saqr_clustering")) {
    stop("`results` must be from clustering()")
  }
  if (is.null(results$enumeration)) {
    message("Only one model was fitted; use n_profiles = 2:5 or multiple models to compare.")
    return(NULL)
  }
  tab <- summary(results)
  col <- if (sort_by == "icl") "icl" else sort_by
  tab[order(tab[[col]]), , drop = FALSE]
}

#' Get Best Model Name
#'
#' @param results A `saqr_clustering` object.
#' @param criterion Selection criterion.
#' @param what What to return: `"name"` or `"fit"`.
#'
#' @return A character string (model name) or a `multilpa` fit.
#' @export
get_best_model <- function(results, criterion = "bic",
                           what = c("name", "fit")) {
  what <- match.arg(what)
  if (!inherits(results, "saqr_clustering")) {
    stop("`results` must be from clustering()")
  }
  switch(what,
         name = results$fit$covariance_structure,
         fit = results$fit)
}

#' List Available Models
#'
#' @param results A `saqr_clustering` object.
#'
#' @return Character vector of model names (invisibly).
#' @export
list_models <- function(results) {
  if (!inherits(results, "saqr_clustering")) {
    stop("`results` must be from clustering()")
  }
  if (is.null(results$enumeration)) {
    cat("Single model:", results$fit$covariance_structure,
        "with", results$fit$n_profiles, "profiles\n")
    return(invisible(results$fit$covariance_structure))
  }
  tab <- results$enumeration$table
  ok <- tab[tab$converged, , drop = FALSE]
  cat("Converged models:\n")
  vapply(seq_len(nrow(ok)), \(i) {
    cat(sprintf("  %d. %s (G=%d, BIC=%.1f)\n",
                i, ok$model[i], ok$n_profiles[i], ok$bic_individual[i]))
    ok$model[i]
  }, character(1L))
  invisible(paste0(ok$model, "_G", ok$n_profiles))
}

# =============================================================================
# DIAGNOSTICS
# =============================================================================

#' Classification Diagnostics
#'
#' @param results A `saqr_clustering` object.
#'
#' @return A `multilpa_diagnostics` object (from \pkg{latents}).
#' @export
cluster_diagnostics <- function(results) {
  if (!inherits(results, "saqr_clustering")) {
    stop("`results` must be from clustering()")
  }
  latents::diagnostics(results$fit)
}

# =============================================================================
# MODEL COMPARISON TABLE (GT)
# =============================================================================

#' Create Formatted Model Comparison Table
#'
#' @param results A `saqr_clustering` object.
#' @param sort_by Criterion to sort by: `"bic"` (default), `"aic"`.
#' @param top_n Number of top models to display. `NULL` shows all.
#' @param highlight_best Highlight the best model row.
#'
#' @return A gt table object, or `NULL` if no enumeration was run.
#' @export
model_comparison_table <- function(results, sort_by = "bic", top_n = NULL,
                                   highlight_best = TRUE) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("gt package required. Install with: install.packages('gt')")
  }
  comparison <- compare_models(results, sort_by = sort_by)
  if (is.null(comparison) || nrow(comparison) == 0L) return(NULL)

  if (!is.null(top_n) && top_n < nrow(comparison)) {
    comparison <- comparison[seq_len(top_n), , drop = FALSE]
  }
  comparison$rank <- seq_len(nrow(comparison))

  display <- comparison[, c("rank", "n_profiles", "model", "log_likelihood",
                            "aic", "bic", "delta_bic", "profile_entropy",
                            "converged"),
                        drop = FALSE]

  gt_table <- gt::gt(display) |>
    gt::tab_header(
      title = "Model Comparison",
      subtitle = paste0("Sorted by ", toupper(sort_by), " (lower is better)")
    ) |>
    gt::fmt_number(columns = c("log_likelihood", "aic", "bic", "delta_bic"),
                   decimals = 1) |>
    gt::fmt_number(columns = "profile_entropy", decimals = 3) |>
    gt::cols_align(align = "center")

  if (highlight_best) {
    gt_table <- gt_table |>
      gt::tab_style(
        style = list(gt::cell_fill(color = "#E8F5E9"),
                     gt::cell_text(weight = "bold")),
        locations = gt::cells_body(rows = 1)
      )
  }
  gt_table
}

# =============================================================================
# REPORT
# =============================================================================

#' Generate Cluster Report
#'
#' @param results A `saqr_clustering` object.
#' @param output_format `"console"` (default), `"gt"`, or `"markdown"`.
#' @param include_recommendations Include interpretation guidelines.
#'
#' @return Invisibly `NULL` for console; a gt table list for `"gt"`;
#'   a character string for `"markdown"`.
#' @export
generate_cluster_report <- function(results,
                                    output_format = "console",
                                    include_recommendations = TRUE) {
  if (!inherits(results, "saqr_clustering")) {
    stop("`results` must be from clustering()")
  }
  fit <- results$fit
  vars <- results$parameters$cluster_vars
  n_profiles <- fit$n_profiles

  assignments <- latents::get_results(fit, what = "assignments")
  profile_col <- assignments$profile
  sizes <- tabulate(profile_col, nbins = n_profiles)
  pcts <- round(100 * sizes / sum(sizes), 1)

  means_df <- as.data.frame(fit)

  if (identical(output_format, "console")) {
    cat("\n", strrep("=", 60), "\n")
    cat("        LATENT PROFILE CLUSTERING REPORT\n")
    cat(strrep("=", 60), "\n\n")

    cat("Model:", fit$covariance_structure, "| Profiles:", n_profiles, "\n")
    cat("n =", results$parameters$sample_size,
        "| Scaling:", results$parameters$scaling_method, "\n")
    cat("BIC =", round(fit$bic, 1),
        "| LogLik =", round(fit$log_likelihood, 1),
        "| Converged:", fit$converged, "\n\n")

    cat("Profile sizes:\n")
    vapply(seq_len(n_profiles), \(k) {
      cat(sprintf("  Profile %d: %d (%.1f%%)\n", k, sizes[k], pcts[k]))
      ""
    }, character(1L))

    cat("\nProfile means (scaled data):\n")
    print(means_df, row.names = FALSE)

    if (include_recommendations) {
      cat("\nUse plot(x) for visual profiles, diagnostics(x) for classification quality.\n")
    }
    cat(strrep("=", 60), "\n")
    return(invisible(NULL))
  }

  if (identical(output_format, "markdown")) {
    md <- sprintf("# Latent Profile Clustering Report\n\n")
    md <- paste0(md, sprintf("**Model:** %s | **Profiles:** %d | **n:** %d\n\n",
                             fit$covariance_structure, n_profiles,
                             results$parameters$sample_size))
    md <- paste0(md, "## Profile sizes\n\n| Profile | n | % |\n|---|---|---|\n")
    vapply(seq_len(n_profiles), \(k) {
      md <<- paste0(md, sprintf("| %d | %d | %.1f%% |\n", k, sizes[k], pcts[k]))
      ""
    }, character(1L))
    return(md)
  }

  if (identical(output_format, "gt")) {
    if (!requireNamespace("gt", quietly = TRUE)) {
      stop("gt package required for gt output")
    }
    size_df <- data.frame(Profile = seq_len(n_profiles), n = sizes,
                          Percent = pcts)
    list(
      sizes = gt::gt(size_df) |> gt::tab_header(title = "Profile Sizes"),
      means = gt::gt(means_df) |> gt::tab_header(title = "Profile Means") |>
        gt::fmt_number(columns = c("mean", "variance", "standard_deviation"),
                       decimals = 3)
    )
  }
}

# =============================================================================
# ALIASES (backward compatibility)
# =============================================================================

#' @rdname clustering
#' @export
cluster_fit <- clustering

#' @rdname list_models
#' @export
cluster_models <- list_models

#' @rdname get_best_model
#' @export
cluster_best <- get_best_model

#' @rdname compare_models
#' @export
cluster_compare <- compare_models

#' @rdname model_comparison_table
#' @export
cluster_compare_table <- model_comparison_table

#' @rdname get_cluster_assignments
#' @export
cluster_assignments <- get_cluster_assignments

#' @rdname generate_cluster_report
#' @export
cluster_report <- generate_cluster_report

#' @rdname cluster_diagnostics
#' @export
cluster_stability <- cluster_diagnostics
