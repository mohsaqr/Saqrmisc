# Saqrmisc Package: latent profile clustering
#
# A thin layer over the `latents` package. clustering() scales the data, fits
# every candidate with latents::enumerate_lpa() and returns the BIC-best fit.
# The result IS a `multilpa` fit (subclassed), so every latents verb --
# diagnostics(), get_results(), as.data.frame(), predict() -- works on it
# directly; Saqrmisc adds only print(), summary(), fitted() and plot().

#' @importFrom stats complete.cases fitted sd
NULL

#' @noRd
.covariance_models <- c("EII", "VII", "EEI", "VEI", "EVI", "VVI",
                        "EEE", "VEE", "EVE", "VVE", "EEV", "VEV", "EVV", "VVV")

#' Scale indicator columns before fitting
#' @param data Data frame of numeric indicators, no missing values.
#' @param method One of `"standardize"`, `"center"`, `"minmax"`, `"none"`.
#' @return A data frame with the same names and dimensions.
#' @noRd
.scale_indicators <- function(data, method) {
  rescale <- switch(method,
    standardize = \(x) (x - mean(x)) / stats::sd(x),
    center      = \(x) x - mean(x),
    minmax      = \(x) (x - min(x)) / (max(x) - min(x)),
    none        = identity
  )
  data[] <- lapply(data, rescale)
  data
}

#' Row of the candidate grid with the lowest BIC among converged fits
#' @return A single integer row index. Ties go to the first row.
#' @noRd
.best_candidate <- function(grid) {
  usable <- grid$converged & is.finite(grid$bic_individual)
  if (!any(usable)) {
    stop(errorCondition(
      "No candidate model converged. Try fewer profiles or simpler `models`.",
      class = "saqrmisc_no_converged", call = NULL))
  }
  which(usable)[which.min(grid$bic_individual[usable])]
}

#' Latent Profile Clustering
#'
#' @md
#' @description
#' Fits latent profile models with the \pkg{latents} package and keeps the
#' candidate with the lowest BIC. Every combination of `n_profiles` and
#' `models` is fitted by [latents::enumerate_lpa()]; a single combination is
#' simply a grid of one.
#'
#' The result is a \pkg{latents} fit, so its verbs apply directly:
#' `as.data.frame(fit)` for profile means, `latents::diagnostics(fit)` for
#' classification quality, `latents::get_results(fit, what)` for any table.
#'
#' @param data A data frame.
#' @param vars Character vector of numeric columns to cluster on.
#' @param n_profiles Number(s) of profiles, each at least 2: one value, or a
#'   range such as `2:5` to compare.
#' @param n_clusters Alias for `n_profiles`.
#' @param models Covariance structure(s) as three-letter mclust codes (e.g.
#'   `"EEE"`, `"VVI"`), or `"all"` for all 14.
#' @param scaling Transformation applied to `vars` before fitting:
#'   `"standardize"` (mean 0, SD 1), `"center"`, `"minmax"` (0 to 1), or
#'   `"none"`.
#' @param n_starts Random starts per candidate.
#' @param seed Random seed passed to \pkg{latents}.
#' @param na_action `"omit"` drops rows with a missing value in `vars`;
#'   `"fail"` raises an error instead.
#' @param verbose Report progress with [message()].
#'
#' @return An object of class `c("saqr_clustering", "multilpa")`: the
#'   selected \pkg{latents} fit, fitted to the scaled data. Methods:
#'   `print()`, `summary()` (one row per candidate), `fitted()` (the input
#'   data with profile assignments), `plot()`.
#'
#' @section Errors:
#' Raises `saqrmisc_bad_input` for unknown `models`, non-numeric or constant
#' `vars`, missing values under `na_action = "fail"`, or fewer than 10
#' complete rows; `saqrmisc_no_converged` when no candidate converged.
#'
#' @references
#' Vermunt, J. K. (2003). Multilevel latent class models. *Sociological
#' Methodology*, 33, 213-239.
#'
#' @seealso [plot_clustering()], [latents::enumerate_lpa()]
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
#' fit <- clustering(iris, vars, n_profiles = 2:4, models = c("EEE", "VVI"))
#' fit
#' summary(fit)
#' as.data.frame(fit)
#' fitted(fit)
#' plot(fit)
#' }
#'
#' @export
clustering <- function(data,
                       vars,
                       n_profiles = NULL,
                       n_clusters = NULL,
                       models = "VVI",
                       scaling = c("standardize", "center", "minmax", "none"),
                       n_starts = 10L,
                       seed = NULL,
                       na_action = c("omit", "fail"),
                       verbose = TRUE) {
  n_profiles <- n_profiles %||% n_clusters
  scaling <- match.arg(scaling)
  na_action <- match.arg(na_action)
  stopifnot(
    "`data` must be a data frame" = is.data.frame(data),
    "`vars` must be column names of `data`" =
      is.character(vars) && length(vars) >= 1L && all(vars %in% names(data)),
    "`n_profiles` (or `n_clusters`) must be whole numbers of at least 2" =
      is.numeric(n_profiles) && length(n_profiles) >= 1L &&
      all(n_profiles >= 2) && all(n_profiles %% 1 == 0),
    "`models` must be a character vector" = is.character(models)
  )
  models <- if (identical(models, "all")) .covariance_models else models
  unknown <- setdiff(models, .covariance_models)
  if (length(unknown) > 0L) .bad_input(sprintf(
    "Unknown `models`: %s. Use %s, or \"all\".",
    paste(unknown, collapse = ", "), paste(.covariance_models, collapse = ", ")))

  indicators <- data[vars]
  numeric_vars <- vapply(indicators, is.numeric, logical(1L))
  if (!all(numeric_vars)) .bad_input(sprintf(
    "`vars` must be numeric; not numeric: %s",
    paste(vars[!numeric_vars], collapse = ", ")))

  complete_rows <- stats::complete.cases(indicators)
  n_removed <- sum(!complete_rows)
  if (n_removed > 0L && identical(na_action, "fail")) .bad_input(sprintf(
    "%d rows have a missing value in `vars` (na_action = \"fail\").",
    n_removed))
  indicators <- indicators[complete_rows, , drop = FALSE]
  if (nrow(indicators) < 10L) .bad_input(
    "Need at least 10 complete rows to cluster.")

  constant <- vapply(indicators, \(x) isTRUE(all.equal(min(x), max(x))),
                     logical(1L))
  if (any(constant)) .bad_input(sprintf(
    "`vars` must vary; constant: %s", paste(vars[constant], collapse = ", ")))

  if (verbose) {
    if (n_removed > 0L) message(sprintf(
      "Removed %d rows with missing values.", n_removed))
    message(sprintf("Fitting %d candidate(s): %s profiles x %s.",
                    length(n_profiles) * length(models),
                    paste(n_profiles, collapse = ", "),
                    paste(models, collapse = ", ")))
  }

  scaled <- .scale_indicators(indicators, scaling)
  enumeration <- latents::enumerate_lpa(
    scaled, vars, n_profiles = n_profiles, model = models,
    n_starts = n_starts, seed = seed)
  grid <- as.data.frame(enumeration)
  best <- .best_candidate(grid)
  fit <- latents::candidate_fit(enumeration,
                                n_profiles = grid$n_profiles[best],
                                model = grid$model[best])

  if (nzchar(grid$warnings[best])) {
    warning(warningCondition(
      sprintf("Selected model %s with %d profiles: %s", grid$model[best],
              grid$n_profiles[best], grid$warnings[best]),
      class = "saqrmisc_fit_warning"))
  }
  if (verbose) message(sprintf(
    "Selected %s with %d profiles (BIC = %.1f).",
    grid$model[best], grid$n_profiles[best], grid$bic_individual[best]))

  fit$clustering <- list(
    enumeration = enumeration,
    best = best,
    input_data = data,
    complete_rows = complete_rows,
    scaling = scaling
  )
  class(fit) <- c("saqr_clustering", class(fit))
  fit
}

#' @noRd
.bad_input <- function(message) {
  stop(errorCondition(message, class = "saqrmisc_bad_input", call = NULL))
}

#' Strip the Saqrmisc class so latents methods dispatch
#' @noRd
.as_multilpa <- function(x) {
  x$clustering <- NULL
  class(x) <- setdiff(class(x), "saqr_clustering")
  x
}

#' Print a Clustering Result
#'
#' @param x A `saqr_clustering` object from [clustering()].
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.saqr_clustering <- function(x, ...) {
  info <- x$clustering
  grid <- as.data.frame(info$enumeration)
  row <- grid[info$best, , drop = FALSE]
  cat(sprintf("Latent profile clustering: %s, %d profiles\n",
              row$model, row$n_profiles))
  cat(sprintf("  n = %d (%d removed), %d variables, scaling = %s\n",
              sum(info$complete_rows), sum(!info$complete_rows),
              length(x$vars), info$scaling))
  cat(sprintf("  BIC = %.1f, log-likelihood = %.1f, relative entropy = %.3f\n",
              row$bic_individual, row$log_likelihood, row$profile_entropy))
  cat(sprintf("  Selected by BIC from %d candidate(s), %d converged\n",
              nrow(grid), sum(grid$converged)))
  invisible(x)
}

#' Compare the Candidate Models of a Clustering Result
#'
#' @param object A `saqr_clustering` object from [clustering()].
#' @param ... Ignored.
#' @return A data.frame with one row per candidate (profiles x covariance
#'   model): `n_profiles`, `model`, `log_likelihood`, `n_parameters`, `aic`,
#'   `bic`, `icl`, `entropy` (relative), `converged`, `boundary`, `delta_bic`
#'   (distance from the selected model) and `selected`.
#' @export
summary.saqr_clustering <- function(object, ...) {
  grid <- as.data.frame(object$clustering$enumeration)
  best <- object$clustering$best
  data.frame(
    n_profiles = grid$n_profiles,
    model = grid$model,
    log_likelihood = grid$log_likelihood,
    n_parameters = grid$n_parameters,
    aic = grid$aic,
    bic = grid$bic_individual,
    icl = grid$icl_individual,
    entropy = grid$profile_entropy,
    converged = grid$converged,
    boundary = grid$boundary,
    delta_bic = grid$bic_individual - grid$bic_individual[best],
    selected = seq_len(nrow(grid)) == best,
    stringsAsFactors = FALSE
  )
}

#' Input Data with Profile Assignments
#'
#' @param object A `saqr_clustering` object from [clustering()].
#' @param ... Ignored.
#' @return The data frame passed to [clustering()], one row per input row,
#'   with `profile` (modal assignment), `uncertainty` and one
#'   `posterior_profile_<k>` column per profile added. Rows dropped for
#'   missing values carry `NA` in the added columns.
#' @export
fitted.saqr_clustering <- function(object, ...) {
  info <- object$clustering
  assignments <- latents::get_results(.as_multilpa(object), what = "assignments")
  added <- setdiff(names(assignments), object$vars)
  padded <- lapply(assignments[added], \(column) {
    out <- rep(column[NA_integer_], length(info$complete_rows))
    out[info$complete_rows] <- column
    out
  })
  out <- info$input_data
  out[added] <- padded
  out
}
