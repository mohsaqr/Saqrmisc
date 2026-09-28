# Saqrmisc Package: Plotting for clustering() results
#
# Delegates to the latents package's plot.multilpa() for the actual figures.
# plot_clustering() is the public verb; plot.saqr_clustering() is the S3 method.

#' @importFrom ggplot2 ggplot aes labs theme_minimal theme element_text
#'   geom_line geom_point scale_x_continuous
NULL

# =============================================================================
# CATALOGUE
# =============================================================================

#' @noRd
.clustering_plot_catalogue <- function() {
  data.frame(
    type = c(
      "profiles", "bars", "heatmap", "raincloud", "sizes",
      "entropy", "posteriors", "avepp",
      "enumeration"
    ),
    group = c(
      rep("clusters", 5L),
      rep("diagnostics", 3L),
      "selection"
    ),
    description = c(
      "Profile means across indicators, one line per profile",
      "Profile means as grouped bars with 95% intervals",
      "Profile means as a diverging heatmap (SDs from grand mean)",
      "Density + box + jitter of each indicator by profile",
      "Number and percentage of observations per profile",
      "Posterior-probability histogram (classification entropy)",
      "Per-observation posterior by assigned profile",
      "Average posterior probability matrix (assigned x posterior)",
      "BIC / AIC across enumerated candidates"
    ),
    stringsAsFactors = FALSE
  )
}

#' List the Plot Types of plot_clustering()
#' @md
#'
#' @description
#' Returns the catalogue of plots that [plot_clustering()] can draw.
#' Any `type` value, group name, or `"all"` can be passed to
#' [plot_clustering()].
#'
#' @return A data.frame with columns `type`, `group`, and `description`.
#'
#' @examples
#' clustering_plot_types()
#'
#' @export
clustering_plot_types <- function() {
  .clustering_plot_catalogue()
}

# =============================================================================
# PUBLIC VERB
# =============================================================================

#' Plot Clustering Results
#'
#' @md
#' @description
#' Draws plots of a [clustering()] result by delegating to the
#' \pkg{latents} package's `plot.multilpa()` method. Plots are organised
#' into three groups:
#'
#' * `"clusters"` — profile shape: `"profiles"`, `"bars"`, `"heatmap"`,
#'   `"raincloud"`, `"sizes"`.
#' * `"diagnostics"` — classification quality: `"entropy"`,
#'   `"posteriors"`, `"avepp"`.
#' * `"selection"` — model comparison: `"enumeration"` (requires
#'   enumeration via multiple `n_profiles` or `models`).
#' * `"all"` — every available plot.
#'
#' Use [clustering_plot_types()] for the full catalogue.
#'
#' @param results A `saqr_clustering` object from [clustering()].
#' @param type Character vector of plot types and/or groups. Defaults to
#'   `"clusters"`.
#' @param scale `"raw"` (default, shows the data as fitted) or
#'   `"standardized"`.
#' @param ... Further arguments passed to `plot.multilpa()`.
#'
#' @return Invisibly, the fitted `multilpa` object (or enumeration).
#'
#' @seealso [clustering()], [clustering_plot_types()], [latents::plot_views()]
#'
#' @examples
#' \donttest{
#' fit <- clustering(iris,
#'   vars = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'   n_profiles = 3, models = "EEE", seed = 1)
#' plot_clustering(fit)
#' plot_clustering(fit, type = "diagnostics")
#' plot_clustering(fit, type = "heatmap")
#' }
#'
#' @export
plot_clustering <- function(results,
                            type = "clusters",
                            scale = c("raw", "standardized"),
                            ...) {
  if (!inherits(results, "saqr_clustering")) {
    stop(errorCondition(
      "`results` must be a saqr_clustering object from clustering()",
      class = "saqrmisc_bad_input", call = NULL))
  }
  scale <- match.arg(scale)

  catalogue <- .clustering_plot_catalogue()
  valid_types <- c(catalogue$type, unique(catalogue$group), "all")
  unknown <- setdiff(type, valid_types)
  if (length(unknown) > 0L) {
    stop(errorCondition(
      sprintf("Unknown `type`: %s. Valid values: %s",
              paste(unknown, collapse = ", "),
              paste(valid_types, collapse = ", ")),
      class = "saqrmisc_bad_type", call = NULL))
  }

  # expand groups
  selected <- catalogue$type %in% type |
    catalogue$group %in% type |
    "all" %in% type
  chosen <- catalogue$type[selected]

  fit <- results$fit

  # cluster + diagnostic plots via latents
  latents_types <- intersect(
    chosen,
    c("profiles", "bars", "heatmap", "raincloud", "sizes",
      "entropy", "posteriors", "avepp")
  )
  vapply(latents_types, \(what) {
    plot(fit, what = what, scale = scale, ...)
    ""
  }, character(1L))

  # enumeration plot
  if ("enumeration" %in% chosen) {
    if (is.null(results$enumeration)) {
      message("No enumeration to plot (only one model was fitted).")
    } else {
      plot(results$enumeration)
    }
  }

  invisible(fit)
}

#' Plot method for saqr_clustering objects
#'
#' @param x A `saqr_clustering` object.
#' @param type Plot type(s); see [plot_clustering()].
#' @param scale `"raw"` or `"standardized"`.
#' @param ... Further arguments passed to `plot.multilpa()`.
#'
#' @return Invisibly, the fitted `multilpa` object.
#' @export
plot.saqr_clustering <- function(x,
                                 type = "clusters",
                                 scale = c("raw", "standardized"),
                                 ...) {
  plot_clustering(x, type = type, scale = scale, ...)
}
