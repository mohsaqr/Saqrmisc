# Saqrmisc Package: plots of a clustering() result
#
# Every figure is a ggplot object built by latents: plot.multilpa() for the
# selected fit and plot.multilpa_enumeration() for the candidate grid. This
# file only maps the Saqrmisc groups onto latents' views.

#' @noRd
.clustering_plot_groups <- c(
  profiles = "clusters", bars = "clusters", heatmap = "clusters",
  raincloud = "clusters", parallel = "clusters", pairs = "clusters",
  sizes = "clusters",
  entropy = "diagnostics", posteriors = "diagnostics", avepp = "diagnostics",
  enumeration = "selection", tree = "selection"
)

#' List the Plot Types of plot_clustering()
#'
#' @md
#' @description
#' The figures [plot_clustering()] can draw. Any `type`, any `group`, or
#' `"all"` can be passed as its `type`.
#'
#' @return A data.frame with one row per plot type and columns `type`,
#'   `group` (`"clusters"`, `"diagnostics"` or `"selection"`) and
#'   `description`.
#'
#' @examples
#' clustering_plot_types()
#'
#' @export
clustering_plot_types <- function() {
  views <- latents::plot_views()
  types <- names(.clustering_plot_groups)
  data.frame(
    type = types,
    group = unname(.clustering_plot_groups),
    description = views$description[match(types, views$type)],
    stringsAsFactors = FALSE
  )
}

#' Plot a Clustering Result
#'
#' @md
#' @description
#' Builds figures of a [clustering()] result with \pkg{latents}, as ggplot
#' objects. `type` takes any mix of plot types and groups:
#'
#' * `"clusters"`: `"profiles"`, `"bars"`, `"heatmap"`, `"raincloud"`,
#'   `"parallel"`, `"pairs"`, `"sizes"`.
#' * `"diagnostics"`: `"entropy"`, `"posteriors"`, `"avepp"`.
#' * `"selection"`: `"enumeration"` (information criteria across the
#'   candidates) and `"tree"` (how profiles split as more are added); both
#'   need more than one candidate.
#' * `"all"`: every type above.
#'
#' @param results A `saqr_clustering` object from [clustering()].
#' @param type Plot types and/or groups; see [clustering_plot_types()].
#' @param ... Passed to the \pkg{latents} plot method of the selected fit,
#'   e.g. `scale = "standardized"` or `main`.
#'
#' @return One type: a ggplot object. Several: a `saqr_plots` list of ggplot
#'   objects named by type, which draws every one when printed. Print, save
#'   with `ggplot2::ggsave()`, or restyle with `+ ggplot2::theme()`.
#'
#' @section Errors:
#' Raises `saqrmisc_bad_input` when `results` is not a [clustering()] result
#' and `saqrmisc_bad_type` for an unknown `type`. A selection view that needs
#' more candidates than were fitted is skipped with a message.
#'
#' @seealso [clustering()], [clustering_plot_types()]
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
#' fit <- clustering(iris, vars, n_profiles = 2:4, models = "EEE")
#' plot_clustering(fit, type = "profiles")
#' plot_clustering(fit, type = "diagnostics")
#' plot_clustering(fit, type = c("pairs", "tree"))
#' }
#'
#' @export
plot_clustering <- function(results, type = "clusters", ...) {
  if (!inherits(results, "saqr_clustering")) {
    stop(errorCondition(
      "`results` must be a clustering() result.",
      class = "saqrmisc_bad_input", call = NULL))
  }
  groups <- .clustering_plot_groups
  valid <- c(names(groups), unique(groups), "all")
  unknown <- setdiff(type, valid)
  if (length(unknown) > 0L) {
    stop(errorCondition(
      sprintf("Unknown `type`: %s. Use one of: %s.",
              paste(unknown, collapse = ", "), paste(valid, collapse = ", ")),
      class = "saqrmisc_bad_type", call = NULL))
  }
  chosen <- names(groups)[names(groups) %in% type | groups %in% type |
                            "all" %in% type]

  fit <- .as_multilpa(results)
  enumeration <- results$clustering$enumeration
  several_candidates <- nrow(as.data.frame(enumeration)) > 1L
  plots <- lapply(chosen, \(view) {
    if (!view %in% c("enumeration", "tree")) return(plot(fit, what = view, ...))
    if (!several_candidates) {
      message(sprintf("Only one candidate was fitted; no %s to plot.", view))
      return(NULL)
    }
    # A tree needs two numbers of profiles for one model; a grid of one
    # count across models has none, which latents refuses by class.
    tryCatch(plot(enumeration, what = view),
             latents_nothing_to_plot = \(condition) {
               message(conditionMessage(condition))
               NULL
             })
  })
  names(plots) <- chosen
  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 1L) return(plots[[1L]])
  structure(plots, class = "saqr_plots")
}

#' Print Several Clustering Plots
#'
#' @param x A `saqr_plots` list from [plot_clustering()].
#' @param ... Ignored.
#' @return `x`, invisibly. Called for the side effect of drawing each plot.
#' @export
print.saqr_plots <- function(x, ...) {
  invisible(lapply(x, print))
  invisible(x)
}

#' Plot Method for Clustering Results
#'
#' @param x A `saqr_clustering` object from [clustering()].
#' @param type Plot types and/or groups; see [plot_clustering()].
#' @param ... Passed to [plot_clustering()].
#' @return As [plot_clustering()]: a ggplot object, or a `saqr_plots` list.
#' @export
plot.saqr_clustering <- function(x, type = "clusters", ...) {
  plot_clustering(x, type = type, ...)
}
