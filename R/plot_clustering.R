# Saqrmisc Package: Systematic plotting for clustering() results
#
# plot_clustering() builds every figure on demand from the stored data and
# posterior probabilities, so all plots share one palette and theme. Plots are
# organised into three groups that answer three different questions:
#   clusters     - what do the clusters look like?
#   diagnostics  - how cleanly does the model separate them?
#   selection    - which model / number of clusters fits best?

# =============================================================================
# CATALOGUE
# =============================================================================

#' @noRd
.clustering_plot_catalogue <- function() {
  data.frame(
    type = c(
      "profile", "heatmap", "distribution", "sizes",
      "certainty", "avepp", "projection",
      "bic", "aic", "icl"
    ),
    group = c(
      rep("clusters", 4L),
      rep("diagnostics", 3L),
      rep("selection", 3L)
    ),
    description = c(
      "Mean of each variable per cluster, one line per cluster",
      "Cluster means, coloured by standardised distance from the overall mean",
      "Within-cluster spread of each variable (boxplots)",
      "Number and percentage of observations per cluster",
      "Posterior probability of the assigned cluster, with relative entropy",
      "Average posterior probability: assigned cluster by posterior cluster",
      "Observations on the first two principal components, by cluster",
      "BIC across models and numbers of clusters (higher is better)",
      "AIC across models and numbers of clusters (higher is better)",
      "ICL across models and numbers of clusters (higher is better)"
    ),
    stringsAsFactors = FALSE
  )
}

#' List the Plot Types of plot_clustering()
#' @md
#'
#' @description
#' Returns the catalogue of plots that [plot_clustering()] can draw, with the
#' group each belongs to. Any `type` or `group` value, or `"all"`, can be passed
#' to the `type` argument of [plot_clustering()].
#'
#' @return A data.frame with one row per plot type and columns `type`,
#'   `group` (`"clusters"`, `"diagnostics"`, or `"selection"`), and
#'   `description`.
#'
#' @examples
#' clustering_plot_types()
#'
#' @export
clustering_plot_types <- function() {
  .clustering_plot_catalogue()
}

# =============================================================================
# STYLE HELPERS
# =============================================================================

#' @noRd
.okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
  "#D55E00", "#CC79A7", "#999999", "#000000"
)

#' Distinct point shapes, paired with colour so no distinction is colour-only
#' @noRd
.cluster_shapes <- c(16, 17, 15, 18, 3, 4, 8, 1, 2, 0, 5, 6)

#' @noRd
.clust_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "grey35"),
      legend.position = "bottom"
    )
}

#' Colour + shape scales for a discrete grouping with `n` levels
#' @noRd
.clust_discrete_scales <- function(n, name) {
  list(
    ggplot2::scale_colour_manual(name = name, values = rep_len(.okabe_ito, n)),
    ggplot2::scale_fill_manual(name = name, values = rep_len(.okabe_ito, n)),
    ggplot2::scale_shape_manual(name = name, values = rep_len(.cluster_shapes, n))
  )
}

# =============================================================================
# CONTEXT: everything a builder needs, resolved once
# =============================================================================

#' @noRd
.clust_plot_context <- function(results, model, scale) {
  model_result <- results$models[[model]]
  assignments <- model_result$cluster_assignments
  n_clusters <- model_result$model_info$n_clusters
  cluster_vars <- results$parameters$cluster_vars

  values <- if (identical(scale, "original")) {
    results$data$original_data
  } else {
    results$data$scaled_data
  }

  list(
    model = model,
    scale = scale,
    n_clusters = n_clusters,
    cluster_vars = cluster_vars,
    values = as.data.frame(values)[cluster_vars],
    fitted_values = as.data.frame(results$data$scaled_data)[cluster_vars],
    cluster = factor(assignments, levels = seq_len(n_clusters)),
    z = unname(as.matrix(model_result$model_fit$z)),
    comparison = results$comparison %||% compare_models(results, sort_by = "bic")
  )
}

#' Cluster-by-variable means in long form
#' @noRd
.clust_means_long <- function(ctx) {
  value_matrix <- as.matrix(ctx$values)
  counts <- tabulate(ctx$cluster, nbins = ctx$n_clusters)
  means <- rowsum(value_matrix, ctx$cluster, reorder = TRUE) / counts
  n_vars <- length(ctx$cluster_vars)

  grand_mean <- colMeans(value_matrix)
  grand_sd <- apply(value_matrix, 2L, stats::sd)
  # A constant variable has no spread: define its standardised distance as 0
  safe_sd <- ifelse(grand_sd > 0, grand_sd, 1)
  standardised <- sweep(sweep(means, 2L, grand_mean), 2L, safe_sd, "/")
  standardised[, !(grand_sd > 0)] <- 0

  data.frame(
    cluster = factor(rep(seq_len(ctx$n_clusters), times = n_vars),
                     levels = seq_len(ctx$n_clusters)),
    variable = factor(rep(ctx$cluster_vars, each = ctx$n_clusters),
                      levels = ctx$cluster_vars),
    mean = as.vector(means),
    standardised = as.vector(standardised)
  )
}

#' @noRd
.scale_label <- function(ctx) {
  if (identical(ctx$scale, "original")) "original scale" else "scaled data"
}

# =============================================================================
# BUILDERS: one function per plot type, each returns a ggplot
# =============================================================================

#' @noRd
.plot_clust_profile <- function(ctx) {
  means_long <- .clust_means_long(ctx)

  ggplot2::ggplot(
    means_long,
    ggplot2::aes(
      x = .data$variable, y = .data$mean, group = .data$cluster,
      colour = .data$cluster, shape = .data$cluster
    )
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 3) +
    .clust_discrete_scales(ctx$n_clusters, "Cluster") +
    ggplot2::labs(
      title = "Cluster profiles",
      subtitle = sprintf("%s, %s", ctx$model, .scale_label(ctx)),
      x = NULL, y = "Mean"
    ) +
    .clust_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' @noRd
.plot_clust_heatmap <- function(ctx) {
  means_long <- .clust_means_long(ctx)
  limit <- max(abs(means_long$standardised), 1e-8)

  ggplot2::ggplot(
    means_long,
    ggplot2::aes(x = .data$variable, y = .data$cluster, fill = .data$standardised)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$mean)), size = 3.5) +
    ggplot2::scale_fill_gradient2(
      name = "SD from\noverall mean",
      low = "#D33F6A", mid = "white", high = "#4A6FE3",
      midpoint = 0, limits = c(-limit, limit)
    ) +
    ggplot2::labs(
      title = "Cluster means",
      subtitle = sprintf("%s, %s; labels are means", ctx$model, .scale_label(ctx)),
      x = NULL, y = "Cluster"
    ) +
    .clust_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right"
    )
}

#' @noRd
.plot_clust_distribution <- function(ctx) {
  n_obs <- length(ctx$cluster)
  long <- data.frame(
    cluster = rep(ctx$cluster, times = length(ctx$cluster_vars)),
    variable = factor(rep(ctx$cluster_vars, each = n_obs), levels = ctx$cluster_vars),
    value = unlist(ctx$values, use.names = FALSE)
  )

  ggplot2::ggplot(
    long,
    ggplot2::aes(x = .data$cluster, y = .data$value, fill = .data$cluster)
  ) +
    ggplot2::geom_boxplot(alpha = 0.75, outlier.size = 0.8) +
    ggplot2::facet_wrap(ggplot2::vars(.data$variable), scales = "free_y") +
    .clust_discrete_scales(ctx$n_clusters, "Cluster") +
    ggplot2::labs(
      title = "Within-cluster distributions",
      subtitle = sprintf("%s, %s", ctx$model, .scale_label(ctx)),
      x = "Cluster", y = NULL
    ) +
    .clust_theme() +
    ggplot2::theme(legend.position = "none")
}

#' @noRd
.plot_clust_sizes <- function(ctx) {
  counts <- tabulate(ctx$cluster, nbins = ctx$n_clusters)
  sizes <- data.frame(
    cluster = factor(seq_len(ctx$n_clusters), levels = seq_len(ctx$n_clusters)),
    n = counts,
    label = sprintf("%d (%.1f%%)", counts, 100 * counts / sum(counts))
  )

  ggplot2::ggplot(
    sizes,
    ggplot2::aes(x = .data$cluster, y = .data$n, fill = .data$cluster)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), vjust = -0.4, size = 3.5) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    .clust_discrete_scales(ctx$n_clusters, "Cluster") +
    ggplot2::labs(
      title = "Cluster sizes",
      subtitle = sprintf("%s, n = %d", ctx$model, sum(counts)),
      x = "Cluster", y = "Observations"
    ) +
    .clust_theme() +
    ggplot2::theme(legend.position = "none")
}

#' Relative entropy of a posterior matrix (1 = perfectly separated)
#' @noRd
.relative_entropy <- function(z) {
  z_log_z <- ifelse(z > 0, z * log(z), 0)
  1 - (-sum(z_log_z)) / (nrow(z) * log(ncol(z)))
}

#' @noRd
.plot_clust_certainty <- function(ctx) {
  certainty <- data.frame(
    cluster = ctx$cluster,
    certainty = do.call(pmax, as.data.frame(ctx$z))
  )

  ggplot2::ggplot(
    certainty,
    ggplot2::aes(x = .data$cluster, y = .data$certainty, fill = .data$cluster)
  ) +
    ggplot2::geom_boxplot(alpha = 0.75, outlier.size = 0.8) +
    ggplot2::scale_y_continuous(limits = c(1 / ctx$n_clusters, 1)) +
    .clust_discrete_scales(ctx$n_clusters, "Cluster") +
    ggplot2::labs(
      title = "Classification certainty",
      subtitle = sprintf(
        "%s; mean certainty %.3f, relative entropy %.3f",
        ctx$model, mean(certainty$certainty), .relative_entropy(ctx$z)
      ),
      x = "Assigned cluster", y = "Posterior probability of assigned cluster"
    ) +
    .clust_theme() +
    ggplot2::theme(legend.position = "none")
}

#' @noRd
.plot_clust_avepp <- function(ctx) {
  g <- ctx$n_clusters
  counts <- tabulate(ctx$cluster, nbins = g)
  avepp <- rowsum(ctx$z, ctx$cluster, reorder = TRUE) / counts
  avepp_long <- data.frame(
    assigned = factor(rep(seq_len(g), times = g), levels = rev(seq_len(g))),
    posterior = factor(rep(seq_len(g), each = g), levels = seq_len(g)),
    probability = as.vector(avepp)
  )

  ggplot2::ggplot(
    avepp_long,
    ggplot2::aes(x = .data$posterior, y = .data$assigned, fill = .data$probability)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = sprintf("%.2f", .data$probability),
        colour = .data$probability > 0.6
      ),
      size = 3.8
    ) +
    ggplot2::scale_colour_manual(values = c(`FALSE` = "black", `TRUE` = "white"),
                                 guide = "none") +
    ggplot2::scale_fill_gradient(
      name = "Mean\nposterior", low = "white", high = "#0072B2", limits = c(0, 1)
    ) +
    ggplot2::labs(
      title = "Average posterior probability",
      subtitle = sprintf("%s; diagonal = mean certainty within each cluster", ctx$model),
      x = "Posterior cluster", y = "Assigned cluster"
    ) +
    .clust_theme() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "right")
}

#' @noRd
.plot_clust_projection <- function(ctx) {
  fitted_matrix <- as.matrix(ctx$fitted_values)

  if (ncol(fitted_matrix) < 2L) {
    one_var <- data.frame(value = fitted_matrix[, 1L], cluster = ctx$cluster)
    return(
      ggplot2::ggplot(
        one_var,
        ggplot2::aes(x = .data$value, y = .data$cluster,
                     colour = .data$cluster, shape = .data$cluster)
      ) +
        ggplot2::geom_point(alpha = 0.7, size = 2) +
        .clust_discrete_scales(ctx$n_clusters, "Cluster") +
        ggplot2::labs(
          title = "Cluster separation",
          subtitle = sprintf("%s; single variable, data as fitted", ctx$model),
          x = ctx$cluster_vars[1L], y = "Cluster"
        ) +
        .clust_theme()
    )
  }

  pca <- stats::prcomp(fitted_matrix, center = TRUE, scale. = FALSE)
  explained <- 100 * pca$sdev^2 / sum(pca$sdev^2)
  scores <- data.frame(
    pc1 = pca$x[, 1L],
    pc2 = pca$x[, 2L],
    cluster = ctx$cluster
  )

  ggplot2::ggplot(
    scores,
    ggplot2::aes(x = .data$pc1, y = .data$pc2,
                 colour = .data$cluster, shape = .data$cluster)
  ) +
    ggplot2::geom_point(alpha = 0.75, size = 2) +
    .clust_discrete_scales(ctx$n_clusters, "Cluster") +
    ggplot2::labs(
      title = "Cluster separation",
      subtitle = sprintf("%s; principal components of the data as fitted", ctx$model),
      x = sprintf("PC1 (%.1f%%)", explained[1L]),
      y = sprintf("PC2 (%.1f%%)", explained[2L])
    ) +
    .clust_theme()
}

#' @noRd
.plot_clust_criterion <- function(ctx, criterion) {
  comparison <- ctx$comparison
  comparison$covariance <- sub("_G[0-9]+$", "", comparison$model)
  comparison$covariance <- factor(
    comparison$covariance,
    levels = intersect(MOECLUST_MODELS, comparison$covariance)
  )
  comparison$value <- comparison[[criterion]]
  best <- comparison[which.max(comparison$value), , drop = FALSE]
  n_types <- nlevels(comparison$covariance)
  label <- toupper(criterion)

  ggplot2::ggplot(
    comparison,
    ggplot2::aes(
      x = .data$n_clusters, y = .data$value, group = .data$covariance,
      colour = .data$covariance, shape = .data$covariance
    )
  ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::geom_point(data = best, colour = "black", shape = 1, size = 6,
                        stroke = 1.1, show.legend = FALSE) +
    ggplot2::scale_x_continuous(breaks = sort(unique(comparison$n_clusters))) +
    .clust_discrete_scales(n_types, "Covariance model") +
    ggplot2::labs(
      title = sprintf("Model selection: %s", label),
      subtitle = sprintf("Higher is better (MoEClust convention); circled = %s",
                         best$model),
      x = "Number of clusters", y = label
    ) +
    .clust_theme()
}

#' @noRd
.clustering_plot_builders <- function() {
  list(
    profile = .plot_clust_profile,
    heatmap = .plot_clust_heatmap,
    distribution = .plot_clust_distribution,
    sizes = .plot_clust_sizes,
    certainty = .plot_clust_certainty,
    avepp = .plot_clust_avepp,
    projection = .plot_clust_projection,
    bic = \(ctx) .plot_clust_criterion(ctx, "bic"),
    aic = \(ctx) .plot_clust_criterion(ctx, "aic"),
    icl = \(ctx) .plot_clust_criterion(ctx, "icl")
  )
}

# =============================================================================
# PUBLIC VERB
# =============================================================================

#' Plot Clustering Results Systematically
#'
#' @md
#' @description
#' One verb for every figure of a [clustering()] result. Plots are organised
#' into three groups, and `type` accepts any mix of single plot names, group
#' names, or `"all"`:
#'
#' * `"clusters"` — what the clusters look like: `"profile"`, `"heatmap"`,
#'   `"distribution"`, `"sizes"`.
#' * `"diagnostics"` — how cleanly the model separates them: `"certainty"`,
#'   `"avepp"`, `"projection"`.
#' * `"selection"` — which model fits best: `"bic"`, `"aic"`, `"icl"`.
#' * `"all"` — every plot above, for **every fitted model**.
#'
#' Cluster and diagnostic plots describe one fitted model at a time, so they
#' are drawn once per model in `model`. Selection plots compare all fitted
#' models and are drawn once.
#'
#' Use [clustering_plot_types()] for the full catalogue with descriptions.
#' All plots use the Okabe-Ito palette, and clusters are distinguished by
#' shape or axis position as well as colour.
#'
#' @param results An moe_analysis object returned by [clustering()].
#' @param type Character vector of plot types and/or groups; see Description.
#'   Defaults to `"clusters"`.
#' @param model Which fitted model(s) to describe: `NULL` (default), one or
#'   more model names, or `"all"` for every fitted model. `NULL` means the best
#'   model by BIC, except with `type = "all"`, where it means every model.
#'   Selection plots ignore `model`.
#' @param scale Data scale for `"profile"`, `"heatmap"`, and `"distribution"`:
#'   `"original"` (default) or `"scaled"`. Diagnostics always use the data
#'   as fitted.
#'
#' @return A single ggplot when the request resolves to one plot. Otherwise an
#'   object of class `clustering_plots`: a named list of ggplots, ordered by
#'   model and then by catalogue. Elements are named by plot type when one
#'   model is drawn, and `"<model>/<type>"` when several are; selection plots
#'   are always named by type. Printing it draws every plot;
#'   [as.data.frame()] lists its contents with one row per plot.
#'
#'   Raises `saqrmisc_bad_input` when `results` is not an moe_analysis object
#'   or has no fitted models, `saqrmisc_bad_type` for an unknown `type`, and
#'   `saqrmisc_model_not_found` for an unknown `model`.
#'
#' @references
#' Murphy, K., & Murphy, T. B. (2020). Gaussian parsimonious clustering models
#' with covariates and a noise component. *Advances in Data Analysis and
#' Classification*, 14, 293-325.
#'
#' Nagin, D. S. (2005). *Group-Based Modeling of Development*. Harvard
#' University Press. (Average posterior probability diagnostic.)
#'
#' @examples
#' clustering_plot_types()
#'
#' \donttest{
#' fit <- clustering(
#'   iris,
#'   vars = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'   n_clusters = 2:3,
#'   models = c("EII", "EEE"),
#'   verbose = FALSE
#' )
#'
#' plot_clustering(fit)                          # cluster plots, best model
#' plot_clustering(fit, type = "diagnostics")
#' plot_clustering(fit, type = "selection")
#' plot_clustering(fit, type = "profile", model = "all")
#' plot_clustering(fit, type = c("heatmap", "certainty"), model = "EII_G3")
#'
#' everything <- plot_clustering(fit, type = "all")  # every plot, every model
#' as.data.frame(everything)
#' }
#'
#' @export
plot_clustering <- function(results,
                            type = "clusters",
                            model = NULL,
                            scale = c("original", "scaled")) {
  if (!inherits(results, "moe_analysis") || length(results$models) == 0L) {
    stop(errorCondition(
      "`results` must be an moe_analysis object from clustering() with at least one fitted model",
      class = "saqrmisc_bad_input", call = NULL
    ))
  }
  scale <- match.arg(scale)

  catalogue <- .clustering_plot_catalogue()
  valid_types <- c(catalogue$type, unique(catalogue$group), "all")
  unknown <- setdiff(type, valid_types)
  if (!is.character(type) || length(type) == 0L || length(unknown) > 0L) {
    stop(errorCondition(
      sprintf(
        "Unknown `type`: %s. Valid values: %s",
        paste(unknown, collapse = ", "), paste(valid_types, collapse = ", ")
      ),
      class = "saqrmisc_bad_type", call = NULL
    ))
  }

  models <- .resolve_plot_models(results, model, everything = "all" %in% type)

  # Expand groups and "all", then keep catalogue order so output is predictable
  selected <- catalogue$type %in% type |
    catalogue$group %in% type |
    "all" %in% type
  chosen <- catalogue[selected, , drop = FALSE]
  per_model <- chosen[chosen$group != "selection", , drop = FALSE]
  across_models <- chosen[chosen$group == "selection", , drop = FALSE]

  builders <- .clustering_plot_builders()
  multiple <- length(models) > 1L

  model_sets <- lapply(models, \(model_name) {
    ctx <- .clust_plot_context(results, model_name, scale)
    list(
      plots = lapply(per_model$type, \(plot_type) builders[[plot_type]](ctx)),
      listing = data.frame(
        name = if (multiple) paste(model_name, per_model$type, sep = "/") else per_model$type,
        plot = per_model$type,
        group = per_model$group,
        model = rep(model_name, nrow(per_model)),
        description = per_model$description,
        stringsAsFactors = FALSE
      )
    )
  })

  selection_set <- if (nrow(across_models) > 0L) {
    ctx <- .clust_plot_context(results, models[1L], scale)
    list(list(
      plots = lapply(across_models$type, \(plot_type) builders[[plot_type]](ctx)),
      listing = data.frame(
        name = across_models$type,
        plot = across_models$type,
        group = across_models$group,
        model = rep("all fitted models", nrow(across_models)),
        description = across_models$description,
        stringsAsFactors = FALSE
      )
    ))
  }

  sets <- c(model_sets, selection_set)
  plots <- do.call(c, lapply(sets, \(set) set$plots))
  listing <- do.call(rbind, lapply(sets, \(set) set$listing))
  rownames(listing) <- NULL
  names(plots) <- listing$name

  stopifnot(
    "plot names must be unique" = !anyDuplicated(listing$name),
    "listing must have one row per plot" = nrow(listing) == length(plots)
  )

  if (length(plots) == 1L) {
    return(plots[[1L]])
  }

  structure(plots, class = c("clustering_plots", "list"), catalogue = listing)
}

#' Resolve the `model` argument of plot_clustering() to model names
#' @noRd
.resolve_plot_models <- function(results, model, everything) {
  available <- names(results$models)

  if (is.null(model)) {
    return(if (everything) available else get_best_model(results, criterion = "bic"))
  }
  if (identical(model, "all")) {
    return(available)
  }

  unknown <- setdiff(model, available)
  if (!is.character(model) || length(model) == 0L || length(unknown) > 0L) {
    stop(errorCondition(
      sprintf(
        "Model '%s' not found. Available models: %s",
        paste(unknown, collapse = ", "), paste(available, collapse = ", ")
      ),
      class = "saqrmisc_model_not_found", call = NULL
    ))
  }
  unique(model)
}

# =============================================================================
# S3 METHODS FOR clustering_plots
# =============================================================================

#' Print a Set of Clustering Plots
#' @md
#'
#' @param x A `clustering_plots` object from [plot_clustering()].
#' @param ... Ignored.
#' @return `x`, invisibly. Called for its side effect of drawing every plot.
#' @export
print.clustering_plots <- function(x, ...) {
  lapply(unclass(x), print)
  invisible(x)
}

#' List the Contents of a Set of Clustering Plots
#' @md
#'
#' @param x A `clustering_plots` object from [plot_clustering()].
#' @param row.names,optional Ignored; present for S3 consistency.
#' @param ... Ignored.
#' @return A data.frame with one row per plot, in drawing order, and columns
#'   `name` (the element name), `plot` (plot type), `group`, `model`, and
#'   `description`.
#' @export
as.data.frame.clustering_plots <- function(x, row.names = NULL, optional = FALSE, ...) {
  attr(x, "catalogue")
}
