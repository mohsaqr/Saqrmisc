fit_multi <- local({
  clustering(
    data = iris,
    vars = names(iris)[1:4],
    n_clusters = 2:3,
    models = c("EII", "EEE"),
    verbose = FALSE
  )
})

test_that("catalogue is tidy and groups are complete", {
  types <- clustering_plot_types()

  expect_s3_class(types, "data.frame")
  expect_named(types, c("type", "group", "description"))
  expect_false(anyDuplicated(types$type) > 0)
  expect_setequal(unique(types$group), c("clusters", "diagnostics", "selection"))
  expect_setequal(names(Saqrmisc:::.clustering_plot_builders()), types$type)
})

test_that("a single type returns one ggplot", {
  profile <- plot_clustering(fit_multi, type = "profile")
  expect_s3_class(profile, "ggplot")
  expect_identical(profile$labels$title, "Cluster profiles")
})

test_that("groups expand to their members in catalogue order", {
  types <- clustering_plot_types()
  groups <- c("clusters", "diagnostics", "selection")

  expanded <- lapply(groups, \(g) names(plot_clustering(fit_multi, type = g)))
  expected <- lapply(groups, \(g) types$type[types$group == g])
  expect_identical(expanded, expected)
})

test_that("'all' for one model yields every plot type with a tidy listing", {
  everything <- plot_clustering(fit_multi, type = "all", model = "EEE_G3")

  expect_s3_class(everything, "clustering_plots")
  expect_identical(names(everything), clustering_plot_types()$type)
  expect_true(all(vapply(everything, inherits, logical(1), "ggplot")))

  listing <- as.data.frame(everything)
  expect_named(listing, c("name", "plot", "group", "model", "description"))
  expect_equal(nrow(listing), length(everything))
  expect_identical(listing$plot, names(everything))
})

test_that("mixed types are deduplicated and ordered by catalogue", {
  mixed <- plot_clustering(fit_multi, type = c("icl", "profile", "clusters"))
  expect_identical(names(mixed), c("profile", "heatmap", "distribution", "sizes", "icl"))
})

test_that("default model is the best by BIC and is used in subtitles", {
  best <- get_best_model(fit_multi, "bic")
  sizes <- plot_clustering(fit_multi, type = "sizes")
  expect_match(sizes$labels$subtitle, best, fixed = TRUE)

  other <- setdiff(names(fit_multi$models), best)[1]
  sizes_other <- plot_clustering(fit_multi, type = "sizes", model = other)
  expect_match(sizes_other$labels$subtitle, other, fixed = TRUE)
})

test_that("profile means match hand-computed cluster means", {
  best <- get_best_model(fit_multi, "bic")
  assignments <- fit_multi$models[[best]]$cluster_assignments
  profile <- plot_clustering(fit_multi, type = "profile")

  hand <- aggregate(iris[names(iris)[1:4]], by = list(cluster = assignments), FUN = mean)
  plotted <- reshape(
    profile$data[c("cluster", "variable", "mean")],
    idvar = "cluster", timevar = "variable", direction = "wide"
  )
  expect_equal(
    unname(as.matrix(plotted[-1])),
    unname(as.matrix(hand[-1])),
    tolerance = sqrt(.Machine$double.eps)
  )
})

test_that("scale switches the plotted values", {
  original <- plot_clustering(fit_multi, type = "profile", scale = "original")
  scaled <- plot_clustering(fit_multi, type = "profile", scale = "scaled")
  expect_false(isTRUE(all.equal(original$data$mean, scaled$data$mean)))
  # Scaled data are standardised, so size-weighted cluster means average to 0
  sizes <- tabulate(fit_multi$models[[get_best_model(fit_multi)]]$cluster_assignments)
  weighted <- tapply(scaled$data$mean * sizes[scaled$data$cluster],
                     scaled$data$variable, sum) / sum(sizes)
  expect_equal(unname(as.vector(weighted)), rep(0, 4), tolerance = 1e-8)
})

test_that("diagnostic invariants hold: avepp rows sum to 1, certainty in range", {
  avepp <- plot_clustering(fit_multi, type = "avepp")
  row_sums <- tapply(avepp$data$probability, avepp$data$assigned, sum)
  expect_equal(unname(as.vector(row_sums)), rep(1, length(row_sums)),
               tolerance = sqrt(.Machine$double.eps))

  certainty <- plot_clustering(fit_multi, type = "certainty")
  g <- nlevels(certainty$data$cluster)
  expect_true(all(certainty$data$certainty >= 1 / g - 1e-12))
  expect_true(all(certainty$data$certainty <= 1 + 1e-12))
})

test_that("relative entropy is 1 for crisp and 0 for uniform posteriors", {
  crisp <- diag(3)
  uniform <- matrix(1 / 3, nrow = 4, ncol = 3)
  expect_equal(Saqrmisc:::.relative_entropy(crisp), 1)
  expect_equal(Saqrmisc:::.relative_entropy(uniform), 0, tolerance = 1e-12)
})

test_that("selection plots circle the best model of each criterion", {
  bic <- plot_clustering(fit_multi, type = "bic")
  expect_match(bic$labels$subtitle, get_best_model(fit_multi, "bic"), fixed = TRUE)
  icl <- plot_clustering(fit_multi, type = "icl")
  expect_match(icl$labels$subtitle, get_best_model(fit_multi, "icl"), fixed = TRUE)
})

test_that("projection falls back to a strip plot for one variable", {
  ctx <- list(
    model = "EII", n_clusters = 2L, cluster_vars = "x",
    fitted_values = data.frame(x = c(-1, -1.2, 1, 1.1)),
    cluster = factor(c(1, 1, 2, 2), levels = 1:2)
  )
  strip <- Saqrmisc:::.plot_clust_projection(ctx)
  expect_s3_class(strip, "ggplot")
  expect_identical(strip$labels$x, "x")
})

test_that("every plot builds without error when rendered", {
  everything <- plot_clustering(fit_multi, type = "all")
  built <- lapply(everything, ggplot2::ggplot_build)
  expect_length(built, length(everything))
})

test_that("bad inputs raise classed errors", {
  expect_error(plot_clustering(list()), class = "saqrmisc_bad_input")
  expect_error(plot_clustering(fit_multi, type = "nope"), class = "saqrmisc_bad_type")
  expect_error(plot_clustering(fit_multi, model = "XYZ"), class = "saqrmisc_model_not_found")
  expect_error(plot_clustering(fit_multi, scale = "log"))
})

test_that("plot() draws via plot_clustering() and returns the same plots", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  plot_types <- list("profile", "diagnostics", "all", c("sizes", "bic"))
  same <- vapply(plot_types, \(tp) {
    drawn <- plot(fit_multi, type = tp, scale = "scaled")
    built <- plot_clustering(fit_multi, type = tp, scale = "scaled")
    identical(names(drawn), names(built)) &&
      identical(class(drawn), class(built))
  }, logical(1))
  expect_true(all(same))

  expect_invisible(plot(fit_multi, type = "heatmap"))
  expect_error(plot(fit_multi, type = "comparison"), class = "saqrmisc_bad_type")
  expect_error(plot(fit_multi, model = "XYZ"), class = "saqrmisc_model_not_found")
})

test_that("type = 'all' draws every plot for every model", {
  everything <- plot_clustering(fit_multi, type = "all")
  types <- clustering_plot_types()
  per_model <- types$type[types$group != "selection"]
  models <- names(fit_multi$models)

  listing <- as.data.frame(everything)
  expect_equal(length(everything), length(models) * length(per_model) + 3L)
  expect_setequal(unique(listing$model), c(models, "all fitted models"))
  expect_identical(names(everything), listing$name)
  expect_true(all(vapply(everything, inherits, logical(1), "ggplot")))

  counts <- table(listing$model[listing$group != "selection"])
  expect_true(all(counts == length(per_model)))
  expect_identical(tail(listing$plot, 3L), c("bic", "aic", "icl"))
})

test_that("model = 'all' works with any type, and names carry the model", {
  profiles <- plot_clustering(fit_multi, type = "profile", model = "all")
  models <- names(fit_multi$models)
  expect_identical(names(profiles), paste(models, "profile", sep = "/"))

  subtitles <- vapply(profiles, \(p) p$labels$subtitle, character(1))
  expect_true(all(mapply(grepl, models, subtitles, fixed = TRUE)))
})

test_that("an explicit model restricts type = 'all' to that model", {
  one <- plot_clustering(fit_multi, type = "all", model = "EII_G2")
  expect_identical(names(one), clustering_plot_types()$type)
  expect_true(all(as.data.frame(one)$model %in% c("EII_G2", "all fitted models")))

  two <- plot_clustering(fit_multi, type = "sizes", model = c("EII_G2", "EEE_G3"))
  expect_identical(names(two), c("EII_G2/sizes", "EEE_G3/sizes"))
})

test_that("selection plots are drawn once however many models are requested", {
  sel <- plot_clustering(fit_multi, type = c("profile", "selection"), model = "all")
  expect_equal(sum(as.data.frame(sel)$group == "selection"), 3L)
})

test_that("unknown model names in a vector raise a classed error", {
  expect_error(
    plot_clustering(fit_multi, model = c("EII_G2", "nope")),
    class = "saqrmisc_model_not_found"
  )
})
