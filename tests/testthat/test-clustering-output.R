test_that("clustering stores criteria, input data, and dispatches plot()", {
  fit <- clustering(
    data = iris,
    vars = names(iris)[1:4],
    n_clusters = 2,
    models = "EII",
    verbose = FALSE
  )

  expect_s3_class(fit, "moe_analysis")
  expect_identical(fit$data$full_input_data, iris)
  expect_s3_class(fit$comparison, "data.frame")
  expect_true(all(c("model", "n_clusters", "loglik", "aic", "bic", "icl") %in%
                    names(fit$comparison)))
  expect_length(fit$comparison$loglik, 1)

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  plots <- expect_invisible(plot(fit, type = "all"))
  expect_s3_class(plots, "clustering_plots")
  expect_identical(names(plots), clustering_plot_types()$type)

  best_plots <- plot_best_model(fit)
  expect_named(
    best_plots,
    c("profile", "heatmap", "barchart", "sizes")
  )
  expect_s3_class(plot_model(fit, "EII", type = "profile"), "ggplot")
})

test_that("cluster is a complete alias for clustering", {
  args <- list(
    data = iris,
    vars = names(iris)[1:4],
    n_clusters = 2,
    models = "EII",
    verbose = FALSE
  )

  original <- do.call(clustering, args)
  aliased <- do.call(cluster, args)

  expect_s3_class(aliased, "moe_analysis")
  expect_equal(aliased$comparison, original$comparison)
  expect_equal(
    aliased$models$EII$cluster_assignments,
    original$models$EII$cluster_assignments
  )
})

test_that("cluster-prefixed aliases preserve original signatures", {
  aliases <- list(
    cluster_fit = cluster_fit,
    cluster_models = cluster_models,
    cluster_best = cluster_best,
    cluster_compare = cluster_compare,
    cluster_compare_table = cluster_compare_table,
    cluster_assignments = cluster_assignments,
    cluster_view = cluster_view,
    cluster_plot_model = cluster_plot_model,
    cluster_plot_best = cluster_plot_best,
    cluster_stability = cluster_stability,
    cluster_report = cluster_report
  )
  originals <- list(
    cluster_fit = clustering,
    cluster_models = list_models,
    cluster_best = get_best_model,
    cluster_compare = compare_models,
    cluster_compare_table = model_comparison_table,
    cluster_assignments = get_cluster_assignments,
    cluster_view = view_results,
    cluster_plot_model = plot_model,
    cluster_plot_best = plot_best_model,
    cluster_stability = assess_cluster_stability,
    cluster_report = generate_cluster_report
  )

  for (name in names(aliases)) {
    expect_identical(formals(aliases[[name]]), formals(originals[[name]]))
    expect_identical(body(aliases[[name]]), body(originals[[name]]))
  }
})

test_that("comparison produces separate BIC, AIC, and ICL plots", {
  fit <- clustering(
    data = iris,
    vars = names(iris)[1:4],
    n_clusters = 2:3,
    models = "EII",
    verbose = FALSE
  )

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  comparison_plots <- plot(fit, type = "selection")
  expect_named(comparison_plots, c("bic", "aic", "icl"))
  expect_true(all(vapply(comparison_plots, inherits, logical(1), "ggplot")))

  bic_plot <- plot(fit, type = "bic")
  expect_s3_class(bic_plot, "ggplot")
  expect_identical(bic_plot$labels$y, "BIC")
  expect_identical(bic_plot$labels$x, "Number of clusters")
  expect_identical(
    get_best_model(fit, "bic"),
    fit$comparison$model[which.max(fit$comparison$bic)]
  )
  expect_s3_class(get_best_model(fit, "bic", what = "fit"), "MoEClust")
  expect_identical(
    get_best_model(fit, "bic", what = "result"),
    fit$models[[get_best_model(fit, "bic")]]
  )
})

test_that("cluster extraction preserves rows omitted for missing values", {
  data_with_na <- iris
  data_with_na$Sepal.Length[1] <- NA_real_

  fit <- clustering(
    data = data_with_na,
    vars = names(iris)[1:4],
    n_clusters = 2,
    models = "EII",
    verbose = FALSE
  )

  extracted <- get_cluster_assignments(
    fit,
    include_probabilities = TRUE
  )

  expect_equal(nrow(extracted), nrow(data_with_na))
  expect_true(is.na(extracted$cluster[1]))
  expect_true(is.na(extracted$cluster_certainty[1]))
  expect_false(anyNA(extracted$cluster[-1]))

  fitted_data <- fitted(fit, probabilities = TRUE)
  expect_s3_class(fitted_data, "tbl_df")
  expect_equal(fitted_data, tibble::as_tibble(extracted))
})

test_that("summary returns a tidy ranked model table", {
  fit <- clustering(
    data = iris,
    vars = names(iris)[1:4],
    n_clusters = 2:3,
    models = "EII",
    verbose = FALSE
  )

  model_summary <- summary(fit)

  expect_s3_class(model_summary, "tbl_df")
  expect_named(
    model_summary,
    c("model", "best", "n_clusters", "loglik", "aic", "bic", "icl")
  )
  expect_equal(sum(model_summary$best), 1)
  expect_true(all(diff(model_summary$bic) <= 0))
})
