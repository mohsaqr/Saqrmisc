test_that("clustering returns saqr_clustering with correct structure", {
  fit <- clustering(
    data = iris,
    vars = names(iris)[1:4],
    n_profiles = 2,
    models = "EII",
    seed = 1,
    verbose = FALSE
  )

  expect_s3_class(fit, "saqr_clustering")
  expect_s3_class(fit$fit, "multilpa")
  expect_null(fit$enumeration)
  expect_identical(fit$data$full_input_data, iris)
  expect_equal(fit$parameters$sample_size, 150L)
  expect_equal(fit$parameters$scaling_method, "standardize")
  expect_identical(fit$parameters$cluster_vars, names(iris)[1:4])
  expect_true(fit$fit$converged)
})

test_that("n_clusters works as alias for n_profiles", {
  fit <- clustering(
    data = iris,
    vars = names(iris)[1:4],
    n_clusters = 3,
    models = "EEE",
    seed = 1,
    verbose = FALSE
  )
  expect_s3_class(fit, "saqr_clustering")
  expect_equal(fit$fit$n_profiles, 3L)
})

test_that("cluster is an alias for clustering", {
  fit1 <- clustering(iris, names(iris)[1:4], n_profiles = 2,
                     models = "EII", seed = 1, verbose = FALSE)
  fit2 <- cluster(iris, names(iris)[1:4], n_profiles = 2,
                  models = "EII", seed = 1, verbose = FALSE)

  expect_equal(fit1$fit$log_likelihood, fit2$fit$log_likelihood)
  expect_equal(fit1$fit$subject_profiles, fit2$fit$subject_profiles)
})

test_that("enumeration works with multiple n_profiles or models", {
  fit <- clustering(
    data = iris,
    vars = names(iris)[1:4],
    n_profiles = 2:3,
    models = c("EII", "EEE"),
    seed = 1,
    n_starts = 3,
    verbose = FALSE
  )

  expect_s3_class(fit, "saqr_clustering")
  expect_s3_class(fit$enumeration, "multilpa_enumeration")

  tab <- summary(fit)
  expect_s3_class(tab, "data.frame")
  expect_true("delta_bic" %in% names(tab))
  expect_true(any(tab$best))
  expect_equal(sum(tab$best), 1L)
})

test_that("as.data.frame returns profile means", {
  fit <- clustering(iris, names(iris)[1:4], n_profiles = 3,
                    models = "EEE", seed = 1, verbose = FALSE)
  df <- as.data.frame(fit)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("profile", "indicator", "mean") %in% names(df)))
  expect_equal(nrow(df), 3L * 4L)
})

test_that("fitted returns observations with assignments", {
  fit <- clustering(iris, names(iris)[1:4], n_profiles = 2,
                    models = "EII", seed = 1, verbose = FALSE)
  f <- fitted(fit)
  expect_equal(nrow(f), 150L)
  expect_true("profile" %in% names(f))
  expect_true("uncertainty" %in% names(f))
  expect_false(anyNA(f$profile))
})

test_that("fitted handles missing values by padding NA", {
  d <- iris
  d$Sepal.Length[1] <- NA_real_

  fit <- clustering(d, names(iris)[1:4], n_profiles = 2,
                    models = "EII", seed = 1, verbose = FALSE)
  f <- fitted(fit)
  expect_equal(nrow(f), 150L)
  expect_true(is.na(f$profile[1]))
  expect_false(anyNA(f$profile[-1]))
})

test_that("get_cluster_assignments works and renames column", {
  fit <- clustering(iris, names(iris)[1:4], n_profiles = 2,
                    models = "EII", seed = 1, verbose = FALSE)
  a <- get_cluster_assignments(fit, cluster_col_name = "grp")
  expect_true("grp" %in% names(a))
  expect_false("profile" %in% names(a))
  expect_false("uncertainty" %in% names(a))

  a2 <- get_cluster_assignments(fit, include_probabilities = TRUE)
  expect_true("uncertainty" %in% names(a2))
})

test_that("compare_models returns sorted enumeration table", {
  fit <- clustering(iris, names(iris)[1:4], n_profiles = 2:3,
                    models = c("EII", "EEE"), seed = 1, n_starts = 3,
                    verbose = FALSE)
  cmp <- compare_models(fit)
  expect_s3_class(cmp, "data.frame")
  expect_true(all(diff(cmp$bic) >= 0))
})

test_that("compare_models returns NULL for single model", {
  fit <- clustering(iris, names(iris)[1:4], n_profiles = 2,
                    models = "EII", seed = 1, verbose = FALSE)
  expect_message(cmp <- compare_models(fit), "Only one model")
  expect_null(cmp)
})

test_that("get_best_model returns structure name or fit", {
  fit <- clustering(iris, names(iris)[1:4], n_profiles = 2,
                    models = "EII", seed = 1, verbose = FALSE)
  expect_equal(get_best_model(fit), "EII")
  expect_s3_class(get_best_model(fit, what = "fit"), "multilpa")
})

test_that("cluster_diagnostics returns a diagnostics object", {
  fit <- clustering(iris, names(iris)[1:4], n_profiles = 3,
                    models = "EEE", seed = 1, verbose = FALSE)
  diag <- cluster_diagnostics(fit)
  expect_s3_class(diag, "multilpa_diagnostics")
})

test_that("summary for single-model fit is a one-row data.frame", {
  fit <- clustering(iris, names(iris)[1:4], n_profiles = 3,
                    models = "EEE", seed = 1, verbose = FALSE)
  s <- summary(fit)
  expect_s3_class(s, "data.frame")
  expect_equal(nrow(s), 1L)
  expect_true(s$converged)
  expect_true(s$best)
})

test_that("generate_cluster_report runs without error", {
  fit <- clustering(iris, names(iris)[1:4], n_profiles = 3,
                    models = "EEE", seed = 1, verbose = FALSE)
  expect_output(generate_cluster_report(fit))
  md <- generate_cluster_report(fit, output_format = "markdown")
  expect_type(md, "character")
  expect_match(md, "Profile sizes")
})

test_that("scaling options are respected", {
  fit_mm <- clustering(iris, names(iris)[1:4], n_profiles = 2,
                       models = "EII", scaling = "minmax",
                       seed = 1, verbose = FALSE)
  expect_equal(fit_mm$parameters$scaling_method, "minmax")
  scaled <- fit_mm$data$scaled_data
  expect_true(all(vapply(scaled, max, numeric(1)) <= 1))
  expect_true(all(vapply(scaled, min, numeric(1)) >= 0))

  fit_none <- clustering(iris, names(iris)[1:4], n_profiles = 2,
                         models = "EII", scaling = "none",
                         seed = 1, verbose = FALSE)
  expect_equal(fit_none$data$scaled_data, fit_none$data$original_data)
})

test_that("bad inputs raise errors", {
  expect_error(clustering(iris, "nope", n_profiles = 2))
  expect_error(clustering(iris, names(iris)[1:4], n_profiles = 1))
  expect_error(clustering(iris, names(iris)[1:4], n_profiles = 2, models = "XYZ"),
               class = "saqrmisc_bad_input")
  expect_error(clustering(iris, names(iris)[1:4], n_profiles = 2, scaling = "nah"))
})
