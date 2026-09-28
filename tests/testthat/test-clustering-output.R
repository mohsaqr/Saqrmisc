iris_vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

fit_one <- clustering(iris, iris_vars, n_profiles = 3, models = "EEE",
                      n_starts = 3, seed = 1, verbose = FALSE)
fit_grid <- clustering(iris, iris_vars, n_profiles = 2:3,
                       models = c("EII", "EEE"), n_starts = 3, seed = 1,
                       verbose = FALSE)

test_that("the result is a latents fit with the Saqrmisc class on top", {
  expect_s3_class(fit_one, c("saqr_clustering", "multilpa"), exact = TRUE)
  expect_identical(fit_one$n_profiles, 3L)
  expect_identical(fit_one$covariance_structure, "EEE")
  expect_true(fit_one$converged)
})

test_that("a single candidate equals latents::lpa() on the scaled data", {
  scaled <- as.data.frame(scale(iris[iris_vars]))
  direct <- latents::lpa(scaled, iris_vars, n_profiles = 3, model = "EEE",
                         n_starts = 3, seed = 1)
  expect_equal(fit_one$log_likelihood, direct$log_likelihood)
  expect_identical(fit_one$subject_profiles, direct$subject_profiles)
})

test_that("n_clusters is an alias for n_profiles", {
  alias <- clustering(iris, iris_vars, n_clusters = 3, models = "EEE",
                      n_starts = 3, seed = 1, verbose = FALSE)
  expect_equal(alias$log_likelihood, fit_one$log_likelihood)
})

test_that("summary has one row per candidate and selects the minimum BIC", {
  grid <- summary(fit_grid)
  expect_s3_class(grid, "data.frame")
  expect_equal(nrow(grid), 4L)
  expect_named(grid, c("n_profiles", "model", "log_likelihood",
                       "n_parameters", "aic", "bic", "icl", "entropy",
                       "converged", "boundary", "delta_bic", "selected"))
  expect_equal(sum(grid$selected), 1L)
  selected_bic <- grid$bic[grid$selected]
  expect_equal(selected_bic, min(grid$bic[grid$converged]))
  expect_true(all(grid$delta_bic >= 0))
  expect_equal(fit_grid$bic_individual, selected_bic)
})

test_that("a single fit's summary is one complete row", {
  row <- summary(fit_one)
  expect_equal(nrow(row), 1L)
  expect_true(row$selected)
  expect_equal(row$delta_bic, 0)
  expect_false(anyNA(row[c("aic", "bic", "icl", "entropy")]))
})

test_that("latents verbs work on the result directly", {
  means <- as.data.frame(fit_one)
  expect_equal(nrow(means), 3L * 4L)
  expect_true(all(c("profile", "indicator", "mean") %in% names(means)))
  expect_s3_class(latents::diagnostics(fit_one), "multilpa_diagnostics")
})

test_that("fitted() returns every input row with its assignment", {
  out <- fitted(fit_one)
  expect_equal(nrow(out), nrow(iris))
  expect_identical(out[names(iris)], iris)
  expect_identical(out$profile, fit_one$subject_profiles)
  posteriors <- out[paste0("posterior_profile_", 1:3)]
  expect_equal(unname(rowSums(posteriors)), rep(1, nrow(iris)))
})

test_that("rows dropped for missing values come back as NA", {
  holed <- iris
  holed$Sepal.Width[c(2, 40)] <- NA_real_
  fit <- clustering(holed, iris_vars, n_profiles = 2, models = "EII",
                    n_starts = 3, seed = 1, verbose = FALSE)
  out <- fitted(fit)
  expect_equal(nrow(out), nrow(iris))
  expect_equal(which(is.na(out$profile)), c(2L, 40L))
  expect_type(out$profile, "integer")
  expect_equal(sum(summary(fit)$selected), 1L)
})

test_that("each scaling transforms the indicators as documented", {
  scaled_data <- \(method) {
    fit <- clustering(iris, iris_vars, n_profiles = 2, models = "EII",
                      scaling = method, n_starts = 2, seed = 1,
                      verbose = FALSE)
    latents::get_results(fit, what = "data")
  }
  standardized <- scaled_data("standardize")
  expect_equal(unname(colMeans(standardized)), rep(0, 4))
  expect_equal(unname(vapply(standardized, sd, numeric(1L))), rep(1, 4))
  minmax <- scaled_data("minmax")
  expect_equal(unname(vapply(minmax, range, numeric(2L))),
               matrix(c(0, 1), 2L, 4L))
  expect_equal(as.list(scaled_data("none")), as.list(iris[iris_vars]))
})

test_that("bad input raises classed errors", {
  expect_error(clustering(iris, iris_vars, n_profiles = 2, models = "XYZ"),
               class = "saqrmisc_bad_input")
  expect_error(clustering(iris, c(iris_vars, "Species"), n_profiles = 2),
               class = "saqrmisc_bad_input")
  constant <- transform(iris, flat = 1)
  expect_error(clustering(constant, c(iris_vars, "flat"), n_profiles = 2),
               class = "saqrmisc_bad_input")
  holed <- iris
  holed$Sepal.Width[1] <- NA_real_
  expect_error(clustering(holed, iris_vars, n_profiles = 2,
                          na_action = "fail"),
               class = "saqrmisc_bad_input")
  expect_error(clustering(iris[1:5, ], iris_vars, n_profiles = 2),
               class = "saqrmisc_bad_input")
  expect_error(clustering(iris, "nope", n_profiles = 2))
  expect_error(clustering(iris, iris_vars, n_profiles = 1))
  expect_error(clustering(iris, iris_vars, n_profiles = 2, scaling = "nah"))
})

test_that("progress is reported as messages, not printed output", {
  expect_message(
    clustering(iris, iris_vars, n_profiles = 2, models = "EII",
               n_starts = 2, seed = 1),
    "Selected EII with 2 profiles")
  expect_silent(
    clustering(iris, iris_vars, n_profiles = 2, models = "EII",
               n_starts = 2, seed = 1, verbose = FALSE))
})

test_that("print is a short header", {
  printed <- capture.output(print(fit_grid))
  expect_length(printed, 4L)
  expect_match(printed[1], "Latent profile clustering: EEE, 3 profiles")
  expect_match(printed[4], "from 4 candidate\\(s\\), 4 converged")
})
