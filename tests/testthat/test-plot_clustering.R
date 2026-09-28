fit_single <- local({
  clustering(
    data = iris,
    vars = names(iris)[1:4],
    n_profiles = 3,
    models = "EEE",
    seed = 1,
    n_starts = 3,
    verbose = FALSE
  )
})

fit_enum <- local({
  clustering(
    data = iris,
    vars = names(iris)[1:4],
    n_profiles = 2:3,
    models = c("EII", "EEE"),
    seed = 1,
    n_starts = 3,
    verbose = FALSE
  )
})

test_that("catalogue is tidy and groups are complete", {
  types <- clustering_plot_types()
  expect_s3_class(types, "data.frame")
  expect_named(types, c("type", "group", "description"))
  expect_false(anyDuplicated(types$type) > 0L)
  expect_setequal(
    unique(types$group),
    c("clusters", "diagnostics", "selection")
  )
})

test_that("plot_clustering draws cluster plots by default", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  result <- plot_clustering(fit_single)
  expect_s3_class(result, "multilpa")
})

test_that("individual latents plot types work", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  for (what in c("profiles", "bars", "heatmap", "sizes", "avepp")) {
    expect_silent(plot_clustering(fit_single, type = what))
  }
})

test_that("diagnostics group draws multiple plots", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_silent(plot_clustering(fit_single, type = "diagnostics"))
})

test_that("enumeration plot works when enumeration exists", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_silent(plot_clustering(fit_enum, type = "selection"))
})

test_that("enumeration plot message when no enumeration", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_message(
    plot_clustering(fit_single, type = "enumeration"),
    "No enumeration"
  )
})

test_that("plot.saqr_clustering dispatches to plot_clustering", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  result <- plot(fit_single)
  expect_s3_class(result, "multilpa")
})

test_that("bad inputs raise classed errors", {
  expect_error(plot_clustering(list()), class = "saqrmisc_bad_input")
  expect_error(plot_clustering(fit_single, type = "nope"),
               class = "saqrmisc_bad_type")
  expect_error(plot_clustering(fit_single, scale = "log"))
})

test_that("all draws everything without error", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # Single model: enumeration type produces a message, not an error
  expect_message(plot_clustering(fit_single, type = "all"), "No enumeration")

  # Enum model: all types work silently
  expect_silent(plot_clustering(fit_enum, type = "all"))
})

test_that("relative entropy helper is correct", {
  crisp <- diag(3)
  uniform <- matrix(1 / 3, nrow = 4, ncol = 3)
  expect_equal(Saqrmisc:::.lpa_relative_entropy(crisp), 1)
  expect_equal(Saqrmisc:::.lpa_relative_entropy(uniform), 0, tolerance = 1e-12)
})
