iris_vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

fit_one <- clustering(iris, iris_vars, n_profiles = 3, models = "EEE",
                      n_starts = 3, seed = 1, verbose = FALSE)
fit_grid <- clustering(iris, iris_vars, n_profiles = 2:3,
                       models = c("EII", "EEE"), n_starts = 3, seed = 1,
                       verbose = FALSE)

test_that("the catalogue is tidy and every type has a latents description", {
  types <- clustering_plot_types()
  expect_named(types, c("type", "group", "description"))
  expect_false(anyDuplicated(types$type) > 0L)
  expect_false(anyNA(types$description))
  expect_setequal(types$group, c("clusters", "diagnostics", "selection"))
})

test_that("every plot type draws without a condition", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  lapply(clustering_plot_types()$type, \(view) {
    expect_silent(plot_clustering(fit_grid, type = view))
  })
})

test_that("groups and 'all' draw and return the result invisibly", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_invisible(plot_clustering(fit_grid))
  expect_identical(plot_clustering(fit_grid, type = "diagnostics"), fit_grid)
  expect_silent(plot_clustering(fit_grid, type = "all"))
  expect_silent(plot_clustering(fit_one, type = "profiles",
                                scale = "standardized"))
})

test_that("plot() dispatches to plot_clustering()", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_identical(plot(fit_one, type = "sizes"), fit_one)
})

test_that("a single candidate has no enumeration to draw", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_message(plot_clustering(fit_one, type = "selection"),
                 "Only one candidate")
})

test_that("bad input raises classed errors", {
  expect_error(plot_clustering(list()), class = "saqrmisc_bad_input")
  expect_error(plot_clustering(fit_one, type = "nope"),
               class = "saqrmisc_bad_type")
})
