skip_if_not_installed("latents")

iris_vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

fit_one <- clustering(iris, iris_vars, n_profiles = 3, models = "EEE",
                      n_starts = 3, seed = 1, verbose = FALSE)
fit_grid <- clustering(iris, iris_vars, n_profiles = 2:3,
                       models = c("EII", "EEE"), n_starts = 3, seed = 1,
                       verbose = FALSE)

expect_built <- function(plot) {
  expect_s3_class(plot, "ggplot")
  expect_warning(ggplot2::ggplot_build(plot), NA)
}

test_that("the catalogue is tidy and every type has a latents description", {
  types <- clustering_plot_types()
  expect_named(types, c("type", "group", "description"))
  expect_false(anyDuplicated(types$type) > 0L)
  expect_false(anyNA(types$description))
  expect_setequal(types$group, c("clusters", "diagnostics", "selection"))
  expect_true(all(types$type %in% latents::plot_views()$type))
})

test_that("each type returns ggplots that build cleanly", {
  lapply(clustering_plot_types()$type, \(view) {
    drawn <- plot_clustering(fit_grid, type = view)
    if (inherits(drawn, "saqr_plots")) lapply(drawn, expect_built) else expect_built(drawn)
  })
})

test_that("a view latents draws once per model gives one entry per model", {
  several <- structure(list(EII = ggplot2::ggplot(), EEE = ggplot2::ggplot()), class = "latents_plots")
  flat <- .flatten_plots(list(sizes = ggplot2::ggplot(), tree = several))
  expect_named(flat, c("sizes", "tree_EII", "tree_EEE"))
  expect_true(all(vapply(flat, inherits, logical(1), what = "ggplot")))
  expect_identical(.flatten_plots(list()), list())
})

test_that("groups and 'all' return a printable list named by type", {
  clusters <- plot_clustering(fit_grid)
  expect_s3_class(clusters, "saqr_plots")
  expect_named(clusters, c("profiles", "bars", "heatmap", "raincloud",
                           "parallel", "pairs", "sizes"))
  everything <- plot_clustering(fit_grid, type = "all")
  expect_setequal(unique(sub("_.*", "", names(everything))), clustering_plot_types()$type)
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({ grDevices::dev.off(); unlink(path) }, add = TRUE, after = FALSE)
  diagnostics <- plot_clustering(fit_grid, type = "diagnostics")
  expect_invisible(print(diagnostics))
  expect_identical(print(diagnostics), diagnostics)
})

test_that("arguments reach the latents plot of the selected fit", {
  standardized <- plot_clustering(fit_one, type = "profiles",
                                  scale = "standardized")
  expect_built(standardized)
  expect_match(standardized$labels$subtitle, "standardized scale")
})

test_that("plot() dispatches to plot_clustering()", {
  expect_built(plot(fit_one, type = "sizes"))
})

test_that("a single candidate has no selection views to draw", {
  expect_message(none <- plot_clustering(fit_one, type = "selection"),
                 "Only one candidate")
  expect_length(none, 0L)
})

test_that("bad input raises classed errors", {
  expect_error(plot_clustering(list()), class = "saqrmisc_bad_input")
  expect_error(plot_clustering(fit_one, type = "nope"),
               class = "saqrmisc_bad_type")
})
