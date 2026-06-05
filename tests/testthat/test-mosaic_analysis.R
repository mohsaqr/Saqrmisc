test_that("mosaic_analysis works with valid input", {
  # Create test data
  test_data <- data.frame(
    var1 = sample(c("A", "B", "C"), 100, replace = TRUE),
    var2 = sample(c("X", "Y", "Z"), 100, replace = TRUE)
  )

  # Test basic functionality (using quoted strings)
  result <- mosaic_analysis(test_data, "var1", "var2", min_count = 5, verbose = FALSE)

  # Check that result is a list
  expect_type(result, "list")

  # Check that required elements exist
  expect_true("mosaic_plot" %in% names(result))
  expect_true("chi_square_test" %in% names(result))
  expect_true("cramers_v" %in% names(result))
  expect_true("summary_table" %in% names(result))

  # Check that Cramer's V is between 0 and 1
  expect_gte(result$cramers_v, 0)
  expect_lte(result$cramers_v, 1)
})

test_that("mosaic_analysis handles insufficient data", {
  # Create data with too few observations
  test_data <- data.frame(
    var1 = c("A", "B"),
    var2 = c("X", "Y")
  )

  # Should throw an error
  expect_error(mosaic_analysis(test_data, "var1", "var2", min_count = 10, verbose = FALSE))
})

test_that("mosaic_analysis works with custom parameters", {
  # Create test data
  test_data <- data.frame(
    var1 = sample(c("A", "B"), 50, replace = TRUE),
    var2 = sample(c("X", "Y"), 50, replace = TRUE)
  )

  # Test with custom parameters (using quoted strings)
  result <- mosaic_analysis(
    test_data, "var1", "var2",
    min_count = 5,
    fontsize = 10,
    title = "Test Plot",
    var1_label = "Variable 1",
    var2_label = "Variable 2",
    show_percentages = TRUE,
    percentage_base = "row",
    verbose = FALSE
  )

  # Check that result is valid
  expect_type(result, "list")
  expect_true("mosaic_plot" %in% names(result))
})

test_that("show_varnames defaults to FALSE and validates input", {
  # Default value is FALSE (variable-name titles off)
  expect_false(formals(mosaic_analysis)$show_varnames)

  test_data <- data.frame(
    var1 = sample(c("A", "B"), 60, replace = TRUE),
    var2 = sample(c("X", "Y"), 60, replace = TRUE)
  )

  # Both settings produce a valid plot without error
  expect_no_error(
    mosaic_analysis(test_data, "var1", "var2", min_count = 5,
                    show_varnames = FALSE, verbose = FALSE)
  )
  expect_no_error(
    mosaic_analysis(test_data, "var1", "var2", min_count = 5,
                    show_varnames = TRUE, verbose = FALSE)
  )

  # Non-logical input is rejected
  expect_error(
    mosaic_analysis(test_data, "var1", "var2", min_count = 5,
                    show_varnames = "yes", verbose = FALSE),
    "show_varnames"
  )
})

test_that("plot_style switches between flat ggplot and classic vcd", {
  skip_if_not_installed("ggplot2")
  test_data <- data.frame(
    var1 = sample(c("A", "B", "C"), 120, replace = TRUE),
    var2 = sample(c("X", "Y"), 120, replace = TRUE)
  )

  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)

  flat <- mosaic_analysis(test_data, "var1", "var2", min_count = 5,
                          plot_style = "flat", verbose = FALSE)
  expect_s3_class(flat$mosaic_plot, "ggplot")

  classic <- mosaic_analysis(test_data, "var1", "var2", min_count = 5,
                             plot_style = "classic", verbose = FALSE)
  expect_s3_class(classic$mosaic_plot, "structable")

  # flat is the default
  expect_identical(eval(formals(mosaic_analysis)$plot_style)[1], "flat")
})

test_that("tile_label and category-label placement options work and validate", {
  skip_if_not_installed("ggplot2")
  test_data <- data.frame(
    var1 = sample(c("A", "B", "C"), 150, replace = TRUE),
    var2 = sample(c("X", "Y", "Z"), 150, replace = TRUE)
  )
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)

  for (tl in c("count", "percent", "residual", "category", "none")) {
    p <- mosaic_analysis(test_data, "var1", "var2", min_count = 5,
                         tile_label = tl, verbose = FALSE)
    expect_s3_class(p$mosaic_plot, "ggplot")
  }

  # labels on both sides, rows rotated vertical
  expect_no_error(
    mosaic_analysis(test_data, "var1", "var2", min_count = 5,
                    col_label_side = "both", row_label_side = "both",
                    row_label_angle = 90, verbose = FALSE)
  )

  expect_error(
    mosaic_analysis(test_data, "var1", "var2", min_count = 5,
                    tile_label = "counts", verbose = FALSE)
  )
  expect_error(
    mosaic_analysis(test_data, "var1", "var2", min_count = 5,
                    col_label_angle = "vertical", verbose = FALSE),
    "angle"
  )
})

test_that("plot.mosaic_analysis re-renders from stored parts with overrides", {
  skip_if_not_installed("ggplot2")
  test_data <- data.frame(
    var1 = sample(c("A", "B", "C"), 150, replace = TRUE),
    var2 = sample(c("X", "Y"), 150, replace = TRUE)
  )
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)

  res <- mosaic_analysis(test_data, "var1", "var2", min_count = 5, verbose = FALSE)
  # the object carries everything needed to redraw
  expect_true(all(c("plot_parts", "plot_args") %in% names(res)))
  expect_true(all(c("table", "residuals", "data") %in% names(res$plot_parts)))

  # re-render with new styling — returns a ggplot, no recomputation needed
  p1 <- plot(res, tile_label = "percent", legend_size = 0.4)
  expect_s3_class(p1, "ggplot")
  p2 <- plot(res, plot_style = "classic")
  expect_s3_class(p2, "structable")

  # unknown styling args are rejected
  expect_error(plot(res, not_a_real_arg = 1), "Unknown styling")
})

test_that("column percentage base builds the consolidated table without error", {
  # Regression: the column-base total row spliced an unnamed vector into
  # tibble(), which errored with duplicated column names.
  test_data <- data.frame(
    var1 = sample(c("A", "B", "C"), 120, replace = TRUE),
    var2 = sample(c("X", "Y"), 120, replace = TRUE)
  )
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  res <- mosaic_analysis(test_data, "var1", "var2", min_count = 5,
                         show_percentages = TRUE, percentage_base = "column",
                         verbose = FALSE)
  expect_true("% (column)" %in% res$summary_table$Type)
})

test_that("legend controls validate their inputs", {
  test_data <- data.frame(
    var1 = sample(c("A", "B"), 60, replace = TRUE),
    var2 = sample(c("X", "Y"), 60, replace = TRUE)
  )
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)

  # show_legend must be logical
  expect_error(
    mosaic_analysis(test_data, "var1", "var2", min_count = 5,
                    show_legend = "yes", verbose = FALSE),
    "show_legend"
  )
  # legend_size must be a positive number
  expect_error(
    mosaic_analysis(test_data, "var1", "var2", min_count = 5,
                    legend_size = 0, verbose = FALSE),
    "legend_size"
  )
  # legend_position is constrained
  expect_error(
    mosaic_analysis(test_data, "var1", "var2", min_count = 5,
                    legend_position = "middle", verbose = FALSE)
  )
  # a valid compact-legend call succeeds
  expect_no_error(
    mosaic_analysis(test_data, "var1", "var2", min_count = 5,
                    legend_size = 0.4, legend_position = "bottom",
                    show_legend = TRUE, verbose = FALSE)
  )
})

test_that("show_varnames actually toggles the variable-name titles", {
  skip_if_not_installed("vcd")
  skip_if_not_installed("grid")

  test_data <- data.frame(
    Specialization = factor(rep(c("Eng", "Med", "Nur"), each = 40)),
    Outcome        = factor(sample(c("Fail", "Pass"), 120, replace = TRUE))
  )

  # Pull every text label out of the current grid display list.
  drawn_labels <- function() {
    paths <- grid::grid.grep("text", grep = TRUE, global = TRUE)
    out <- vapply(paths, function(p) {
      g <- tryCatch(grid::grid.get(p), error = function(e) NULL)
      if (!is.null(g$label)) as.character(g$label)[1] else NA_character_
    }, character(1))
    out[!is.na(out)]
  }

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # Default (FALSE): variable names must NOT be drawn ...
  grid::grid.newpage()
  invisible(mosaic_analysis(test_data, "Specialization", "Outcome",
                            min_count = 5, plot_style = "classic",
                            show_varnames = FALSE, verbose = FALSE))
  off <- drawn_labels()
  expect_false(any(grepl("Specialization", off)))
  expect_false(any(grepl("Outcome", off)))
  # ... but the category labels still are.
  expect_true(any(grepl("Eng", off)))

  # show_varnames = TRUE: variable names ARE drawn.
  grid::grid.newpage()
  invisible(mosaic_analysis(test_data, "Specialization", "Outcome",
                            min_count = 5, plot_style = "classic",
                            show_varnames = TRUE, verbose = FALSE))
  on_lbls <- drawn_labels()
  expect_true(any(grepl("Specialization", on_lbls)))
  expect_true(any(grepl("Outcome", on_lbls)))
})
