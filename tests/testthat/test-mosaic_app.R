# Reactive-logic tests for the bundled Shiny app.
#
# These drive the real server function through shiny::testServer, so the
# plot-sizing arithmetic and the stratification wiring are exercised rather
# than re-derived here. The app itself is a Suggests-only artefact, so the
# whole file skips when shiny or DT is unavailable.

skip_if_not_installed("shiny")
skip_if_not_installed("DT")

app_file <- function() {
  f <- system.file("shiny", "mosaic_app", "app.R", package = "Saqrmisc")
  if (!nzchar(f)) skip("bundled app not found")
  f
}

# Inputs for a complete run; individual tests override what they care about.
base_inputs <- list(
  data_source = "demo", var1_sel = "Specialization_EN",
  var2_sel = "Final_Evaluation_EN", by_sel = "__none__", test = "chisq",
  min_count = 10, show_pct = TRUE, pct_base = "total", min_stratum_n = 30,
  p_adjust = "BH", seed = 42, plot_style = "flat", title = "",
  tile_label = "count", label_size = 3.5, col_label_side = "top",
  row_label_side = "left", col_label_angle = "0", row_label_angle = "0",
  show_varnames = FALSE, fontsize = 8, show_legend = TRUE,
  legend_position = "right", legend_size = 0.7, legend_title = "Std",
  facet_ncol = 0, facet_show_n = TRUE, plot_w = 900, plot_h = 620,
  panel_w = 380, panel_h = 340, size_mode = "canvas",
  var1_label = "", var2_label = ""
)

test_that("the demo data carries variables worth stratifying on", {
  shiny::testServer(shiny::shinyAppFile(app_file()), {
    do.call(session$setInputs, base_inputs)
    expect_true(all(c("Specialization_EN", "Final_Evaluation_EN",
                      "Study_Mode", "Cohort_Year") %in%
                      colnames(data_loaded())))
  })
})

test_that("a run without a stratifier stays un-stratified", {
  shiny::testServer(shiny::shinyAppFile(app_file()), {
    do.call(session$setInputs, base_inputs)
    session$setInputs(run = 1)
    expect_equal(analysis()$status, "ok")
    expect_false(is_stratified())
    expect_false(inherits(res_value(), "mosaic_stratified"))
  })
})

test_that("choosing a stratifier fits one panel per group", {
  shiny::testServer(shiny::shinyAppFile(app_file()), {
    do.call(session$setInputs, c(base_inputs[names(base_inputs) != "by_sel"],
                                 list(by_sel = "Study_Mode")))
    session$setInputs(run = 1)
    expect_true(is_stratified())
    expect_equal(res_value()$n_strata, 2L)
    expect_equal(nrow(as.data.frame(res_value(), what = "strata")), 2L)
  })
})

test_that("fit warnings are collected for display, not suppressed", {
  # The demo keeps a 15-student category, which splits thin across strata and
  # must raise a low-expected-count warning the UI can show.
  shiny::testServer(shiny::shinyAppFile(app_file()), {
    do.call(session$setInputs, c(base_inputs[names(base_inputs) != "by_sel"],
                                 list(by_sel = "Study_Mode")))
    session$setInputs(run = 1)
    expect_gt(length(analysis()$warnings), 0)
    expect_true(any(grepl("Expected counts", analysis()$warnings)))
  })
})

test_that("canvas mode reports the canvas sliders unchanged", {
  shiny::testServer(shiny::shinyAppFile(app_file()), {
    do.call(session$setInputs, c(base_inputs[names(base_inputs) != "by_sel"],
                                 list(by_sel = "Study_Mode")))
    session$setInputs(run = 1)
    session$setInputs(size_mode = "canvas", plot_w = 1100, plot_h = 700)
    expect_equal(unname(plot_dims()[["w"]]), 1100)
    expect_equal(unname(plot_dims()[["h"]]), 700)
  })
})

test_that("panel mode multiplies panel size by the grid and adds padding", {
  shiny::testServer(shiny::shinyAppFile(app_file()), {
    do.call(session$setInputs, c(base_inputs[names(base_inputs) != "by_sel"],
                                 list(by_sel = "Study_Mode")))
    session$setInputs(run = 1)
    session$setInputs(size_mode = "panel", panel_w = 380, panel_h = 340)

    # 2 strata lay out 1 row x 2 cols; a right-hand legend costs 150px wide.
    expect_equal(unname(grid_dims()[["nrow"]]), 1L)
    expect_equal(unname(grid_dims()[["ncol"]]), 2L)
    expect_equal(unname(plot_dims()[["w"]]), 2 * 380 + 150)
    expect_equal(unname(plot_dims()[["h"]]), 1 * 340 + 45)

    # Stacking the panels swaps which dimension grows.
    session$setInputs(facet_ncol = 1)
    expect_equal(unname(plot_dims()[["w"]]), 1 * 380 + 150)
    expect_equal(unname(plot_dims()[["h"]]), 2 * 340 + 45)
  })
})

test_that("padding follows the legend and title, so panels keep their size", {
  shiny::testServer(shiny::shinyAppFile(app_file()), {
    do.call(session$setInputs, c(base_inputs[names(base_inputs) != "by_sel"],
                                 list(by_sel = "Study_Mode")))
    session$setInputs(run = 1)
    session$setInputs(size_mode = "panel")

    # A legend below costs height, not width.
    session$setInputs(legend_position = "bottom")
    expect_equal(unname(plot_dims()[["w"]]), 2 * 380 + 40)
    expect_equal(unname(plot_dims()[["h"]]), 340 + 120)

    # Turning the legend off reclaims that room; a title costs 35px.
    session$setInputs(show_legend = FALSE, title = "A title")
    expect_equal(unname(plot_dims()[["h"]]), 340 + 45 + 35)
  })
})

test_that("panel mode on an un-stratified run uses a 1x1 grid", {
  shiny::testServer(shiny::shinyAppFile(app_file()), {
    do.call(session$setInputs, base_inputs)
    session$setInputs(run = 1)
    session$setInputs(size_mode = "panel")
    expect_equal(unname(grid_dims()), c(1L, 1L))
    expect_equal(unname(plot_dims()[["w"]]), 380 + 150)
  })
})

test_that("plot size is capped so a large grid cannot blow up the render", {
  shiny::testServer(shiny::shinyAppFile(app_file()), {
    do.call(session$setInputs, c(base_inputs[names(base_inputs) != "by_sel"],
                                 list(by_sel = "Study_Mode")))
    session$setInputs(run = 1)
    session$setInputs(size_mode = "panel", panel_w = 1000, panel_h = 900,
                      facet_ncol = 1)
    expect_lte(plot_dims()[["w"]], 4000)
    expect_lte(plot_dims()[["h"]], 4000)
  })
})
