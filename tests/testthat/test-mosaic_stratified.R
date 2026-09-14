# Tests for stratified (faceted) mosaic analysis.
#
# The fixture below is a hand-built confounding structure with known cell
# counts, so the per-stratum and conditional statistics can be checked against
# values computed by hand rather than against whatever the code happens to emit.

# Dept A is easy and mostly Female; Dept B is hard and mostly Male. Within each
# department Females pass more often; pooled, the department imbalance inflates
# the apparent sex effect several-fold.
make_confounded <- function() {
  cells <- rbind(
    data.frame(dept = "A", sex = "Female", res = "Pass", n = 180),
    data.frame(dept = "A", sex = "Female", res = "Fail", n =  20),
    data.frame(dept = "A", sex = "Male",   res = "Pass", n =  40),
    data.frame(dept = "A", sex = "Male",   res = "Fail", n =  10),
    data.frame(dept = "B", sex = "Female", res = "Pass", n =  20),
    data.frame(dept = "B", sex = "Female", res = "Fail", n =  30),
    data.frame(dept = "B", sex = "Male",   res = "Pass", n =  60),
    data.frame(dept = "B", sex = "Male",   res = "Fail", n = 140)
  )
  cells[rep(seq_len(nrow(cells)), cells$n), c("dept", "sex", "res")]
}

make_plain <- function(n = 600, seed = 11) {
  set.seed(seed)
  data.frame(
    a = sample(c("X", "Y", "Z"), n, replace = TRUE),
    b = sample(c("P", "Q"), n, replace = TRUE),
    g = sample(c("g1", "g2", "g3"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------- calibration

test_that("conditional odds ratio matches a hand-computed Mantel-Haenszel value", {
  fit <- mosaic_analysis(make_confounded(), "sex", "res", by = "dept",
                         min_count = 5, verbose = FALSE)
  overall <- as.data.frame(fit, what = "overall")
  cond <- overall[overall$scope == "conditional", ]

  # MH estimator = sum(a_i d_i / n_i) / sum(b_i c_i / n_i), with the table
  # ordered (Female, Male) x (Fail, Pass):
  #   A: a=20 b=180 c=10  d=40  n=250
  #   B: a=30 b=20  c=140 d=60  n=250
  hand_or <- (20 * 40 / 250 + 30 * 60 / 250) /
             (180 * 10 / 250 + 20 * 140 / 250)
  expect_equal(cond$common_or, round(hand_or, 3), tolerance = 1e-8)

  # A weighted average of the stratum-specific odds ratios must lie between them.
  expect_gt(cond$common_or, (20 * 40) / (180 * 10))
  expect_lt(cond$common_or, (30 * 60) / (20 * 140))
})

test_that("per-stratum Cramer's V matches a direct chisq.test on the same stratum", {
  d <- make_confounded()
  fit <- mosaic_analysis(d, "sex", "res", by = "dept", min_count = 5, verbose = FALSE)
  strata <- as.data.frame(fit, what = "strata")

  a <- d[d$dept == "A", ]
  tab_a <- table(a$sex, a$res)
  chi_a <- suppressWarnings(stats::chisq.test(tab_a))
  v_a <- sqrt(as.numeric(chi_a$statistic) / (sum(tab_a) * (min(dim(tab_a)) - 1)))

  expect_equal(strata$cramers_v[strata$stratum == "A"], round(v_a, 3))
  expect_equal(strata$statistic[strata$stratum == "A"],
               round(as.numeric(chi_a$statistic), 3))
})

test_that("confounding is visible: pooled effect far exceeds every stratum effect", {
  fit <- mosaic_analysis(make_confounded(), "sex", "res", by = "dept",
                         min_count = 5, verbose = FALSE)
  strata <- as.data.frame(fit, what = "strata")
  pooled_v <- as.data.frame(fit, what = "overall")
  pooled_v <- pooled_v$cramers_v[pooled_v$scope == "pooled"]

  expect_gt(pooled_v, 2 * max(strata$cramers_v))
})

# ----------------------------------------------------------------- invariants

test_that("stratum sizes partition the filtered sample exactly", {
  fit <- mosaic_analysis(make_plain(), "a", "b", by = "g", verbose = FALSE)
  expect_equal(sum(as.data.frame(fit, what = "strata")$n), fit$filtered_n)
})

test_that("adjusted p-values are never smaller than raw p-values", {
  fit <- mosaic_analysis(make_plain(), "a", "b", by = "g", verbose = FALSE)
  strata <- as.data.frame(fit, what = "strata")
  expect_true(all(strata$p_adjusted >= strata$p_value - 1e-12))
  expect_true(all(strata$p_adjusted <= 1))
})

# One stratum in which category "Z" is never observed, so its panel contains a
# structurally empty row on the pooled category grid.
make_gappy <- function(seed = 3) {
  set.seed(seed)
  arm <- function(g, levs, n) data.frame(
    a = sample(levs, n, replace = TRUE),
    b = sample(c("P", "Q"), n, replace = TRUE),
    g = g, stringsAsFactors = FALSE)
  rbind(arm("g1", c("X", "Y", "Z"), 300),
        arm("g2", c("X", "Y"), 300),
        arm("g3", c("X", "Y", "Z"), 300))
}

test_that("every panel carries the same categories, so tiles line up", {
  fit <- suppressWarnings(
    mosaic_analysis(make_gappy(), "a", "b", by = "g", verbose = FALSE))
  dims <- lapply(fit$strata_tables, dimnames)
  expect_true(all(vapply(dims, function(x) identical(x, dims[[1]]), logical(1))))
  # the unobserved category is present as a genuine zero, not dropped
  expect_equal(unname(rowSums(fit$strata_tables$g2)["Z"]), 0)
})

test_that("a structurally empty category is excluded from that stratum's test", {
  expect_warning(
    fit <- mosaic_analysis(make_gappy(), "a", "b", by = "g", verbose = FALSE),
    class = "saqrmisc_empty_cells")
  strata <- as.data.frame(fit, what = "strata")

  # g2 has a 2x2 core, so df must be 1; the full-grid strata are 3x2, df 2.
  expect_equal(strata$df[strata$stratum == "g2"], 1)
  expect_equal(strata$df[strata$stratum == "g1"], 2)

  # the statistic must be real, not the NaN a zero expected count would give
  expect_false(is.nan(strata$statistic[strata$stratum == "g2"]))
  expect_false(is.na(strata$cramers_v[strata$stratum == "g2"]))
  expect_type(strata$n_cells_beyond_2, "integer")
  expect_false(anyNA(strata$n_cells_beyond_2))

  # the absent cells are NA (structurally absent), never NaN (0/0)
  resid <- as.data.frame(fit, what = "residuals")
  gap <- resid[resid$stratum == "g2" & resid$a == "Z", ]
  expect_true(all(is.na(gap$std_residual)))
  expect_false(any(is.nan(gap$std_residual)))
  expect_equal(gap$observed, c(0L, 0L))

  # and the plot still renders on a finite shared scale
  lims <- ggplot2::ggplot_build(plot(fit))$plot$scales$get_scales("fill")$limits
  expect_true(all(is.finite(lims)))
})

test_that("per-stratum results do not depend on the order of stratum levels", {
  d <- make_plain()
  fit1 <- mosaic_analysis(d, "a", "b", by = "g", verbose = FALSE)
  d$g <- factor(d$g, levels = c("g3", "g2", "g1"))
  fit2 <- mosaic_analysis(d, "a", "b", by = "g", verbose = FALSE)

  s1 <- as.data.frame(fit1, what = "strata")
  s2 <- as.data.frame(fit2, what = "strata")
  expect_equal(s1$cramers_v[order(s1$stratum)], s2$cramers_v[order(s2$stratum)])
})

test_that("residual colour scale is shared across panels", {
  fit <- mosaic_analysis(make_plain(), "a", "b", by = "g", verbose = FALSE)
  p <- plot(fit)
  lims <- ggplot2::ggplot_build(p)$plot$scales$get_scales("fill")$limits
  worst <- max(abs(unlist(lapply(fit$plot_parts$residuals, as.numeric))))

  expect_equal(lims, c(-worst, worst), tolerance = 1e-8)
  expect_equal(lims[1], -lims[2])
})

# ---------------------------------------------------------------------- plot

test_that("a stratified fit renders one panel per stratum", {
  fit <- mosaic_analysis(make_plain(), "a", "b", by = "g", verbose = FALSE)
  layout <- ggplot2::ggplot_build(plot(fit))$layout$layout
  expect_equal(nrow(layout), 3L)
  expect_equal(length(unique(ggplot2::ggplot_build(
    plot(fit, facet_ncol = 1))$layout$layout$COL)), 1L)
})

test_that("panel strips report stratum sizes unless switched off", {
  fit <- mosaic_analysis(make_plain(), "a", "b", by = "g", verbose = FALSE)
  with_n <- ggplot2::ggplot_build(plot(fit))$layout$layout$stratum
  without <- ggplot2::ggplot_build(plot(fit, facet_show_n = FALSE))$layout$layout$stratum

  expect_true(all(grepl("^g[123] \\(n = [0-9,]+\\)$", as.character(with_n))))
  expect_equal(as.character(without), c("g1", "g2", "g3"))
})

# ------------------------------------------------------------- error contract

test_that("stratifier problems raise classed errors", {
  d <- make_plain()
  expect_error(mosaic_analysis(d, "a", "b", by = "absent", verbose = FALSE),
               "not found in data")
  expect_error(mosaic_analysis(d, "a", "b", by = "a", verbose = FALSE),
               "must differ")
  expect_error(mosaic_analysis(d, "a", "b", by = c("g", "a"), verbose = FALSE),
               "single character string")
})

test_that("too few usable strata raises a classed error", {
  d <- make_plain()
  d$one_big <- ifelse(seq_len(nrow(d)) <= 5, "tiny", "big")
  expect_error(
    suppressWarnings(mosaic_analysis(d, "a", "b", by = "one_big",
                                     min_stratum_n = 30, verbose = FALSE)),
    class = "saqrmisc_too_few_strata")
})

test_that("undersized strata are dropped with a classed warning naming them", {
  d <- make_plain()
  d$grp <- ifelse(seq_len(nrow(d)) <= 8, "sliver",
                  ifelse(seq_len(nrow(d)) %% 2 == 0, "even", "odd"))
  expect_warning(
    fit <- mosaic_analysis(d, "a", "b", by = "grp", min_stratum_n = 30,
                           verbose = FALSE),
    class = "saqrmisc_strata_dropped")
  expect_equal(fit$dropped_strata, "sliver")
  expect_equal(fit$n_strata, 2L)
})

test_that("strata accessors refuse an un-stratified fit", {
  fit <- mosaic_analysis(make_plain(), "a", "b", verbose = FALSE)
  expect_error(as.data.frame(fit, what = "strata"),
               class = "saqrmisc_not_stratified")
  expect_error(as.data.frame(fit, what = "overall"),
               class = "saqrmisc_not_stratified")
})

# ----------------------------------------------------- accessors and back-compat

test_that("an un-stratified fit is unchanged by the new argument", {
  fit <- mosaic_analysis(make_plain(), "a", "b", verbose = FALSE)
  expect_identical(class(fit), c("mosaic_analysis", "list"))
  expect_null(fit$by_name)
  expect_s3_class(as.data.frame(fit, what = "summary"), "data.frame")
  expect_s3_class(as.data.frame(fit, what = "residuals"), "data.frame")
})

test_that("as.data.frame returns tidy frames of the documented shape", {
  fit <- mosaic_analysis(make_plain(), "a", "b", by = "g", verbose = FALSE)

  strata <- as.data.frame(fit, what = "strata")
  expect_s3_class(strata, "data.frame")
  expect_equal(nrow(strata), 3L)
  expect_true(all(c("stratum", "n", "test", "statistic", "df", "p_value",
                    "p_adjusted", "cramers_v", "effect_size",
                    "n_cells_beyond_2") %in% names(strata)))

  # one row per stratum x cell of the shared category grid
  resid <- as.data.frame(fit, what = "residuals")
  expect_equal(nrow(resid), 3L * 3L * 2L)
  expect_true(all(c("stratum", "a", "b", "observed", "expected",
                    "std_residual", "beyond_2") %in% names(resid)))
  expect_type(resid$beyond_2, "logical")

  expect_equal(as.data.frame(fit, what = "overall")$scope,
               c("pooled", "conditional"))
})

test_that("expected counts reconstruct the observed margins within each stratum", {
  fit <- mosaic_analysis(make_plain(), "a", "b", by = "g", verbose = FALSE)
  resid <- as.data.frame(fit, what = "residuals")
  by_stratum <- split(resid, resid$stratum)
  for (s in by_stratum) {
    expect_equal(sum(s$expected), sum(s$observed), tolerance = 1e-6)
  }
})

test_that("a seed makes the Monte-Carlo Fisher p-value reproducible", {
  d <- make_plain()
  p1 <- mosaic_analysis(d, "a", "b", by = "g", use_fisher = TRUE, seed = 42,
                        verbose = FALSE)$strata_summary$p_value
  p2 <- mosaic_analysis(d, "a", "b", by = "g", use_fisher = TRUE, seed = 42,
                        verbose = FALSE)$strata_summary$p_value
  expect_identical(p1, p2)
})

test_that("seeding restores the caller's RNG stream", {
  d <- make_plain()
  set.seed(99)
  before <- .Random.seed
  invisible(mosaic_analysis(d, "a", "b", by = "g", use_fisher = TRUE, seed = 7,
                            verbose = FALSE))
  expect_identical(.Random.seed, before)
})

test_that("print and summary methods return invisibly without error", {
  fit <- mosaic_analysis(make_plain(), "a", "b", by = "g", verbose = FALSE)
  expect_output(print(fit), "Stratified Mosaic Analysis")
  expect_output(print(fit), "stratified by")
  expect_output(summary(fit), "Per-stratum tests")
  expect_s3_class(suppressWarnings(summary(fit)), "data.frame")
})

# ------------------------------------------------------- category-label fitting

# Category labels are drawn inside the plot area, so faceting shrinks the room
# they have. These tests pin the adaptive behaviour: narrower panels drop the
# labels that would overprint, and widening the panels brings them back.

# Pull the column-label layer's data out of a built plot.
col_label_data <- function(p) {
  built <- ggplot2::ggplot_build(p)
  for (layer in built$plot$layers) {
    d <- layer$data
    if (is.data.frame(d) && "vj" %in% names(d)) return(d)
  }
  NULL
}

make_longnames <- function(seed = 8) {
  set.seed(seed)
  n <- 3000
  spec <- sample(c("Engineering", "Medicine", "Nursing", "Social Sciences"),
                 n, replace = TRUE, prob = c(0.30, 0.18, 0.40, 0.12))
  data.frame(
    spec = spec,
    res  = sample(c("Pass", "Fail"), n, replace = TRUE),
    grp  = sample(c("g1", "g2", "g3"), n, replace = TRUE),
    stringsAsFactors = FALSE)
}

test_that("an un-stratified plot keeps the category labels it always kept", {
  d <- make_longnames()
  fit <- mosaic_analysis(d, "spec", "res", verbose = FALSE)
  labs <- col_label_data(plot(fit))$lab
  # Single-mosaic output must be untouched by the panel label fitting: every
  # label present, and none wrapped onto a second line.
  expect_setequal(labs, c("Engineering", "Medicine", "Nursing", "Social Sciences"))
  expect_false(any(grepl("\n", labs, fixed = TRUE)))
})

test_that("narrow panels drop labels that would not fit", {
  d <- make_longnames()
  fit <- mosaic_analysis(d, "spec", "res", by = "grp", verbose = FALSE)
  narrow <- col_label_data(plot(fit))                     # 3 panels in one row
  wide   <- col_label_data(plot(fit, facet_ncol = 1))     # full-width panels

  n_narrow <- nrow(narrow)
  n_wide   <- nrow(wide)
  expect_lt(n_narrow, n_wide)

  # widening restores the full category set in every panel. Labels may carry a
  # deliberate line break, so compare on the unwrapped text.
  expect_setequal(unique(gsub("\n", " ", wide$lab)),
                  c("Engineering", "Medicine", "Nursing", "Social Sciences"))
  # whatever survives in a narrow panel is a genuine category, never truncated
  expect_true(all(gsub("\n", " ", narrow$lab) %in%
                    c("Engineering", "Medicine", "Nursing", "Social Sciences")))
})

test_that("the panel-column count comes from ggplot2's own layout", {
  d <- make_longnames()
  fit <- mosaic_analysis(d, "spec", "res", by = "grp", verbose = FALSE)
  # 3 panels lay out in ONE row, so the budget must divide by 3, not by
  # ceiling(sqrt(3)) = 2. Guard against the layout assumption drifting.
  expect_equal(unname(ggplot2::wrap_dims(3)[2]), 3)
  layout <- ggplot2::ggplot_build(plot(fit))$layout$layout
  expect_equal(max(layout$COL), 3L)
})

test_that("a long category label wraps rather than being dropped when it can fit", {
  d <- make_longnames()
  fit <- mosaic_analysis(d, "spec", "res", by = "grp", verbose = FALSE)
  labs <- col_label_data(plot(fit, facet_ncol = 1))$lab
  # "Social Sciences" is the longest name and the narrowest column; at full
  # panel width it must survive, wrapped onto two lines if need be.
  social <- labs[grepl("^Social", labs)]
  expect_gt(length(social), 0)
  expect_true(all(gsub("\n", " ", social) == "Social Sciences"))
})

# ------------------------------------------------- combined count/percent tile

tile_data <- function(p) ggplot2::ggplot_build(p)$plot$data

make_tiles <- function(seed = 11) {
  set.seed(seed)
  data.frame(
    a = sample(c("Alpha", "Beta", "Gamma"), 1200, replace = TRUE,
               prob = c(0.5, 0.35, 0.15)),
    b = sample(c("Yes", "No"), 1200, replace = TRUE),
    g = sample(c("g1", "g2"), 1200, replace = TRUE),
    stringsAsFactors = FALSE)
}

test_that("count_percent shows the count above its percentage", {
  fit <- mosaic_analysis(make_tiles(), "a", "b", verbose = FALSE)
  td <- tile_data(plot(fit, tile_label = "count_percent"))

  # two lines: the count, then the percentage in parentheses
  expect_true(all(grepl("^[0-9,]+\n\\([0-9.]+%\\)$", td$lab)))

  # the first line must be exactly the count the "count" label would show
  counts <- tile_data(plot(fit, tile_label = "count"))$lab
  expect_equal(vapply(strsplit(td$lab, "\n"), `[`, character(1), 1), counts)
})

test_that("the percentage honours percentage_base", {
  fit <- mosaic_analysis(make_tiles(), "a", "b", verbose = FALSE)
  pct_of <- function(base) {
    td <- tile_data(plot(fit, tile_label = "count_percent",
                         percentage_base = base))
    as.numeric(sub("%\\)$", "", sub("^.*\\(", "", td$lab)))
  }
  # 3 rows and 2 columns, so a row base sums to 300 and a column base to 200
  expect_equal(sum(pct_of("total")),  100, tolerance = 0.5)
  expect_equal(sum(pct_of("row")),    300, tolerance = 0.5)
  expect_equal(sum(pct_of("column")), 200, tolerance = 0.5)
})

test_that("stratified percentages are computed within each panel", {
  fit <- mosaic_analysis(make_tiles(), "a", "b", by = "g", verbose = FALSE)
  td <- tile_data(plot(fit, tile_label = "count_percent"))
  by_panel <- tapply(td$pct, td$stratum, sum)
  expect_true(all(abs(by_panel - 100) < 0.5))

  # and the counts still partition the stratum. tapply returns a 1-d array, so
  # as.vector() is needed to compare values rather than attributes.
  n_by_panel <- as.vector(tapply(td$count, td$stratum, sum))
  expect_equal(sort(n_by_panel), sort(as.vector(fit$strata_n)))
})

test_that("a two-line tile label needs more tile height than a one-line one", {
  # Gamma/Yes is the smallest cell, so it is the first to lose its label when
  # the label grows from one line to two.
  fit <- mosaic_analysis(make_tiles(), "a", "b", verbose = FALSE)
  one <- tile_data(plot(fit, tile_label = "count"))
  two <- tile_data(plot(fit, tile_label = "count_percent"))
  expect_equal(nrow(one), nrow(two))
  # never MORE labels shown when the label is taller
  expect_lte(sum(nzchar(two$lab)), sum(nzchar(one$lab)))
})

test_that("count_percent is accepted by mosaic_analysis and rejected if misspelled", {
  d <- make_tiles()
  expect_s3_class(
    mosaic_analysis(d, "a", "b", tile_label = "count_percent", verbose = FALSE),
    "mosaic_analysis")
  expect_error(
    mosaic_analysis(d, "a", "b", tile_label = "count_pct", verbose = FALSE))
})
