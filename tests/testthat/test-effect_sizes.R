# Cohen's d and eta squared replaced effsize::cohen.d() and
# effectsize::eta_squared(); they matched both to 4.4e-16 on 200 random cases
# (2026-11-14). These tests pin the formulas to hand-computed values.

test_that("Cohen's d is the mean difference over the pooled SD", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(2, 4, 6, 8, 10)
  # means 3 and 6; variances 2.5 and 10; pooled SD sqrt((4 * 2.5 + 4 * 10) / 8)
  expect_equal(.cohens_d(x, y), (3 - 6) / sqrt(50 / 8))
  expect_equal(.cohens_d(x, y), -1.2)
})

test_that("Cohen's d is antisymmetric and unchanged by shift and scale", {
  set.seed(3)
  x <- rnorm(20, 1)
  y <- rnorm(25)
  expect_equal(.cohens_d(y, x), -.cohens_d(x, y))
  expect_equal(.cohens_d(3 * x + 7, 3 * y + 7), .cohens_d(x, y))
})

test_that("eta squared is the between-group share of the total sum of squares", {
  df <- data.frame(g = factor(rep(c("a", "b"), each = 3)), y = c(1, 2, 3, 4, 5, 6))
  aov_table <- summary(stats::aov(y ~ g, data = df))[[1]]
  # group means 2 and 5, grand mean 3.5: SS_between 13.5, SS_within 4
  expect_equal(.eta_squared(aov_table), 13.5 / 17.5)
})

test_that("eta squared lies in [0, 1] and is 0 for identical groups", {
  df <- data.frame(g = factor(rep(c("a", "b", "c"), each = 3)), y = rep(c(1, 2, 3), 3))
  expect_equal(.eta_squared(summary(stats::aov(y ~ g, data = df))[[1]]), 0)
  set.seed(4)
  df$y <- rnorm(9)
  eta <- .eta_squared(summary(stats::aov(y ~ g, data = df))[[1]])
  expect_true(eta >= 0 && eta <= 1)
})

test_that("a missing optional package raises saqrmisc_missing_package", {
  expect_error(.kr_need("notapackage.saqrmisc"), class = "saqrmisc_missing_package")
})
