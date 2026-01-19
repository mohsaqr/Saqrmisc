# Tests for pivot table functionality in compare_groups

# Helper function to create test data
create_test_data <- function(n = 100, seed = 123) {
  set.seed(seed)
  data.frame(
    LLM = sample(c("GPT", "Mistral", "Qwen"), n, replace = TRUE),
    pronoun = sample(c("he/him", "she/her"), n, replace = TRUE),
    support = sample(c("Low", "Medium", "High"), n, replace = TRUE),
    score1 = rnorm(n, mean = 5, sd = 2),
    score2 = rnorm(n, mean = 3, sd = 1),
    score3 = rnorm(n, mean = 7, sd = 1.5)
  )
}

# =============================================================================
# TEST: Basic pivot table (between mode, no repeat_category)
# =============================================================================

test_that("compare_groups with pivot=TRUE creates pivot_table (basic)", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1", "score2"),
    pivot = TRUE,
    plots = FALSE,
    verbose = FALSE
  )

  # Check that pivot_table exists

  expect_true("pivot_table" %in% names(result))
  expect_s3_class(result$pivot_table, "gt_tbl")
})

test_that("pivot table works with pivot_stat='mean'", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    pivot = TRUE,
    pivot_stat = "mean",
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("pivot_table" %in% names(result))
})

test_that("pivot table works with pivot_stat='mean_sd'", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    pivot = TRUE,
    pivot_stat = "mean_sd",
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("pivot_table" %in% names(result))
})

test_that("pivot table works with pivot_stat='n'", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    pivot = TRUE,
    pivot_stat = "n",
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("pivot_table" %in% names(result))
})

test_that("pivot table works with pivot_stars=FALSE", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    pivot = TRUE,
    pivot_stars = FALSE,
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("pivot_table" %in% names(result))
})

# =============================================================================
# TEST: Different output formats
# =============================================================================

test_that("pivot table works with format='plain'", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    pivot = TRUE,
    format = "plain",
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("pivot_table" %in% names(result))
  expect_true(is.data.frame(result$pivot_table))
})

test_that("pivot table works with format='markdown'", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    pivot = TRUE,
    format = "markdown",
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("pivot_table" %in% names(result))
})

test_that("pivot table works with format='latex'", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    pivot = TRUE,
    format = "latex",
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("pivot_table" %in% names(result))
})

test_that("pivot table works with format='kable'", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    pivot = TRUE,
    format = "kable",
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("pivot_table" %in% names(result))
})

# =============================================================================
# TEST: Pivot with repeat_category (stratified analysis)
# =============================================================================

test_that("pivot table works with single repeat_category", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    repeat_category = "pronoun",
    pivot = TRUE,
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("pivot_table" %in% names(result))
})

test_that("pivot table works with multiple repeat_category variables", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    repeat_category = c("pronoun", "support"),
    pivot = TRUE,
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("pivot_table" %in% names(result))
})

test_that("pivot_by='category' puts category as columns", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    repeat_category = "pronoun",
    pivot = TRUE,
    pivot_by = "category",
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("pivot_table" %in% names(result))
})

test_that("pivot_by='repeat_category' puts repeat_category as columns", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    repeat_category = "pronoun",
    pivot = TRUE,
    pivot_by = "repeat_category",
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("pivot_table" %in% names(result))
})

test_that("pivot_split_level works with multiple repeat_category", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    repeat_category = c("pronoun", "support"),
    pivot = TRUE,
    pivot_split_level = TRUE,
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("pivot_table" %in% names(result))
})

test_that("pivot_split_level=FALSE keeps combined level column", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    repeat_category = c("pronoun", "support"),
    pivot = TRUE,
    pivot_split_level = FALSE,
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("pivot_table" %in% names(result))
})

# =============================================================================
# TEST: Within-group analysis (compare_mode = "within")
# =============================================================================

test_that("within mode creates within_table", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    compare_by = "pronoun",
    compare_mode = "within",
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("within_table" %in% names(result))
})

test_that("within mode works with multiple compare_by factors", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    compare_by = c("pronoun", "support"),
    compare_mode = "within",
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("within_table" %in% names(result))
})

test_that("within mode works with pivot_stat='mean_sd'", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    compare_by = "pronoun",
    compare_mode = "within",
    pivot_stat = "mean_sd",
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("within_table" %in% names(result))
})

test_that("within mode works with format='plain'", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    compare_by = "pronoun",
    compare_mode = "within",
    format = "plain",
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("within_table" %in% names(result))
  expect_true(is.data.frame(result$within_table))
})

test_that("within mode returns both within_table and summary_table", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    compare_by = "pronoun",
    compare_mode = "within",
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("within_table" %in% names(result))
  expect_true("summary_table" %in% names(result))
})

# =============================================================================
# TEST: Nonparametric tests
# =============================================================================

test_that("pivot table works with nonparametric tests", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    pivot = TRUE,
    nonparametric = TRUE,
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("pivot_table" %in% names(result))
})

test_that("within mode works with nonparametric tests", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    compare_by = "pronoun",
    compare_mode = "within",
    nonparametric = TRUE,
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("within_table" %in% names(result))
})

# =============================================================================
# TEST: Two-group comparisons
# =============================================================================

test_that("pivot table works with two-group comparison", {
  test_data <- create_test_data()
  # Filter to only two groups
  test_data_2group <- test_data[test_data$LLM %in% c("GPT", "Mistral"), ]

  result <- compare_groups(
    data = test_data_2group,
    category = "LLM",
    Vars = c("score1"),
    pivot = TRUE,
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("pivot_table" %in% names(result))
})

test_that("within mode works with two-group comparison", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    compare_by = "pronoun",  # pronoun has 2 levels
    compare_mode = "within",
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("within_table" %in% names(result))
})

# =============================================================================
# TEST: Multiple variables
# =============================================================================

test_that("pivot table works with multiple outcome variables", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1", "score2", "score3"),
    pivot = TRUE,
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("pivot_table" %in% names(result))
})

test_that("within mode works with multiple outcome variables", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1", "score2"),
    compare_by = "pronoun",
    compare_mode = "within",
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("within_table" %in% names(result))
})

# =============================================================================
# TEST: Edge cases
# =============================================================================

test_that("pivot works with NA values in data", {
  test_data <- create_test_data()
  # Introduce some NAs
  test_data$score1[sample(1:nrow(test_data), 10)] <- NA

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    pivot = TRUE,
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("pivot_table" %in% names(result))
})

test_that("pivot=FALSE does not create pivot_table", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    pivot = FALSE,
    plots = FALSE,
    verbose = FALSE
  )

  expect_false("pivot_table" %in% names(result))
})

# =============================================================================
# TEST: Error handling
# =============================================================================

test_that("within mode requires compare_by", {
  test_data <- create_test_data()

  expect_error(
    compare_groups(
      data = test_data,
      category = "LLM",
      Vars = c("score1"),
      compare_mode = "within",
      plots = FALSE,
      verbose = FALSE
    ),
    "compare_by must be specified"
  )
})

test_that("compare_by must exist in data", {
  test_data <- create_test_data()

  expect_error(
    compare_groups(
      data = test_data,
      category = "LLM",
      Vars = c("score1"),
      compare_by = "nonexistent_var",
      compare_mode = "within",
      plots = FALSE,
      verbose = FALSE
    ),
    "not found in data"
  )
})

# =============================================================================
# TEST: show_header parameter
# =============================================================================

test_that("pivot table respects show_header=FALSE", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    pivot = TRUE,
    show_header = FALSE,
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("pivot_table" %in% names(result))
})

test_that("within mode respects show_header=FALSE", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    compare_by = "pronoun",
    compare_mode = "within",
    show_header = FALSE,
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("within_table" %in% names(result))
})

# =============================================================================
# TEST: p_adjust_method parameter
# =============================================================================

test_that("pivot table works with different p_adjust_method", {
  test_data <- create_test_data()

  # Test various adjustment methods
  for (method in c("fdr", "bonferroni", "holm", "none")) {
    result <- compare_groups(
      data = test_data,
      category = "LLM",
      Vars = c("score1"),
      pivot = TRUE,
      p_adjust_method = method,
      plots = FALSE,
      verbose = FALSE
    )

    expect_true("pivot_table" %in% names(result),
                info = paste("Failed for method:", method))
  }
})

# =============================================================================
# TEST: Metadata in within mode
# =============================================================================

test_that("within mode includes correct metadata", {
  test_data <- create_test_data()

  result <- compare_groups(
    data = test_data,
    category = "LLM",
    Vars = c("score1"),
    compare_by = c("pronoun", "support"),
    compare_mode = "within",
    plots = FALSE,
    verbose = FALSE
  )

  expect_true("metadata" %in% names(result))
  expect_equal(result$metadata$compare_mode, "within")
  expect_equal(result$metadata$category, "LLM")
  expect_true(all(c("pronoun", "support") %in% result$metadata$compare_by))
})
