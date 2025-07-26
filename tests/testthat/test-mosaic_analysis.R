test_that("mosaic_analysis works with valid input", {
  # Create test data
  test_data <- data.frame(
    var1 = sample(c("A", "B", "C"), 100, replace = TRUE),
    var2 = sample(c("X", "Y", "Z"), 100, replace = TRUE)
  )
  
  # Test basic functionality
  result <- mosaic_analysis(test_data, var1, var2, min_count = 5)
  
  # Check that result is a list
  expect_type(result, "list")
  
  # Check that required elements exist
  expect_true("mosaic_plot" %in% names(result))
  expect_true("chi_square_test" %in% names(result))
  expect_true("cramers_v" %in% names(result))
  expect_true("summary_table" %in% names(result))
  
  # Check that Cramér's V is between 0 and 1
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
  expect_error(mosaic_analysis(test_data, var1, var2, min_count = 10))
})

test_that("mosaic_analysis works with custom parameters", {
  # Create test data
  test_data <- data.frame(
    var1 = sample(c("A", "B"), 50, replace = TRUE),
    var2 = sample(c("X", "Y"), 50, replace = TRUE)
  )
  
  # Test with custom parameters
  result <- mosaic_analysis(
    test_data, var1, var2,
    min_count = 5,
    fontsize = 10,
    title = "Test Plot",
    var1_label = "Variable 1",
    var2_label = "Variable 2",
    show_percentages = TRUE,
    percentage_base = "row"
  )
  
  # Check that result is valid
  expect_type(result, "list")
  expect_true("mosaic_plot" %in% names(result))
}) 