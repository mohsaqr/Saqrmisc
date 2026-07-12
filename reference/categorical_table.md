# Generate Publication-Ready Frequency Tables for Categorical Variables

Creates comprehensive frequency tables for categorical variables with
counts, percentages, and optional cross-tabulation. Supports chi-square
tests for associations and outputs beautifully formatted gt tables.

## Usage

``` r
categorical_table(
  data,
  var,
  by = NULL,
  group_by = NULL,
  percentages = NULL,
  show_n = TRUE,
  show_total = TRUE,
  show_missing = FALSE,
  show_header = TRUE,
  chi_square = TRUE,
  fisher = FALSE,
  cramers_v = TRUE,
  digits = 1,
  sort_by = "none",
  labels = NULL,
  title = NULL,
  subtitle = NULL,
  format = c("gt", "plain", "markdown", "latex", "kable"),
  theme = "default",
  combine = TRUE,
  interpret = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame containing the variables to summarize.

- var:

  Character. Name of the primary categorical variable.

- by:

  Optional character. Name of a second categorical variable for
  cross-tabulation. When provided, creates a contingency table.

- group_by:

  Optional character. Name of a stratification variable. Creates
  separate tables for each level of this variable.

- percentages:

  Character vector specifying which percentages to include. Options:
  "col" (column), "row", "total", "none". Default: c("col") for
  cross-tabs, c("total") for single variable.

- show_n:

  Logical. Show frequency counts? Default: TRUE.

- show_total:

  Logical. Include row/column totals? Default: TRUE.

- show_missing:

  Logical. Include missing values as a category? Default: FALSE.

- show_header:

  Logical. Show title/subtitle header? Default: TRUE. Set to FALSE to
  hide the table header.

- chi_square:

  Logical. Compute chi-square test for cross-tabulations? Default: TRUE.

- fisher:

  Logical. Also compute Fisher's exact test (for small samples)?
  Default: FALSE.

- cramers_v:

  Logical. Compute Cramer's V effect size? Default: TRUE.

- digits:

  Integer. Decimal places for percentages. Default: 1.

- sort_by:

  How to sort categories: "none" (original order), "frequency"
  (descending), "alphabetical". Default: "none".

- labels:

  Optional named character vector for category labels.

- title:

  Optional character string for table title.

- subtitle:

  Optional character string for table subtitle.

- format:

  Character. Output format: "gt" (default, publication-ready gt table),
  "plain" (data frame), "markdown", "latex", or "kable".

- theme:

  Character. Visual theme: "default", "minimal", "dark", or "colorful".
  Default: "default".

- combine:

  Logical. For single variables, combine n and percent in one column?
  Default: TRUE. Shows as "n (pct)".

- interpret:

  Logical. Pass results to AI for automatic interpretation? Default
  FALSE. When TRUE, generates clean Methods and Results text using AI.
  Requires API key setup (see
  [`set_api_key`](https://pak.dynasite.org/Saqrmisc/reference/set_api_key.md)).

- ...:

  Additional arguments passed to
  [`pass`](https://pak.dynasite.org/Saqrmisc/reference/pass.md) when
  interpret = TRUE (e.g., provider, model, context).

## Value

A gt table object (default) or data frame with frequencies and
percentages. For cross-tabulations with chi_square = TRUE, includes test
statistics.

## See also

[`descriptive_table`](https://pak.dynasite.org/Saqrmisc/reference/descriptive_table.md)
for numeric variable summaries
[`mosaic_analysis`](https://pak.dynasite.org/Saqrmisc/reference/mosaic_analysis.md)
for mosaic plot visualization with chi-square tests

## Examples

``` r
if (FALSE) { # \dontrun{
# ============================================================
# EXAMPLE 1: Single Variable Frequency Table
# ============================================================
data <- data.frame(
  gender = sample(c("Male", "Female", "Other"), 200, replace = TRUE,
                  prob = c(0.45, 0.45, 0.10)),
  education = sample(c("High School", "Bachelor", "Master", "PhD"), 200,
                     replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)),
  country = sample(c("USA", "UK", "Germany"), 200, replace = TRUE)
)

# Basic frequency table
categorical_table(data, var = "gender")

# With sorting by frequency
categorical_table(data, var = "education", sort_by = "frequency")

# ============================================================
# EXAMPLE 2: Cross-Tabulation (Two Variables)
# ============================================================
categorical_table(
  data = data,
  var = "gender",
  by = "education",
  chi_square = TRUE,
  cramers_v = TRUE
)

# ============================================================
# EXAMPLE 3: Stratified Analysis
# ============================================================
categorical_table(
  data = data,
  var = "gender",
  by = "education",
  group_by = "country"
)

# ============================================================
# EXAMPLE 4: Custom Labels and Formatting
# ============================================================
categorical_table(
  data = data,
  var = education,
  labels = c(
    "High School" = "Secondary Education",
    "Bachelor" = "Bachelor's Degree",
    "Master" = "Master's Degree",
    "PhD" = "Doctorate"
  ),
  title = "Educational Attainment",
  subtitle = "Sample Distribution",
  theme = "colorful"
)

# ============================================================
# EXAMPLE 5: Show Row and Column Percentages
# ============================================================
categorical_table(
  data = data,
  var = gender,
  by = education,
  percentages = c("row", "col"),
  combine = FALSE
)
} # }
```
