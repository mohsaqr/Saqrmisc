# Analyze Missing Data Patterns

Provides comprehensive missing data analysis including patterns,
Little's MCAR test, and visualizations.

## Usage

``` r
missing_analysis(
  data,
  Vars = NULL,
  pattern_plot = TRUE,
  mcar_test = TRUE,
  correlations = FALSE,
  digits = 2
)
```

## Arguments

- data:

  A data frame to analyze.

- Vars:

  Character vector of variable names. If NULL (default), all variables
  are included.

- pattern_plot:

  Logical. Create missing data pattern visualization? Default TRUE.

- mcar_test:

  Logical. Perform Little's MCAR test? Default TRUE.

- correlations:

  Logical. Show correlations between missingness indicators? Default
  FALSE.

- digits:

  Integer. Number of decimal places. Default 2.

## Value

A list containing:

- `summary`: gt table with missing data summary

- `patterns`: Data frame of missing patterns

- `mcar`: MCAR test results (if requested)

- `plot`: Missing pattern plot (if requested)

## Examples

``` r
if (FALSE) { # \dontrun{
# Create data with missing values
df <- mtcars
df$mpg[c(1, 5, 10)] <- NA
df$hp[c(2, 5, 15)] <- NA

missing_analysis(df)
} # }
```
