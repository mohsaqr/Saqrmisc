# Check for Outliers

Detects univariate and multivariate outliers using various methods
including z-score (2SD, 3SD, etc.), IQR, percentile, or Mahalanobis
distance.

## Usage

``` r
outlier_check(
  data,
  Vars,
  method = c("zscore", "iqr", "percentile", "mahalanobis"),
  threshold = NULL,
  flag = TRUE,
  plot = TRUE
)
```

## Arguments

- data:

  A data frame containing the variables.

- Vars:

  Character vector of numeric variable names to check.

- method:

  Detection method: "zscore", "iqr", "percentile", or "mahalanobis".

- threshold:

  Numeric threshold for outlier detection. For zscore: number of SDs
  (default 3). Common values: 2, 2.5, 3, 3.29. For iqr: IQR multiplier
  (default 1.5). Common values: 1.5, 3. For percentile: percentile
  cutoff (default 0.01 for 1st/99th). Values like 0.05 for 5th/95th. For
  mahalanobis: chi-sq p-value threshold (default 0.001).

- flag:

  Logical. Add outlier flag column to returned data? Default TRUE.

- plot:

  Logical. Create outlier visualization? Default TRUE.

## Value

A list containing:

- `summary`: gt table with outlier summary

- `data`: Data frame with outlier flags (if flag = TRUE)

- `outlier_indices`: Row indices of outliers

- `plot`: Visualization (if requested)

## Examples

``` r
if (FALSE) { # \dontrun{
# Z-score with 3 SD threshold
outlier_check(mtcars, Vars = c("mpg", "hp"), method = "zscore", threshold = 3)

# Z-score with 2 SD threshold (more conservative)
outlier_check(mtcars, Vars = c("mpg", "hp"), method = "zscore", threshold = 2)

# IQR method
outlier_check(mtcars, Vars = c("mpg", "hp"), method = "iqr", threshold = 1.5)

# Percentile method (1st and 99th percentile)
outlier_check(mtcars, Vars = c("mpg", "hp"), method = "percentile", threshold = 0.01)

# Percentile method (5th and 95th percentile)
outlier_check(mtcars, Vars = c("mpg", "hp"), method = "percentile", threshold = 0.05)

# Mahalanobis distance (multivariate)
outlier_check(mtcars, Vars = c("mpg", "hp", "wt"), method = "mahalanobis")
} # }
```
