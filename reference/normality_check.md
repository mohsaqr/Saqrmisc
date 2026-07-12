# Check Normality of Variables

Tests and visualizes normality assumptions for numeric variables using
Shapiro-Wilk test, skewness, kurtosis, and Q-Q plots.

## Usage

``` r
normality_check(data, Vars, tests = TRUE, plots = TRUE, digits = 3)
```

## Arguments

- data:

  A data frame containing the variables.

- Vars:

  Character vector of numeric variable names to check.

- tests:

  Logical. Perform normality tests? Default TRUE.

- plots:

  Logical. Create Q-Q plots? Default TRUE.

- digits:

  Integer. Number of decimal places. Default 3.

## Value

A list containing:

- `table`: A gt table with normality statistics

- `plots`: Q-Q plots (if requested)

- `results`: Data frame with all statistics

## Examples

``` r
if (FALSE) { # \dontrun{
normality_check(mtcars, Vars = c("mpg", "hp", "wt"))
} # }
```
