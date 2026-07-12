# Quick Dataset Overview

Provides a comprehensive snapshot of a dataset including variable types,
missing data percentages, unique values, and basic statistics.

## Usage

``` r
data_overview(data, Vars = NULL, max_levels = 10, digits = 2)
```

## Arguments

- data:

  A data frame to summarize.

- Vars:

  Character vector of variable names. If NULL (default), all variables
  are included.

- max_levels:

  Integer. For categorical variables, show level counts if \<= this
  value. Default 10.

- digits:

  Integer. Number of decimal places for numeric summaries. Default 2.

## Value

A gt table with dataset overview.

## Examples

``` r
if (FALSE) { # \dontrun{
data_overview(mtcars)
data_overview(iris, Vars = c("Sepal.Length", "Species"))
} # }
```
