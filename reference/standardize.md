# Standardize Variables (Z-Score)

Standardizes numeric variables to z-scores (mean = 0, SD = 1). Supports
group-wise standardization for multilevel/nested data.

## Usage

``` r
standardize(data, Vars, suffix = "_z", group_by = NULL)
```

## Arguments

- data:

  A data frame.

- Vars:

  Character vector of numeric variable names to standardize.

- suffix:

  Character. Suffix for new column names. Default "\_z".

- group_by:

  Character. Name of grouping variable for group-wise standardization.
  Optional.

## Value

Data frame with standardized variables added.

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple standardization
df <- standardize(mtcars, Vars = c("mpg", "hp"))

# Group-wise standardization
df <- standardize(mtcars, Vars = "mpg", group_by = "cyl")
} # }
```
