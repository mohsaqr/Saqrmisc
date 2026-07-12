# Convert Any Table to Data Frame

Extracts the underlying data frame from various table objects including
gt tables, kable objects, and matrices.

## Usage

``` r
to_dataframe(x, digits = NULL)
```

## Arguments

- x:

  A table object (gt, kable, matrix, or data frame).

- digits:

  Number of decimal places for formatting numeric columns. Set to NULL
  to skip formatting. Default NULL.

## Value

A data frame.

## Examples

``` r
if (FALSE) { # \dontrun{
# Extract data from a gt table
library(gt)
gt_table <- gt(mtcars[1:5, 1:3])
df <- to_dataframe(gt_table)

# Works with matrices too
mat <- matrix(1:6, nrow = 2)
to_dataframe(mat)
} # }
```
