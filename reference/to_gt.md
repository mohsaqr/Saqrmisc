# Convert Table to GT Format

Converts a data frame or matrix to a gt table object for
publication-ready output. Provides a consistent interface matching other
to\_\* functions.

## Usage

``` r
to_gt(
  x,
  title = NULL,
  subtitle = NULL,
  digits = 2,
  rowname_col = NULL,
  theme = c("default", "fancy", "minimal", "dark", "none")
)
```

## Arguments

- x:

  A data frame or matrix to convert.

- title:

  Optional title for the table.

- subtitle:

  Optional subtitle for the table.

- digits:

  Number of decimal places for numeric columns. Default 2.

- rowname_col:

  Optional column name to use as row names.

- theme:

  Character. Visual theme: "default" (clean scientific style), "fancy"
  (blue headers), "minimal" (simple borders), "dark" (dark mode), or
  "none" (no styling). Default "default".

## Value

A gt table object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Convert a data frame to gt
df <- data.frame(
  Variable = c("Age", "Score"),
  Mean = c(35.2, 78.5),
  SD = c(10.1, 12.3)
)
to_gt(df)
to_gt(df, title = "Descriptive Statistics", theme = "fancy")

# With row names
to_gt(mtcars[1:5, 1:3], rowname_col = NULL)
} # }
```
