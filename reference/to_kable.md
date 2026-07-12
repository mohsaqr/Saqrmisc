# Convert Table to Kable Format

Converts a data frame or gt table to knitr::kable format for use in R
Markdown documents.

## Usage

``` r
to_kable(
  x,
  format = c("pipe", "markdown", "html", "latex", "rst"),
  digits = 2,
  caption = NULL,
  col.names = NULL,
  row.names = FALSE,
  align = NULL,
  ...
)
```

## Arguments

- x:

  A data frame, matrix, or gt table to convert.

- format:

  Output format: "markdown", "html", "latex", "rst", "pipe". Default
  "pipe" for GitHub-flavored markdown.

- digits:

  Number of decimal places for numeric columns. Default 2.

- caption:

  Optional table caption.

- col.names:

  Optional column names (overrides data frame names).

- row.names:

  Logical. Include row names? Default FALSE.

- align:

  Column alignment: "l" (left), "c" (center), "r" (right), or a vector
  of alignments.

- ...:

  Additional arguments passed to knitr::kable.

## Value

A knitr_kable object.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(A = 1:3, B = c("x", "y", "z"))
to_kable(df)
to_kable(df, format = "html", caption = "My Table")
} # }
```
