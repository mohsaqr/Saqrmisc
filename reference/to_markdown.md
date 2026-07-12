# Convert Table to Markdown Format

Converts a data frame or gt table to markdown format. Works with any
tabular data structure.

## Usage

``` r
to_markdown(
  x,
  title = NULL,
  subtitle = NULL,
  digits = 2,
  align = "l",
  caption_style = c("header", "bold", "plain")
)
```

## Arguments

- x:

  A data frame, matrix, or gt table to convert.

- title:

  Optional title for the table.

- subtitle:

  Optional subtitle for the table.

- digits:

  Number of decimal places for numeric columns. Default 2.

- align:

  Character vector of column alignments ("l", "c", "r") or single
  character to apply to all columns. Default "l" (left).

- caption_style:

  Style for title: "header" (## Title), "bold" (\*\*Title\*\*), or
  "plain". Default "header".

## Value

A character string containing the markdown table with class
"markdown_table" for proper printing.

## Examples

``` r
if (FALSE) { # \dontrun{
# Convert a data frame to markdown
df <- data.frame(
  Name = c("Alice", "Bob", "Carol"),
  Score = c(85.5, 92.3, 78.9),
  Grade = c("B", "A", "C")
)
to_markdown(df)
to_markdown(df, title = "Student Scores", digits = 1)

# Works with gt tables too
library(gt)
gt_table <- gt(df)
to_markdown(gt_table)
} # }
```
