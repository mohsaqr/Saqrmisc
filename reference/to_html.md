# Convert Table to HTML Format

Converts a data frame or gt table to HTML table format. Works with any
tabular data structure.

## Usage

``` r
to_html(
  x,
  title = NULL,
  digits = 2,
  class = "table",
  id = NULL,
  border = 1,
  style = NULL,
  header_style = "font-weight: bold;",
  stripe = FALSE
)
```

## Arguments

- x:

  A data frame, matrix, or gt table to convert.

- title:

  Optional title (added as caption).

- digits:

  Number of decimal places for numeric columns. Default 2.

- class:

  CSS class(es) for the table element. Default "table".

- id:

  Optional HTML id attribute for the table.

- border:

  Table border width. Default 1.

- style:

  Additional inline CSS style for the table.

- header_style:

  CSS style for header cells. Default bold.

- stripe:

  Logical. Add striped rows? Default FALSE.

## Value

A character string containing the HTML code with class "html_table" for
proper printing.

## Examples

``` r
if (FALSE) { # \dontrun{
# Convert a data frame to HTML
df <- data.frame(
  Name = c("Alice", "Bob"),
  Score = c(85, 92)
)
to_html(df)
to_html(df, title = "Scores", class = "table table-striped")
} # }
```
