# Convert Table to LaTeX Format

Converts a data frame or gt table to LaTeX tabular format. Works with
any tabular data structure.

## Usage

``` r
to_latex(
  x,
  title = NULL,
  label = NULL,
  digits = 2,
  align = "l",
  booktabs = TRUE,
  float = NULL,
  position = "htbp"
)
```

## Arguments

- x:

  A data frame, matrix, or gt table to convert.

- title:

  Optional title (added as caption).

- label:

  Optional LaTeX label for cross-referencing.

- digits:

  Number of decimal places for numeric columns. Default 2.

- align:

  Character vector of column alignments ("l", "c", "r") or single
  character to apply to all columns. Default "l" (left).

- booktabs:

  Logical. Use booktabs style (toprule, midrule, bottomrule)? Default
  TRUE for cleaner tables.

- float:

  Logical. Wrap in table environment with caption? Default TRUE if title
  is provided.

- position:

  Table float position (e.g., "htbp"). Default "htbp".

## Value

A character string containing the LaTeX code with class "latex_table"
for proper printing.

## Examples

``` r
if (FALSE) { # \dontrun{
# Convert a data frame to LaTeX
df <- data.frame(
  Variable = c("Age", "Score", "Income"),
  Mean = c(35.2, 78.5, 52000),
  SD = c(10.1, 12.3, 15000)
)
to_latex(df)
to_latex(df, title = "Descriptive Statistics", booktabs = TRUE)

# With custom alignment
to_latex(df, align = c("l", "r", "r"))
} # }
```
