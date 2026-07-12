# Add Row Totals to Data Frame

Computes row totals across specified columns and adds them as a new
column.

## Usage

``` r
add_total(data, cols = NULL, name = "total", na.rm = TRUE)
```

## Arguments

- data:

  A data frame.

- cols:

  Column specification. Can be:

  - NULL (default): uses all numeric columns

  - Character vector: column names, e.g., \`c("a", "b", "c")\`

  - Numeric vector: column indices, e.g., \`2:5\` or \`c(2,3,4)\`

  - Single number: from that column to end, e.g., \`3\` means cols 3 to
    last

- name:

  Character. Name for the total column. Default: \`"total"\`.

- na.rm:

  Logical. Remove NA values when summing? Default: \`TRUE\`.

## Value

The original data frame with an added total column.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(id = 1:3, a = c(1,2,3), b = c(4,5,6), c = c(7,8,9))

# All numeric columns
add_total(df)

# Specific columns by name
add_total(df, cols = c("a", "b"))

# Columns by index range
add_total(df, cols = 2:4)

# From column 2 to end
add_total(df, cols = 2)

# Custom name
add_total(df, cols = c("a", "b"), name = "sum_ab")
} # }
```
