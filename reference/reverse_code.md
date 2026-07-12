# Reverse Code Variables

Reverse codes numeric variables, commonly used for Likert scales where
some items are negatively worded. Uses the formula: reversed = (max +
min) - original.

## Usage

``` r
reverse_code(data, Vars, min = NULL, max = NULL, suffix = "_r")
```

## Arguments

- data:

  A data frame.

- Vars:

  Character vector of variable names to reverse code.

- min:

  Numeric. Minimum value of the scale. If NULL (default), detected from
  data.

- max:

  Numeric. Maximum value of the scale. If NULL (default), detected from
  data.

- suffix:

  Character. Suffix for new column names. Default "\_r".

## Value

Data frame with reverse coded variables added.

## Details

The reverse coding formula is: reversed = (max + min) - original

For a 1-5 Likert scale:

- 1 becomes 5

- 2 becomes 4

- 3 stays 3

- 4 becomes 2

- 5 becomes 1

If min/max are not specified, they are detected from the data. For
Likert scales, it's recommended to explicitly specify min and max to
ensure correct reversal even if extreme values are not present in the
data.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create sample Likert data
df <- data.frame(
  item1 = c(1, 2, 3, 4, 5),
  item2 = c(5, 4, 3, 2, 1),  # reverse-worded
  item3 = c(2, 3, 4, 3, 2)
)

# Reverse code with automatic detection
df <- reverse_code(df, Vars = "item2")

# Reverse code with explicit scale (1-5 Likert)
df <- reverse_code(df, Vars = "item2", min = 1, max = 5)

# Reverse code multiple items
df <- reverse_code(df, Vars = c("item2", "item3"), min = 1, max = 5)

# 0-10 scale
df <- reverse_code(df, Vars = "item2", min = 0, max = 10)
} # }
```
