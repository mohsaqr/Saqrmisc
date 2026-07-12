# Replace Missing Values

Imputes missing values using various methods including mean, median,
mode, or custom values.

## Usage

``` r
replace_missing(
  data,
  Vars,
  method = c("mean", "median", "mode", "zero", "min", "max", "value"),
  value = NULL,
  group_by = NULL,
  suffix = NULL
)
```

## Arguments

- data:

  A data frame.

- Vars:

  Character vector of variable names to impute.

- method:

  Imputation method: "mean", "median", "mode", "zero", "min", "max", or
  "value".

- value:

  Custom value to use when method = "value".

- group_by:

  Character. Name of grouping variable for group-wise imputation.
  Optional.

- suffix:

  Character. Suffix for new columns. If NULL (default), replaces in
  place.

## Value

Data frame with imputed values.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- mtcars
df$mpg[c(1, 5, 10)] <- NA

# Replace with mean
df_imputed <- replace_missing(df, Vars = "mpg", method = "mean")

# Replace with median, create new column
df_imputed <- replace_missing(df, Vars = "mpg", method = "median", suffix = "_imp")

# Group-wise mean imputation
df_imputed <- replace_missing(df, Vars = "mpg", method = "mean", group_by = "cyl")
} # }
```
