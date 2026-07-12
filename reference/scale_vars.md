# Scale Variables

Scales numeric variables by dividing by standard deviation or rescaling
to a specified range. Supports group-wise scaling.

## Usage

``` r
scale_vars(
  data,
  Vars,
  method = c("sd", "range"),
  range = c(0, 1),
  suffix = "_s",
  group_by = NULL
)
```

## Arguments

- data:

  A data frame.

- Vars:

  Character vector of numeric variable names to scale.

- method:

  Scaling method: "sd" (divide by SD) or "range" (min-max scaling).

- range:

  Numeric vector of length 2 specifying target range for "range" method.
  Default c(0, 1). Use c(1, 10) for 1-10 scaling.

- suffix:

  Character. Suffix for new column names. Default "\_s".

- group_by:

  Character. Name of grouping variable for group-wise scaling. Optional.

## Value

Data frame with scaled variables added.

## Examples

``` r
if (FALSE) { # \dontrun{
# Scale by SD
df <- scale_vars(mtcars, Vars = c("mpg", "hp"), method = "sd")

# Min-max scaling (0-1)
df <- scale_vars(mtcars, Vars = "mpg", method = "range")

# Scale to 1-10 range
df <- scale_vars(mtcars, Vars = "mpg", method = "range", range = c(1, 10))

# Group-wise scaling
df <- scale_vars(mtcars, Vars = "mpg", method = "sd", group_by = "cyl")
} # }
```
