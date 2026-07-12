# Standardize a Vector (for use in mutate/across)

Vectorized standardization function for use with dplyr mutate and
across. For group-wise standardization, use dplyr::group_by() before
mutate().

## Usage

``` r
standardize_vec(x, na.rm = TRUE)
```

## Arguments

- x:

  Numeric vector to standardize.

- na.rm:

  Logical. Remove NA values? Default TRUE.

## Value

Standardized numeric vector (mean = 0, SD = 1).

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

# Single variable
mtcars %>% mutate(mpg_z = standardize_vec(mpg))

# Multiple variables
mtcars %>% mutate(across(c(mpg, hp, wt), standardize_vec, .names = "{.col}_z"))

# Group-wise standardization
mtcars %>% group_by(cyl) %>% mutate(mpg_z = standardize_vec(mpg))
} # }
```
