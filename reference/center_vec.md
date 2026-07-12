# Center a Vector (for use in mutate/across)

Vectorized centering function for use with dplyr mutate and across. For
group-wise centering, use dplyr::group_by() before mutate().

## Usage

``` r
center_vec(x, na.rm = TRUE)
```

## Arguments

- x:

  Numeric vector to center.

- na.rm:

  Logical. Remove NA values when computing mean? Default TRUE.

## Value

Centered numeric vector.

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

# Single variable
mtcars %>% mutate(mpg_c = center_vec(mpg))

# Multiple variables
mtcars %>% mutate(across(c(mpg, hp, wt), center_vec, .names = "{.col}_c"))

# Group-wise centering
mtcars %>% group_by(cyl) %>% mutate(mpg_c = center_vec(mpg))
} # }
```
