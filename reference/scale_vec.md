# Scale a Vector (for use in mutate/across)

Vectorized scaling function for use with dplyr mutate and across. For
group-wise scaling, use dplyr::group_by() before mutate().

## Usage

``` r
scale_vec(x, method = c("sd", "range"), range = c(0, 1), na.rm = TRUE)
```

## Arguments

- x:

  Numeric vector to scale.

- method:

  Scaling method: "sd" or "range".

- range:

  Target range for "range" method. Default c(0, 1).

- na.rm:

  Logical. Remove NA values? Default TRUE.

## Value

Scaled numeric vector.

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

# Scale by SD
mtcars %>% mutate(mpg_s = scale_vec(mpg, method = "sd"))

# Scale to 1-10
mtcars %>% mutate(mpg_s = scale_vec(mpg, method = "range", range = c(1, 10)))

# Multiple variables to 0-100
mtcars %>% mutate(across(c(mpg, hp), ~scale_vec(.x, method = "range", range = c(0, 100))))

# Group-wise scaling
mtcars %>% group_by(cyl) %>% mutate(mpg_s = scale_vec(mpg, method = "sd"))
} # }
```
