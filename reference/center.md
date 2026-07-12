# Center Variables (Mean-Centering)

Centers numeric variables by subtracting the mean. Supports group-wise
centering for multilevel/nested data. Works with dplyr mutate/across.

## Usage

``` r
center(data, Vars, suffix = "_c", group_by = NULL)
```

## Arguments

- data:

  A data frame (or vector if used in mutate).

- Vars:

  Character vector of numeric variable names to center. Not needed if
  used in mutate.

- suffix:

  Character. Suffix for new column names. Default "\_c".

- group_by:

  Character. Name of grouping variable for group-wise centering.
  Optional.

## Value

Data frame with centered variables added, or vector if input is vector.

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple mean centering
df <- center(mtcars, Vars = c("mpg", "hp"))

# Group-mean centering
df <- center(mtcars, Vars = "mpg", group_by = "cyl")

# With dplyr
library(dplyr)
mtcars %>% mutate(mpg_c = center_vec(mpg))
mtcars %>% mutate(across(c(mpg, hp), center_vec, .names = "{.col}_c"))

# Group-wise with dplyr
mtcars %>% group_by(cyl) %>% mutate(mpg_c = center_vec(mpg))
} # }
```
