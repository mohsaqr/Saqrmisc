# Winsorize a Vector (vectorized for mutate/across)

Vectorized winsorization for use with dplyr. For group-wise
winsorization, use dplyr::group_by() before mutate().

## Usage

``` r
winsorize_vec(
  x,
  method = c("zscore", "iqr", "percentile"),
  threshold = NULL,
  na.rm = TRUE
)
```

## Arguments

- x:

  Numeric vector.

- method:

  Detection method: "zscore", "iqr", or "percentile".

- threshold:

  Threshold value.

- na.rm:

  Logical. Remove NA values? Default TRUE.

## Value

Winsorized numeric vector.

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

# Winsorize at 3 SD
mtcars %>% mutate(hp_w = winsorize_vec(hp, method = "zscore", threshold = 3))

# Winsorize at 5th/95th percentile
mtcars %>% mutate(hp_w = winsorize_vec(hp, method = "percentile", threshold = 0.05))

# Group-wise winsorization
mtcars %>% group_by(cyl) %>% mutate(hp_w = winsorize_vec(hp))
} # }
```
