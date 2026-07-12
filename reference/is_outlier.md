# Check if Value is Outlier (vectorized for mutate/across)

Vectorized function to detect outliers for use with dplyr. For
group-wise detection, use dplyr::group_by() before mutate().

## Usage

``` r
is_outlier(
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

Logical vector indicating outliers.

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

# Flag outliers beyond 2 SD
mtcars %>% mutate(hp_outlier = is_outlier(hp, method = "zscore", threshold = 2))

# Flag outliers at 5th/95th percentile
mtcars %>% mutate(hp_outlier = is_outlier(hp, method = "percentile", threshold = 0.05))

# Group-wise outlier detection
mtcars %>% group_by(cyl) %>% mutate(hp_outlier = is_outlier(hp))
} # }
```
