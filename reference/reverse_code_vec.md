# Reverse Code a Vector (for use in mutate/across)

Vectorized reverse coding for use with dplyr mutate and across.

## Usage

``` r
reverse_code_vec(x, min = NULL, max = NULL, na.rm = TRUE)
```

## Arguments

- x:

  Numeric vector to reverse code.

- min:

  Numeric. Minimum value of the scale. If NULL (default), detected from
  data.

- max:

  Numeric. Maximum value of the scale. If NULL (default), detected from
  data.

- na.rm:

  Logical. Remove NA values when detecting min/max? Default TRUE.

## Value

Reverse coded numeric vector.

## Details

The reverse coding formula is: reversed = (max + min) - original

For Likert scales, always specify min and max explicitly to ensure
correct reversal even when extreme values are not present in the data.

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

df <- data.frame(
  item1 = c(1, 2, 3, 4, 5),
  item2 = c(5, 4, 3, 2, 1),
  item3 = c(2, 3, 4, 3, 2)
)

# Reverse single variable (auto-detect scale)
df %>% mutate(item2_r = reverse_code_vec(item2))

# Reverse with explicit 1-5 scale
df %>% mutate(item2_r = reverse_code_vec(item2, min = 1, max = 5))

# Reverse multiple variables
df %>% mutate(across(c(item2, item3), ~reverse_code_vec(.x, min = 1, max = 5), .names = "{.col}_r"))

# 0-10 scale
df %>% mutate(item2_r = reverse_code_vec(item2, min = 0, max = 10))

# 1-7 Likert scale
df %>% mutate(item2_r = reverse_code_vec(item2, min = 1, max = 7))
} # }
```
