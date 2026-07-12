# Replace Outliers

Replaces detected outliers with specified values such as NA, mean,
median, or winsorized values. Supports multiple detection methods.

## Usage

``` r
replace_outliers(
  data,
  Vars,
  detect = c("zscore", "iqr", "percentile"),
  threshold = NULL,
  replace_with = c("NA", "mean", "median", "winsorize", "boundary"),
  suffix = NULL
)
```

## Arguments

- data:

  A data frame.

- Vars:

  Character vector of numeric variable names to process.

- detect:

  Detection method: "zscore", "iqr", or "percentile".

- threshold:

  Numeric threshold for detection. For zscore: number of SDs (default
  3). Use 2 for 2SD, 2.5 for 2.5SD, etc. For iqr: IQR multiplier
  (default 1.5). For percentile: percentile cutoff (default 0.01 for
  1st/99th).

- replace_with:

  Replacement method: "NA", "mean", "median", "winsorize", or
  "boundary".

- suffix:

  Character. Suffix for new columns. If NULL (default), replaces in
  place.

## Value

Data frame with outliers replaced.

## Details

Detection methods:

- `"zscore"`: Values beyond threshold SDs from mean. Common thresholds:
  2, 2.5, 3, 3.29

- `"iqr"`: Values beyond Q1/Q3 +/- threshold\*IQR. Common thresholds:
  1.5, 3

- `"percentile"`: Values below or above percentile cutoffs. E.g., 0.01 =
  1st/99th, 0.05 = 5th/95th

Replacement methods:

- `"NA"`: Set outliers to NA

- `"mean"`: Replace with mean of non-outliers

- `"median"`: Replace with median of non-outliers

- `"winsorize"`: Cap at threshold boundary

- `"boundary"`: Replace with nearest non-outlier value

## Examples

``` r
if (FALSE) { # \dontrun{
df <- mtcars

# Replace outliers beyond 2 SD with NA
df_clean <- replace_outliers(df, Vars = "hp",
                             detect = "zscore", threshold = 2,
                             replace_with = "NA")

# Winsorize at 3 SD
df_clean <- replace_outliers(df, Vars = "hp",
                             detect = "zscore", threshold = 3,
                             replace_with = "winsorize")

# Replace outliers at 5th/95th percentile with median
df_clean <- replace_outliers(df, Vars = "hp",
                             detect = "percentile", threshold = 0.05,
                             replace_with = "median")

# IQR-based winsorization
df_clean <- replace_outliers(df, Vars = c("mpg", "hp"),
                             detect = "iqr", replace_with = "winsorize")
} # }
```
