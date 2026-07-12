# Create a Publication-Ready Correlation Matrix

Creates a formatted correlation matrix with significance stars, optional
confidence intervals, and heatmap visualization. Supports bivariate
(zero-order), partial, and semi-partial correlations.

## Usage

``` r
correlation_matrix(
  data,
  Vars = NULL,
  type = c("bivariate", "partial", "semi-partial"),
  method = c("pearson", "spearman", "kendall"),
  triangle = c("lower", "upper", "full"),
  diagonal = c("dash", "one", "names"),
  show_n = FALSE,
  show_ci = FALSE,
  show_p = FALSE,
  p_adjust = c("none", "bonferroni", "holm", "fdr"),
  stars = TRUE,
  heatmap = FALSE,
  digits = 2,
  title = NULL,
  use = c("pairwise", "complete"),
  format = c("gt", "plain", "markdown", "latex", "kable"),
  show_header = TRUE,
  interpret = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame containing the variables to correlate.

- Vars:

  Character vector of variable names to include in the correlation
  matrix. If NULL (default), all numeric variables in the data frame are
  used.

- type:

  Type of correlation: "bivariate" (default, zero-order), "partial", or
  "semi-partial". For partial/semi-partial, each pair is controlled for
  all other variables in Vars.

- method:

  Correlation method: "pearson" (default), "spearman", or "kendall".

- triangle:

  Which triangle to display: "lower" (default), "upper", or "full".

- diagonal:

  What to show on the diagonal: "dash" (default), "one", or "names".

- show_n:

  Logical. Show pairwise sample sizes? Default FALSE.

- show_ci:

  Logical. Show 95 percent confidence intervals? Default FALSE. Only
  available for bivariate Pearson.

- show_p:

  Logical. Show p-values below correlations? Default FALSE.

- p_adjust:

  Method for p-value adjustment: "none" (default), "bonferroni", "holm",
  "fdr".

- stars:

  Logical. Show significance stars? Default TRUE.

- heatmap:

  Logical. Create a heatmap visualization? Default FALSE.

- digits:

  Number of decimal places. Default 2.

- title:

  Optional title for the table.

- use:

  Method for handling missing data: "pairwise" (default) or "complete".
  Note: partial and semi-partial correlations always use complete cases.

- format:

  Character. Output format: "gt" (default, publication-ready gt table),
  "plain" (data frame), "markdown", "latex", or "kable".

- show_header:

  Logical. Show title header? Default: \`TRUE\`. Set to \`FALSE\` to
  hide the table header.

- interpret:

  Logical. Pass results to AI for automatic interpretation? Default
  FALSE. When TRUE, generates clean Methods and Results text using AI.
  Requires API key setup (see
  [`set_api_key`](https://pak.dynasite.org/Saqrmisc/reference/set_api_key.md)).

- ...:

  Additional arguments passed to
  [`pass`](https://pak.dynasite.org/Saqrmisc/reference/pass.md) when
  interpret = TRUE (e.g., provider, model, context, append_prompt).

## Value

A list containing: table (gt table with formatted correlations),
correlation_matrix (numeric matrix), p_matrix (matrix of p-values),
n_matrix (pairwise sample sizes), and heatmap (ggplot if requested).

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic correlation matrix
correlation_matrix(
  data = mtcars,
  Vars = c("mpg", "cyl", "disp", "hp")
)

# Partial correlations (controlling for other variables)
correlation_matrix(
  data = mtcars,
  Vars = c("mpg", "cyl", "disp", "hp"),
  type = "partial",
  title = "Partial Correlations"
)

# Semi-partial correlations
correlation_matrix(
  data = mtcars,
  Vars = c("mpg", "cyl", "disp", "hp"),
  type = "semi-partial"
)

# With confidence intervals and heatmap
correlation_matrix(
  data = mtcars,
  Vars = c("mpg", "cyl", "disp", "hp"),
  show_ci = TRUE,
  heatmap = TRUE,
  title = "Motor Trend Car Correlations"
)

# Spearman correlations with Bonferroni correction
correlation_matrix(
  data = mtcars,
  Vars = c("mpg", "cyl", "disp"),
  method = "spearman",
  p_adjust = "bonferroni"
)
} # }
```
