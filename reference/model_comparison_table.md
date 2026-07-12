# Create Formatted Model Comparison Table

Generates a publication-ready gt table comparing all fitted models by
BIC, AIC, ICL, and log-likelihood. Highlights the best model and
provides interpretation guidance.

## Usage

``` r
model_comparison_table(
  results,
  sort_by = "bic",
  top_n = NULL,
  highlight_best = TRUE
)
```

## Arguments

- results:

  Object from clustering()

- sort_by:

  Criterion to sort by: "bic" (default), "aic", or "icl"

- top_n:

  Number of top models to display. NULL shows all. Defaults to NULL.

- highlight_best:

  Logical. Highlight the best model row. Defaults to TRUE.

## Value

A gt table object

## Examples

``` r
if (FALSE) { # \dontrun{
results <- clustering(data, vars, n_clusters = 3)
model_comparison_table(results)
model_comparison_table(results, sort_by = "aic", top_n = 5)
} # }
```
