# Create Formatted Model Comparison Table

Create Formatted Model Comparison Table

## Usage

``` r
model_comparison_table(
  results,
  sort_by = "bic",
  top_n = NULL,
  highlight_best = TRUE
)

cluster_compare_table(
  results,
  sort_by = "bic",
  top_n = NULL,
  highlight_best = TRUE
)
```

## Arguments

- results:

  A \`saqr_clustering\` object.

- sort_by:

  Criterion to sort by: \`"bic"\` (default), \`"aic"\`.

- top_n:

  Number of top models to display. \`NULL\` shows all.

- highlight_best:

  Highlight the best model row.

## Value

A gt table object, or \`NULL\` if no enumeration was run.
