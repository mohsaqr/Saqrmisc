# Compare Models from an Enumeration

Compare Models from an Enumeration

## Usage

``` r
compare_models(results, sort_by = "bic")

cluster_compare(results, sort_by = "bic")
```

## Arguments

- results:

  A \`saqr_clustering\` object.

- sort_by:

  Criterion to sort by: \`"bic"\` (default), \`"aic"\`, or \`"icl"\`.

## Value

A data.frame, or \`NULL\` if no enumeration was run.
