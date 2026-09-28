# Get Best Model Name

Get Best Model Name

## Usage

``` r
get_best_model(results, criterion = "bic", what = c("name", "fit"))

cluster_best(results, criterion = "bic", what = c("name", "fit"))
```

## Arguments

- results:

  A \`saqr_clustering\` object.

- criterion:

  Selection criterion.

- what:

  What to return: \`"name"\` or \`"fit"\`.

## Value

A character string (model name) or a \`multilpa\` fit.
