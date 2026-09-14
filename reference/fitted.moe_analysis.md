# Extract Fitted Clustering Data

Extract Fitted Clustering Data

## Usage

``` r
# S3 method for class 'moe_analysis'
fitted(object, model = NULL, probabilities = FALSE, ...)
```

## Arguments

- object:

  An moe_analysis object.

- model:

  Model name. If NULL, uses the best model by BIC.

- probabilities:

  Include cluster-membership probabilities and certainty.

- ...:

  Additional arguments (ignored).

## Value

A tibble containing the original data and a \`cluster\` column. Rows
omitted during fitting are retained with \`NA\` fitted values.
