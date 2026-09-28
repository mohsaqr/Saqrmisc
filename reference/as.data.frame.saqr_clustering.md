# Extract Profile Means

Extract Profile Means

## Usage

``` r
# S3 method for class 'saqr_clustering'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A \`saqr_clustering\` object.

- row.names, optional:

  Ignored.

- ...:

  Ignored.

## Value

A data.frame with one row per profile and indicator, with columns
\`profile\`, \`indicator\`, \`mean\`, \`variance\`,
\`standard_deviation\`.
