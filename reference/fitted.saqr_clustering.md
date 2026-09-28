# Extract Fitted Observations with Profile Assignments

Extract Fitted Observations with Profile Assignments

## Usage

``` r
# S3 method for class 'saqr_clustering'
fitted(object, ...)
```

## Arguments

- object:

  A \`saqr_clustering\` object.

- ...:

  Ignored.

## Value

A data.frame with all columns of the input data plus \`profile\`,
\`uncertainty\`, and posterior probability columns. Rows removed during
fitting get \`NA\` fitted values.
