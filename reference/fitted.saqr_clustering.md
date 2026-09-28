# Input Data with Profile Assignments

Input Data with Profile Assignments

## Usage

``` r
# S3 method for class 'saqr_clustering'
fitted(object, ...)
```

## Arguments

- object:

  A \`saqr_clustering\` object from \[clustering()\].

- ...:

  Ignored.

## Value

The data frame passed to \[clustering()\], one row per input row, with
\`profile\` (modal assignment), \`uncertainty\` and one
\`posterior_profile\_\<k\>\` column per profile added. Rows dropped for
missing values carry \`NA\` in the added columns.
