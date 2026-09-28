# Compare the Candidate Models of a Clustering Result

Compare the Candidate Models of a Clustering Result

## Usage

``` r
# S3 method for class 'saqr_clustering'
summary(object, ...)
```

## Arguments

- object:

  A \`saqr_clustering\` object from \[clustering()\].

- ...:

  Ignored.

## Value

A data.frame with one row per candidate (profiles x covariance model):
\`n_profiles\`, \`model\`, \`log_likelihood\`, \`n_parameters\`,
\`aic\`, \`bic\`, \`icl\`, \`entropy\` (relative), \`converged\`,
\`boundary\`, \`delta_bic\` (distance from the selected model) and
\`selected\`.
