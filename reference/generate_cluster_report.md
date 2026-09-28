# Generate Cluster Report

Generate Cluster Report

## Usage

``` r
generate_cluster_report(
  results,
  output_format = "console",
  include_recommendations = TRUE
)

cluster_report(
  results,
  output_format = "console",
  include_recommendations = TRUE
)
```

## Arguments

- results:

  A \`saqr_clustering\` object.

- output_format:

  \`"console"\` (default), \`"gt"\`, or \`"markdown"\`.

- include_recommendations:

  Include interpretation guidelines.

## Value

Invisibly \`NULL\` for console; a gt table list for \`"gt"\`; a
character string for \`"markdown"\`.
