# Generate Interpretable Cluster Report

Creates a comprehensive, interpretable report of the clustering results
including model selection summary, cluster profiles, and practical
interpretations.

## Usage

``` r
generate_cluster_report(
  results,
  model_name = NULL,
  output_format = "console",
  include_recommendations = TRUE
)
```

## Arguments

- results:

  Object from clustering()

- model_name:

  Model to report on. If NULL, uses best model by BIC.

- output_format:

  Output format: "console" (default), "gt" (returns gt tables), or
  "markdown" (returns markdown text).

- include_recommendations:

  Logical. Include interpretation guidelines. Defaults to TRUE.

## Value

Depending on output_format:

- "console": Prints report and returns NULL invisibly

- "gt": Returns a list of gt table objects

- "markdown": Returns markdown text as character string

## Examples

``` r
if (FALSE) { # \dontrun{
results <- clustering(data, vars, n_clusters = 3)

# Print to console
generate_cluster_report(results)

# Get gt tables
tables <- generate_cluster_report(results, output_format = "gt")

# Get markdown
md_text <- generate_cluster_report(results, output_format = "markdown")
} # }
```
