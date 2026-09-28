# Get Cluster Assignments with Original Data

Get Cluster Assignments with Original Data

## Usage

``` r
get_cluster_assignments(
  results,
  model_name = NULL,
  include_probabilities = FALSE,
  cluster_col_name = "cluster"
)

cluster_assignments(
  results,
  model_name = NULL,
  include_probabilities = FALSE,
  cluster_col_name = "cluster"
)
```

## Arguments

- results:

  A \`saqr_clustering\` object.

- model_name:

  Ignored (kept for backward compatibility).

- include_probabilities:

  Logical. Include posterior probabilities.

- cluster_col_name:

  Name for the assignment column.

## Value

A data.frame with the original data plus a cluster/profile column.
