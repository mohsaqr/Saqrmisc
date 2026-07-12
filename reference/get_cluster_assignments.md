# Get Cluster Assignments with Original Data

Extracts cluster assignments from a fitted model and adds them to the
original data frame. Optionally includes cluster membership
probabilities.

## Usage

``` r
get_cluster_assignments(
  results,
  model_name = NULL,
  include_probabilities = FALSE,
  cluster_col_name = "cluster"
)
```

## Arguments

- results:

  Object from clustering()

- model_name:

  Name of the model to use. If NULL (default), uses the best model by
  BIC.

- include_probabilities:

  Logical. If TRUE, includes probability of membership for each cluster.
  Defaults to FALSE.

- cluster_col_name:

  Name for the cluster assignment column. Defaults to "cluster".

## Value

A data frame containing the original data plus cluster assignments (and
optionally probabilities).

## Examples

``` r
if (FALSE) { # \dontrun{
results <- clustering(data, vars, n_clusters = 3)

# Get data with cluster assignments
data_with_clusters <- get_cluster_assignments(results)

# Specify a particular model
data_with_clusters <- get_cluster_assignments(results, model_name = "VVV")

# Include membership probabilities
data_with_clusters <- get_cluster_assignments(results, include_probabilities = TRUE)
} # }
```
