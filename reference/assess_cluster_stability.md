# Assess Cluster Stability via Bootstrap

Evaluates the stability of cluster assignments for a SINGLE selected
model using bootstrap resampling. For each bootstrap sample, the
clustering is re-run with the same model specification and assignments
are compared to the original using the Adjusted Rand Index.

Note: This function assesses one model at a time. If you tested multiple
models, use the model_name parameter to specify which model to assess,
or leave NULL to use the best model by BIC.

## Usage

``` r
assess_cluster_stability(
  results,
  model_name = NULL,
  n_boot = 100,
  verbose = TRUE,
  seed = NULL
)
```

## Arguments

- results:

  Object from clustering()

- model_name:

  Model to assess (single model). If NULL, uses best model by BIC.

- n_boot:

  Number of bootstrap iterations. Defaults to 100.

- verbose:

  Print progress. Defaults to TRUE.

- seed:

  Random seed for reproducibility. Defaults to NULL.

## Value

A list of class "cluster_stability" containing:

- `overall_stability`: Mean Adjusted Rand Index across bootstraps

- `stability_sd`: Standard deviation of ARI

- `bootstrap_ari`: Vector of ARI values for each bootstrap

- `observation_stability`: Proportion of times each observation was
  assigned to the same cluster as in the original

- `cluster_stability`: Stability score for each cluster

- `interpretation`: Text interpretation of stability

## Examples

``` r
if (FALSE) { # \dontrun{
results <- clustering(data, vars, n_clusters = 3)

# Assess stability of best model (default)
stability <- assess_cluster_stability(results, n_boot = 100)

# Assess a specific model
stability <- assess_cluster_stability(results, model_name = "VVV", n_boot = 50)

# View results
print(stability)
stability$overall_stability
stability$interpretation

# Plot observation stability
hist(stability$observation_stability, main = "Observation Stability")
} # }
```
