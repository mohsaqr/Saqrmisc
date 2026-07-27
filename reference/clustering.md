# Model-Based Clustering Analysis

Performs comprehensive model-based clustering analysis using the
MoEClust package. Systematically tests multiple covariance models and
provides results on both original and scaled data scales for
interpretation.

## Usage

``` r
clustering(
  data,
  vars,
  n_clusters,
  scaling = "standardize",
  models = "all",
  verbose = TRUE,
  na_action = "omit"
)

cluster(
  data,
  vars,
  n_clusters,
  scaling = "standardize",
  models = "all",
  verbose = TRUE,
  na_action = "omit"
)
```

## Arguments

- data:

  A data frame containing the dataset.

- vars:

  A character vector of column names to use for clustering.

- n_clusters:

  An integer or vector specifying the number of clusters (G) to fit.

- scaling:

  Scaling method: "standardize" (z-score), "center" (mean only),
  "minmax" (0-1 range), or "none". Defaults to "standardize".

- models:

  Character vector of model names to test, or "all" for all 14 models.
  Valid models: EII, VII, EEI, VEI, EVI, VVI, EEE, EVE, VEE, VVE, EEV,
  VEV, EVV, VVV.

- verbose:

  Logical. If TRUE, prints progress messages. Defaults to TRUE.

- na_action:

  How to handle NAs: "omit" (remove rows) or "fail" (stop with error).
  Defaults to "omit".

## Value

An object of class "moe_analysis" containing:

- `models`: List of fitted models with results

- `data`: Original and scaled data used

- `parameters`: Analysis parameters

- `summary`: Summary statistics including best model

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with iris data
results <- clustering(
  data = iris,
  vars = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  n_clusters = 3
)

# Test specific models
results <- clustering(
  data = iris,
  vars = c("Sepal.Length", "Sepal.Width"),
  n_clusters = 2:4,
  models = c("EEE", "VVV", "VEV")
)

# View results using plot()
plot(results, type = "profile")
plot(results, type = "heatmap")
plot(results, type = "all")

# Get cluster assignments
data_clustered <- get_cluster_assignments(results)

# Assess stability
stability <- assess_cluster_stability(results)

# Generate report
generate_cluster_report(results)
} # }
```
