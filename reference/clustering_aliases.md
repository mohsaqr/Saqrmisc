# Optional \`cluster\_\*\` aliases

These aliases provide a consistent naming family without replacing or
deprecating the original function names.

## Usage

``` r
cluster_fit(
  data,
  vars,
  n_clusters,
  scaling = "standardize",
  models = "all",
  verbose = TRUE,
  na_action = "omit"
)

cluster_models(results)

cluster_best(results, criterion = "bic", what = c("name", "result", "fit"))

cluster_compare(results, sort_by = "bic")

cluster_compare_table(
  results,
  sort_by = "bic",
  top_n = NULL,
  highlight_best = TRUE
)

cluster_assignments(
  results,
  model_name = NULL,
  include_probabilities = FALSE,
  cluster_col_name = "cluster"
)

cluster_view(
  results,
  what = "plots",
  scale = "original",
  model_name = "all",
  cluster_range = NULL,
  plot_type = "profile",
  verbose = FALSE,
  colors = NULL
)

cluster_plot_model(
  results,
  model,
  type = c("all", "profile", "heatmap", "barchart", "sizes"),
  scale = c("original", "scaled")
)

cluster_plot_best(
  results,
  type = c("all", "profile", "heatmap", "barchart", "sizes"),
  criterion = c("bic", "aic", "icl"),
  scale = c("original", "scaled")
)

cluster_stability(
  results,
  model_name = NULL,
  n_boot = 100,
  verbose = TRUE,
  seed = NULL
)

cluster_report(
  results,
  model_name = NULL,
  output_format = "console",
  include_recommendations = TRUE
)
```
