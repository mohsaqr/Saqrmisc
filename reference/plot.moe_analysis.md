# Plot method for moe_analysis objects

Display visualizations from the clustering analysis. Supports multiple
plot types including profile plots, heatmaps, bar charts, cluster sizes,
and model comparison charts.

## Usage

``` r
# S3 method for class 'moe_analysis'
plot(x, type = "profile", model = NULL, scale = "original", ...)
```

## Arguments

- x:

  An moe_analysis object

- type:

  Type of plot: "profile" (default), "heatmap", "barchart", "sizes",
  "bic", "aic", "icl", "comparison" (all three information criteria as
  separate plots), or "all" (all plot types for every fitted model, plus
  all model-comparison plots)

- model:

  Model to plot. If NULL (default), uses best model by BIC.

- scale:

  Data scale for plots: "original" (default) or "scaled"

- ...:

  Additional arguments (currently ignored)

## Value

The plot object(s) invisibly. `type = "comparison"` returns the three
criterion plots. `type = "all"` returns a nested list containing every
fitted model's plots and all comparison plots.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- clustering(data, vars, n_clusters = 3)

# Profile plot (default)
plot(results)

# Heatmap
plot(results, type = "heatmap")

# All plot types
plot(results, type = "all")

# Information criteria, separately
plot(results, type = "bic")
plot(results, type = "aic")
plot(results, type = "icl")

# Specific model with scaled data
plot(results, type = "profile", model = "VVV", scale = "scaled")
} # }
```
