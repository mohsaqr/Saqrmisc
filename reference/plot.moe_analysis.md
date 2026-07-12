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
  "comparison", or "all" (displays all plot types)

- model:

  Model to plot. If NULL (default), uses best model by BIC.

- scale:

  Data scale for plots: "original" (default) or "scaled"

- ...:

  Additional arguments (currently ignored)

## Value

The plot object(s) invisibly. When type = "all", returns a list of all
generated plots.

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

# Specific model with scaled data
plot(results, type = "profile", model = "VVV", scale = "scaled")
} # }
```
