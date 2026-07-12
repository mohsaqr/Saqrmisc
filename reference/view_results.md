# View Results from MoEClust Analysis (Deprecated)

Display plots or tables from the analysis results. Supports multiple
visualization types including profile plots, heatmaps, bar charts, and
cluster size charts.

Note: Consider using the S3 plot() method instead: plot(results, type =
"...")

## Usage

``` r
view_results(
  results,
  what = "plots",
  scale = "original",
  model_name = "all",
  cluster_range = NULL,
  plot_type = "profile",
  verbose = FALSE,
  colors = NULL
)
```

## Arguments

- results:

  Object from clustering()

- what:

  What to display: "plots", "tables", "heatmap", "barchart",
  "cluster_sizes", or "all"

- scale:

  Data scale: "original" or "scaled"

- model_name:

  Specific model(s) to view, or "all"

- cluster_range:

  Optional cluster range to filter

- plot_type:

  Plot type: "profile", "heatmap", "barchart", "cluster_sizes", "all"

- verbose:

  Print headers and metadata

- colors:

  Optional custom colors for plots

## Value

NULL (invisibly). Used for side effects.
