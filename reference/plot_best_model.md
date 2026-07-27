# Plot the Best Fitted Clustering Model

Plot the Best Fitted Clustering Model

## Usage

``` r
plot_best_model(
  results,
  type = c("all", "profile", "heatmap", "barchart", "sizes"),
  criterion = c("bic", "aic", "icl"),
  scale = c("original", "scaled")
)
```

## Arguments

- results:

  An moe_analysis object.

- type:

  Plot type: "profile", "heatmap", "barchart", "sizes", or "all".

- criterion:

  Criterion used to select the best model.

- scale:

  Data scale: "original" or "scaled".

## Value

A ggplot object, or a named list of four ggplot objects when
`type = "all"`.
