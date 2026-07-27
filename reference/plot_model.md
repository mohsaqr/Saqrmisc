# Plot One Fitted Clustering Model

Plot One Fitted Clustering Model

## Usage

``` r
plot_model(
  results,
  model,
  type = c("all", "profile", "heatmap", "barchart", "sizes"),
  scale = c("original", "scaled")
)
```

## Arguments

- results:

  An moe_analysis object.

- model:

  Name of the fitted model.

- type:

  Plot type: "profile", "heatmap", "barchart", "sizes", or "all".

- scale:

  Data scale: "original" or "scaled".

## Value

A ggplot object, or a named list of four ggplot objects when
`type = "all"`.
