# Plot method for moe_analysis objects

Draws plots of a
[`clustering()`](https://pak.dynasite.org/Saqrmisc/reference/clustering.md)
result. This method is the drawing front end for
[`plot_clustering()`](https://pak.dynasite.org/Saqrmisc/reference/plot_clustering.md):
it accepts the same `type`, `model`, and `scale` arguments, prints the
plots, and returns them invisibly. See
[`clustering_plot_types()`](https://pak.dynasite.org/Saqrmisc/reference/clustering_plot_types.md)
for the catalogue of plot types and groups.

## Usage

``` r
# S3 method for class 'moe_analysis'
plot(x, type = "clusters", model = NULL, scale = c("original", "scaled"), ...)
```

## Arguments

- x:

  An moe_analysis object returned by
  [`clustering()`](https://pak.dynasite.org/Saqrmisc/reference/clustering.md).

- type:

  Character vector of plot types and/or groups; see Description.
  Defaults to `"clusters"`.

- model:

  Which fitted model(s) to describe: `NULL` (default), one or more model
  names, or `"all"` for every fitted model. `NULL` means the best model
  by BIC, except with `type = "all"`, where it means every model.
  Selection plots ignore `model`.

- scale:

  Data scale for `"profile"`, `"heatmap"`, and `"distribution"`:
  `"original"` (default) or `"scaled"`. Diagnostics always use the data
  as fitted.

- ...:

  Ignored.

## Value

The value of
[`plot_clustering()`](https://pak.dynasite.org/Saqrmisc/reference/plot_clustering.md),
invisibly: a single ggplot when `type` resolves to one plot, otherwise a
`clustering_plots` object. Raises the same classed errors as
[`plot_clustering()`](https://pak.dynasite.org/Saqrmisc/reference/plot_clustering.md).

## Examples

``` r
# \donttest{
fit <- clustering(
  iris,
  vars = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  n_clusters = 2:3,
  models = c("EII", "EEE"),
  verbose = FALSE
)

plot(fit)                          # the cluster plots




plot(fit, type = "diagnostics")



plot(fit, type = "selection")



plot(fit, type = "heatmap", scale = "scaled")

plot(fit, type = "profile", model = "all")  # one plot type, every model




plot(fit, type = "all")                    # every plot, every model































# }
```
