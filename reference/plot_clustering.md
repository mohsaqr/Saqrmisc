# Plot Clustering Results

Draws plots of a
[`clustering()`](https://pak.dynasite.org/Saqrmisc/reference/clustering.md)
result by delegating to the latents package's `plot.multilpa()` method.
Plots are organised into three groups:

- `"clusters"` — profile shape: `"profiles"`, `"bars"`, `"heatmap"`,
  `"raincloud"`, `"sizes"`.

- `"diagnostics"` — classification quality: `"entropy"`, `"posteriors"`,
  `"avepp"`.

- `"selection"` — model comparison: `"enumeration"` (requires
  enumeration via multiple `n_profiles` or `models`).

- `"all"` — every available plot.

Use
[`clustering_plot_types()`](https://pak.dynasite.org/Saqrmisc/reference/clustering_plot_types.md)
for the full catalogue.

## Usage

``` r
plot_clustering(
  results,
  type = "clusters",
  scale = c("raw", "standardized"),
  ...
)
```

## Arguments

- results:

  A `saqr_clustering` object from
  [`clustering()`](https://pak.dynasite.org/Saqrmisc/reference/clustering.md).

- type:

  Character vector of plot types and/or groups. Defaults to
  `"clusters"`.

- scale:

  `"raw"` (default, shows the data as fitted) or `"standardized"`.

- ...:

  Further arguments passed to `plot.multilpa()`.

## Value

Invisibly, the fitted `multilpa` object (or enumeration).

## See also

[`clustering()`](https://pak.dynasite.org/Saqrmisc/reference/clustering.md),
[`clustering_plot_types()`](https://pak.dynasite.org/Saqrmisc/reference/clustering_plot_types.md),
[`latents::plot_views()`](https://pak.dynasite.org/latents/reference/plot_views.html)

## Examples

``` r
# \donttest{
fit <- clustering(iris,
  vars = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  n_profiles = 3, models = "EEE", seed = 1)
#> Latent profile analysis
#>   Profiles: 3 
#>   Models: EEE 
#>   Scaling: standardize  | n: 150  | vars: 4 
#>   EEE with 3 profiles: logLik = -364.7, BIC = 849.6, converged = TRUE
plot_clustering(fit)





plot_clustering(fit, type = "diagnostics")



plot_clustering(fit, type = "heatmap")

# }
```
