# Plot a Clustering Result

Draws figures of a
[`clustering()`](https://pak.dynasite.org/Saqrmisc/reference/clustering.md)
result with latents. `type` takes any mix of plot types and groups:

- `"clusters"`: `"profiles"`, `"bars"`, `"heatmap"`, `"raincloud"`,
  `"sizes"`.

- `"diagnostics"`: `"entropy"`, `"posteriors"`, `"avepp"`.

- `"selection"`: `"enumeration"`, information criteria across the
  candidates (needs more than one candidate).

- `"all"`: every type above.

## Usage

``` r
plot_clustering(results, type = "clusters", ...)
```

## Arguments

- results:

  A `saqr_clustering` object from
  [`clustering()`](https://pak.dynasite.org/Saqrmisc/reference/clustering.md).

- type:

  Plot types and/or groups; see
  [`clustering_plot_types()`](https://pak.dynasite.org/Saqrmisc/reference/clustering_plot_types.md).

- ...:

  Passed to the latents plot method of the selected fit, e.g.
  `scale = "standardized"` or `main`.

## Value

`results`, invisibly. Called for its plots.

## Errors

Raises `saqrmisc_bad_input` when `results` is not a
[`clustering()`](https://pak.dynasite.org/Saqrmisc/reference/clustering.md)
result and `saqrmisc_bad_type` for an unknown `type`.

## See also

[`clustering()`](https://pak.dynasite.org/Saqrmisc/reference/clustering.md),
[`clustering_plot_types()`](https://pak.dynasite.org/Saqrmisc/reference/clustering_plot_types.md)

## Examples

``` r
# \donttest{
set.seed(1)
vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
fit <- clustering(iris, vars, n_profiles = 2:4, models = "EEE")
#> Fitting 3 candidate(s): 2, 3, 4 profiles x EEE.
#> Selected EEE with 4 profiles (BIC = 808.1).
plot_clustering(fit)





plot_clustering(fit, type = "diagnostics")



plot_clustering(fit, type = c("heatmap", "enumeration"))


# }
```
