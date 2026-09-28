# Plot a Clustering Result

Builds figures of a
[`clustering()`](https://pak.dynasite.org/Saqrmisc/reference/clustering.md)
result with latents, as ggplot objects. `type` takes any mix of plot
types and groups:

- `"clusters"`: `"profiles"`, `"bars"`, `"heatmap"`, `"raincloud"`,
  `"parallel"`, `"pairs"`, `"sizes"`.

- `"diagnostics"`: `"entropy"`, `"posteriors"`, `"avepp"`.

- `"selection"`: `"enumeration"` (information criteria across the
  candidates) and `"tree"` (how profiles split as more are added); both
  need more than one candidate.

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

One type: a ggplot object. Several: a `saqr_plots` list of ggplot
objects named by type, which draws every one when printed. Print, save
with
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html),
or restyle with `+ ggplot2::theme()`.

## Errors

Raises `saqrmisc_bad_input` when `results` is not a
[`clustering()`](https://pak.dynasite.org/Saqrmisc/reference/clustering.md)
result and `saqrmisc_bad_type` for an unknown `type`. A selection view
that needs more candidates than were fitted is skipped with a message.

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
plot_clustering(fit, type = "profiles")

plot_clustering(fit, type = "diagnostics")



plot_clustering(fit, type = c("pairs", "tree"))


# }
```
