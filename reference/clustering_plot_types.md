# List the Plot Types of plot_clustering()

Returns the catalogue of plots that
[`plot_clustering()`](https://pak.dynasite.org/Saqrmisc/reference/plot_clustering.md)
can draw. Any `type` value, group name, or `"all"` can be passed to
[`plot_clustering()`](https://pak.dynasite.org/Saqrmisc/reference/plot_clustering.md).

## Usage

``` r
clustering_plot_types()
```

## Value

A data.frame with columns `type`, `group`, and `description`.

## Examples

``` r
clustering_plot_types()
#>          type       group
#> 1    profiles    clusters
#> 2        bars    clusters
#> 3     heatmap    clusters
#> 4   raincloud    clusters
#> 5       sizes    clusters
#> 6     entropy diagnostics
#> 7  posteriors diagnostics
#> 8       avepp diagnostics
#> 9 enumeration   selection
#>                                                   description
#> 1       Profile means across indicators, one line per profile
#> 2            Profile means as grouped bars with 95% intervals
#> 3  Profile means as a diverging heatmap (SDs from grand mean)
#> 4         Density + box + jitter of each indicator by profile
#> 5           Number and percentage of observations per profile
#> 6    Posterior-probability histogram (classification entropy)
#> 7               Per-observation posterior by assigned profile
#> 8 Average posterior probability matrix (assigned x posterior)
#> 9                      BIC / AIC across enumerated candidates
```
