# List the Plot Types of plot_clustering()

The figures
[`plot_clustering()`](https://pak.dynasite.org/Saqrmisc/reference/plot_clustering.md)
can draw. Any `type`, any `group`, or `"all"` can be passed as its
`type`.

## Usage

``` r
clustering_plot_types()
```

## Value

A data.frame with one row per plot type and columns `type`, `group`
(`"clusters"`, `"diagnostics"` or `"selection"`) and `description`.

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
#>                                                                     description
#> 1        Profile means across indicators, point size showing profile prevalence
#> 2        Profile means as grouped bars, with 95% intervals when `data` is given
#> 3         Profile means as standard deviations from each indicator's grand mean
#> 4 Each indicator's distribution by assigned profile: density, box, observations
#> 5                     Effective number of cases in each profile, with its share
#> 6                  Per-case entropy contribution within each profile, as ridges
#> 7                      Posterior probability of the assigned profile, as ridges
#> 8          Average posterior probability: assigned profile by posterior profile
#> 9            Information criteria across a candidate grid (plot an enumeration)
```
