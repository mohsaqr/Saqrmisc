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
#>           type       group
#> 1     profiles    clusters
#> 2         bars    clusters
#> 3      heatmap    clusters
#> 4    raincloud    clusters
#> 5     parallel    clusters
#> 6        pairs    clusters
#> 7        sizes    clusters
#> 8      entropy diagnostics
#> 9   posteriors diagnostics
#> 10       avepp diagnostics
#> 11 enumeration   selection
#> 12        tree   selection
#>                                                                      description
#> 1                 Profile means across indicators, one labelled line per profile
#> 2                    Profile means as grouped bars from zero, with 95% intervals
#> 3           Profile means in observed standard deviations from the observed mean
#> 4  Each indicator's distribution by assigned profile: density, box, observations
#> 5                  Every case as a line across indicators, one panel per profile
#> 6                 Scatter-plot matrix with each profile's 95% covariance ellipse
#> 7                      Effective number of cases in each profile, with its share
#> 8                   Each case's entropy relative to a flat posterior, by profile
#> 9              Posterior probability of each case's assigned profile, by profile
#> 10          Average posterior probability: assigned profile by posterior profile
#> 11            Information criteria across a candidate grid (plot an enumeration)
#> 12                    How profiles split as more are added (plot an enumeration)
```
