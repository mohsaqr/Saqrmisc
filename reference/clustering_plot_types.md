# List the Plot Types of plot_clustering()

Returns the catalogue of plots that
[`plot_clustering()`](https://pak.dynasite.org/Saqrmisc/reference/plot_clustering.md)
can draw, with the group each belongs to. Any `type` or `group` value,
or `"all"`, can be passed to the `type` argument of
[`plot_clustering()`](https://pak.dynasite.org/Saqrmisc/reference/plot_clustering.md).

## Usage

``` r
clustering_plot_types()
```

## Value

A data.frame with one row per plot type and columns `type`, `group`
(`"clusters"`, `"diagnostics"`, or `"selection"`), and `description`.

## Examples

``` r
clustering_plot_types()
#>            type       group
#> 1       profile    clusters
#> 2       heatmap    clusters
#> 3  distribution    clusters
#> 4         sizes    clusters
#> 5     certainty diagnostics
#> 6         avepp diagnostics
#> 7    projection diagnostics
#> 8           bic   selection
#> 9           aic   selection
#> 10          icl   selection
#>                                                               description
#> 1                 Mean of each variable per cluster, one line per cluster
#> 2  Cluster means, coloured by standardised distance from the overall mean
#> 3                       Within-cluster spread of each variable (boxplots)
#> 4                       Number and percentage of observations per cluster
#> 5    Posterior probability of the assigned cluster, with relative entropy
#> 6    Average posterior probability: assigned cluster by posterior cluster
#> 7          Observations on the first two principal components, by cluster
#> 8            BIC across models and numbers of clusters (higher is better)
#> 9            AIC across models and numbers of clusters (higher is better)
#> 10           ICL across models and numbers of clusters (higher is better)
```
