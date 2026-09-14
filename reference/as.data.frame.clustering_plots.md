# List the Contents of a Set of Clustering Plots

List the Contents of a Set of Clustering Plots

## Usage

``` r
# S3 method for class 'clustering_plots'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A `clustering_plots` object from
  [`plot_clustering()`](https://pak.dynasite.org/Saqrmisc/reference/plot_clustering.md).

- row.names, optional:

  Ignored; present for S3 consistency.

- ...:

  Ignored.

## Value

A data.frame with one row per plot, in drawing order, and columns `name`
(the element name), `plot` (plot type), `group`, `model`, and
`description`.
