# Launch the Saqrmisc mosaic analysis Shiny app

Opens an interactive Shiny application for
[`mosaic_analysis`](https://pak.dynasite.org/Saqrmisc/reference/mosaic_analysis.md).
Upload a CSV (or use the built-in demo), pick two categorical variables,
and explore the chi-square / Fisher test, Cramer's V effect size,
standardized residuals, and the shaded mosaic plot. Every
[`mosaic_analysis()`](https://pak.dynasite.org/Saqrmisc/reference/mosaic_analysis.md)
option is exposed in the sidebar.

Requires the shiny and DT packages.

## Usage

``` r
launch_app(...)
```

## Arguments

- ...:

  Passed to [`runApp`](https://rdrr.io/pkg/shiny/man/runApp.html) (e.g.
  `port`, `launch.browser`, `host`).

## Value

Called for its side effect (launches the app). No return value.

## Examples

``` r
if (interactive()) {
  launch_app()
}
```
