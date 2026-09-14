# Changelog

## Saqrmisc 0.9.4

### Systematic clustering plots

- New `plot_clustering(results, type, model, scale)`: one verb for every
  figure of a
  [`clustering()`](https://pak.dynasite.org/Saqrmisc/reference/clustering.md)
  result. `type` takes any mix of plot names, group names, or `"all"`:
  - `"clusters"`: `profile`, `heatmap`, `distribution`, `sizes`.
  - `"diagnostics"`: `certainty` (with relative entropy), `avepp`
    (average posterior probability), `projection` (principal
    components).
  - `"selection"`: `bic`, `aic`, `icl`, with the best model circled.
- One plot returns a ggplot; several return a `clustering_plots` object
  that draws every plot when printed and lists its contents via
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).
- New
  [`clustering_plot_types()`](https://pak.dynasite.org/Saqrmisc/reference/clustering_plot_types.md)
  returns the catalogue as a data.frame.
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on a
  [`clustering()`](https://pak.dynasite.org/Saqrmisc/reference/clustering.md)
  result now draws through
  [`plot_clustering()`](https://pak.dynasite.org/Saqrmisc/reference/plot_clustering.md)
  and takes the same `type`, `model`, and `scale`. **Behaviour
  changes:** the default `type` is `"clusters"` (was `"profile"`);
  `"comparison"` is now `"selection"` (see below); `"barchart"` is no
  longer a [`plot()`](https://rdrr.io/r/graphics/plot.default.html) type
  (still available in
  [`plot_model()`](https://pak.dynasite.org/Saqrmisc/reference/plot_model.md)).
- `model` accepts one or more model names or `"all"`. Cluster and
  diagnostic plots are drawn once per model (named `"<model>/<type>"`);
  selection plots once. `type = "all"` draws **every plot for every
  fitted model** unless `model` narrows it.
- Plots use the Okabe-Ito palette and pair colour with shape or
  position. The heatmap colours standardised distance from the overall
  mean, so variables in different units are comparable, and labels the
  actual means.

## Saqrmisc 0.9.3

### Stratified (faceted) mosaic analysis

- [`mosaic_analysis()`](https://pak.dynasite.org/Saqrmisc/reference/mosaic_analysis.md)
  gains `by =`, which fits the `var1` x `var2` table separately within
  each level of a third variable and draws one mosaic panel per stratum.
  This is the standard check for effect modification and for Simpson’s
  paradox, where a pooled association weakens, vanishes or reverses
  inside every subgroup.
  - Category filtering (`min_count`) is applied to the **pooled** table
    before splitting, so every panel shows the same rows and columns.
  - The residual colour scale is **shared** across panels, so a given
    shade means the same standardized deviation everywhere.
  - Panel strips carry the stratum size, since panels are drawn equal
    width.
- New `by_label`, `min_stratum_n`, `p_adjust`, `facet_ncol`,
  `facet_show_n` and `seed` arguments. Per-stratum p-values are
  corrected for multiplicity (Benjamini-Hochberg by default) and
  reported beside the raw values.
- A stratified fit gains class `mosaic_stratified` and the fields
  `strata_summary`, `strata_residuals`, `strata_table` and
  `overall_summary`, the last pairing the pooled test with a
  Cochran-Mantel-Haenszel test of the association conditional on the
  stratifier.
- New [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  method for `mosaic_analysis` objects, so results are reached with
  `as.data.frame(fit, what = "strata")` rather than by indexing into the
  object. `what` accepts “summary”, “table”, “residuals”, “strata” and
  “overall”.
- New [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) methods for
  stratified fits.
- `seed` makes the Monte-Carlo Fisher p-value reproducible and restores
  the caller’s RNG stream on exit.

### Tile labels

- `tile_label = "count_percent"` prints the count with its percentage on
  a second line, e.g. `3,391` over `(29.8%)`. The percentage follows
  `percentage_base` (“total”, “row” or “column”) and, under
  stratification, is computed within each panel. A two-line label is
  required to clear roughly twice the tile height of a one-line label
  before it is drawn, so it is suppressed in tiles too short to hold it
  rather than spilling over the edge.

### Bug fixes

- A stratum in which a category is unobserved no longer produces `NaN`
  statistics. The test now runs on the non-empty core of the table
  (giving the correct degrees of freedom) while residuals are padded
  back onto the shared category grid so panels stay aligned. Affected
  strata are named in a warning.
- Category labels are fitted to the panel width when faceting: long
  names wrap and labels that would overprint a neighbour are suppressed.
  Un-stratified plots are unaffected.

### Shiny app

- New “Stratify / facet” control, “Strata” tab (per-group tests, plus
  pooled vs conditional), and per-group residuals.
- Plot size can now be set either as the whole canvas or per panel, with
  the resulting canvas size reported; both downloads follow the same
  size.
- Warnings raised during a fit (low expected counts, dropped strata) are
  shown in the interface instead of being suppressed.

## Saqrmisc 0.9.2

- Added
  [`cluster()`](https://pak.dynasite.org/Saqrmisc/reference/clustering.md)
  as a short alias for
  [`clustering()`](https://pak.dynasite.org/Saqrmisc/reference/clustering.md).
- Added optional `cluster_*` aliases for the existing clustering
  helpers. All original function names remain unchanged.
- Fixed
  [`clustering()`](https://pak.dynasite.org/Saqrmisc/reference/clustering.md)
  model comparisons by storing the scalar fitted log-likelihood instead
  of each model’s variable-length iteration history.
- [`clustering()`](https://pak.dynasite.org/Saqrmisc/reference/clustering.md)
  now stores a comparison table containing BIC, AIC, and ICL.
  `plot(..., type = "all")` plots each criterion separately, while
  `"bic"`, `"aic"`, and `"icl"` can be requested individually.
- Information-criterion plots now place the number of clusters on the
  x-axis and draw one line per covariance model.
- Corrected MoEClust information-criterion ranking so larger values
  select the best model.
- Preserved both the complete input data and the complete-case analysis
  data in clustering results.
- [`get_cluster_assignments()`](https://pak.dynasite.org/Saqrmisc/reference/get_cluster_assignments.md)
  now preserves rows omitted during complete-case fitting and marks
  their assignments and probabilities as `NA`.
- Added a tidy interface:
  [`summary()`](https://rdrr.io/r/base/summary.html) returns a ranked
  model tibble, [`fitted()`](https://rdrr.io/r/stats/fitted.values.html)
  returns original rows with fitted clusters, and
  `get_best_model(..., what = "fit")` extracts the raw MoEClust fit.
- `plot(..., type = "all")` now means every plot for every fitted model.
  Added
  [`plot_best_model()`](https://pak.dynasite.org/Saqrmisc/reference/plot_best_model.md)
  and
  [`plot_model()`](https://pak.dynasite.org/Saqrmisc/reference/plot_model.md)
  for focused plotting.

## Saqrmisc 0.9.1

- [`mosaic_analysis()`](https://pak.dynasite.org/Saqrmisc/reference/mosaic_analysis.md)
  no longer draws the variable-name axis titles by default, which
  previously overprinted the category labels (especially next to thin
  categories). Set `show_varnames = TRUE` to restore them.

## Saqrmisc 0.1.0

### Initial Release

This is the initial release of the Saqrmisc package, providing
comprehensive tools for data analysis and visualization.

#### New Features

- **Model-Based Clustering Analysis** (`run_full_moe_analysis`)
  - Comprehensive MoEClust analysis with all 14 covariance models
  - Robust error handling for failed model convergence
  - Dual-scale outputs (original and scaled data)
  - Profile plots and summary tables
- **Statistical Comparison Analysis** (`generate_comparison_plots`)
  - Automated comparison plots using ggbetweenstats
  - Support for stratified analysis by additional variables
  - Quality control with automatic exclusion of small categories
  - Flexible output options (plots and tables)
- **Categorical Variable Analysis** (`mosaic_analysis`)
  - Comprehensive mosaic plot analysis
  - Chi-square testing with effect size calculation (Cramér’s V)
  - Quality filtering for minimum observation counts
  - Detailed summary tables with percentages
- **Helper Functions**
  - [`view_results()`](https://pak.dynasite.org/Saqrmisc/reference/view_results.md)
    for easy visualization of clustering results

#### Documentation

- Comprehensive README with installation instructions and examples
- Detailed function documentation with roxygen2
- Introduction vignette with complete workflow examples
- Package website configuration with pkgdown

#### Infrastructure

- GitHub Actions workflow for continuous integration
- Test suite with testthat framework
- Proper package structure with all required files
- MIT license and citation information

#### Dependencies

- Core: MoEClust, mclust, dplyr, ggplot2, ggstatsplot, vcd, grid,
  tibble, rlang, gridExtra, gt, janitor, prcr, ggcharts, MASS, tidyverse
- Suggested: testthat, knitr, rmarkdown, devtools, roxygen2
