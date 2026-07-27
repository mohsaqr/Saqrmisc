# Saqrmisc 0.9.2

* Added `cluster()` as a short alias for `clustering()`.
* Added optional `cluster_*` aliases for the existing clustering helpers.
  All original function names remain unchanged.
* Fixed `clustering()` model comparisons by storing the scalar fitted
  log-likelihood instead of each model's variable-length iteration history.
* `clustering()` now stores a comparison table containing BIC, AIC, and ICL.
  `plot(..., type = "all")` plots each criterion separately, while `"bic"`,
  `"aic"`, and `"icl"` can be requested individually.
* Information-criterion plots now place the number of clusters on the x-axis
  and draw one line per covariance model.
* Corrected MoEClust information-criterion ranking so larger values select the
  best model.
* Preserved both the complete input data and the complete-case analysis data
  in clustering results.
* `get_cluster_assignments()` now preserves rows omitted during complete-case
  fitting and marks their assignments and probabilities as `NA`.
* Added a tidy interface: `summary()` returns a ranked model tibble,
  `fitted()` returns original rows with fitted clusters, and
  `get_best_model(..., what = "fit")` extracts the raw MoEClust fit.
* `plot(..., type = "all")` now means every plot for every fitted model.
  Added `plot_best_model()` and `plot_model()` for focused plotting.

# Saqrmisc 0.9.1

* `mosaic_analysis()` no longer draws the variable-name axis titles by default,
  which previously overprinted the category labels (especially next to thin
  categories). Set `show_varnames = TRUE` to restore them.

# Saqrmisc 0.1.0

## Initial Release

This is the initial release of the Saqrmisc package, providing comprehensive tools for data analysis and visualization.

### New Features

* **Model-Based Clustering Analysis** (`run_full_moe_analysis`)
  - Comprehensive MoEClust analysis with all 14 covariance models
  - Robust error handling for failed model convergence
  - Dual-scale outputs (original and scaled data)
  - Profile plots and summary tables

* **Statistical Comparison Analysis** (`generate_comparison_plots`)
  - Automated comparison plots using ggbetweenstats
  - Support for stratified analysis by additional variables
  - Quality control with automatic exclusion of small categories
  - Flexible output options (plots and tables)

* **Categorical Variable Analysis** (`mosaic_analysis`)
  - Comprehensive mosaic plot analysis
  - Chi-square testing with effect size calculation (Cramér's V)
  - Quality filtering for minimum observation counts
  - Detailed summary tables with percentages

* **Helper Functions**
  - `view_results()` for easy visualization of clustering results

### Documentation

* Comprehensive README with installation instructions and examples
* Detailed function documentation with roxygen2
* Introduction vignette with complete workflow examples
* Package website configuration with pkgdown

### Infrastructure

* GitHub Actions workflow for continuous integration
* Test suite with testthat framework
* Proper package structure with all required files
* MIT license and citation information

### Dependencies

* Core: MoEClust, mclust, dplyr, ggplot2, ggstatsplot, vcd, grid, tibble, rlang, gridExtra, gt, janitor, prcr, ggcharts, MASS, tidyverse
* Suggested: testthat, knitr, rmarkdown, devtools, roxygen2
