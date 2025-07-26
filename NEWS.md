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