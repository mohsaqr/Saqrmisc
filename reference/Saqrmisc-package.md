# Saqrmisc: Comprehensive Data Analysis and Visualization for R

A comprehensive R package providing functions for statistical analysis,
data transformation, and visualization. Designed for exploratory data
analysis with robust error handling and publication-ready outputs.

## Main Features

The package is organized into the following function categories:

### Group Comparisons

- [`compare_groups`](https://pak.dynasite.org/Saqrmisc/reference/compare_groups.md):
  Compare groups with t-tests, ANOVA, post-hoc tests, Bayesian analysis,
  and equivalence testing (TOST). Supports stratified analyses.

### Correlation Analysis

- [`correlations`](https://pak.dynasite.org/Saqrmisc/reference/correlations.md):
  Full pairwise correlation table with complete statistics (r, CI, t,
  df, p, n). Supports bivariate, partial, and semi-partial correlations,
  group-wise correlations, and multilevel (within-cluster) correlations.

- [`correlation_matrix`](https://pak.dynasite.org/Saqrmisc/reference/correlation_matrix.md):
  Publication-ready correlation matrix with significance stars, heatmap
  visualization, and optional confidence intervals.

### Descriptive Statistics

- [`descriptive_table`](https://pak.dynasite.org/Saqrmisc/reference/descriptive_table.md):
  Publication-ready descriptive statistics with gt formatting. Supports
  20+ statistics and group stratification.

- [`categorical_table`](https://pak.dynasite.org/Saqrmisc/reference/categorical_table.md):
  Frequency tables with cross-tabulation, chi-square tests, and Cramer's
  V effect size.

- [`auto_describe`](https://pak.dynasite.org/Saqrmisc/reference/auto_describe.md):
  Automatic detection and description of all variables.

- [`data_overview`](https://pak.dynasite.org/Saqrmisc/reference/data_overview.md):
  Quick overview of data structure and quality.

### Data Transformation

- [`center`](https://pak.dynasite.org/Saqrmisc/reference/center.md) /
  [`center_vec`](https://pak.dynasite.org/Saqrmisc/reference/center_vec.md):
  Mean-centering with optional group-wise centering for multilevel data.

- [`scale_vars`](https://pak.dynasite.org/Saqrmisc/reference/scale_vars.md)
  /
  [`scale_vec`](https://pak.dynasite.org/Saqrmisc/reference/scale_vec.md):
  Scaling by SD or min-max normalization with optional group-wise
  scaling.

- [`standardize`](https://pak.dynasite.org/Saqrmisc/reference/standardize.md)
  /
  [`standardize_vec`](https://pak.dynasite.org/Saqrmisc/reference/standardize_vec.md):
  Z-score standardization with optional group-wise standardization.

- [`reverse_code`](https://pak.dynasite.org/Saqrmisc/reference/reverse_code.md)
  /
  [`reverse_code_vec`](https://pak.dynasite.org/Saqrmisc/reference/reverse_code_vec.md):
  Reverse coding for Likert scales and similar measures.

### Missing Data

- [`missing_analysis`](https://pak.dynasite.org/Saqrmisc/reference/missing_analysis.md):
  Comprehensive missing data analysis with patterns, Little's MCAR test,
  and visualizations.

- [`replace_missing`](https://pak.dynasite.org/Saqrmisc/reference/replace_missing.md):
  Imputation using mean, median, mode, or custom values. Supports
  group-wise imputation.

### Outlier Analysis

- [`outlier_check`](https://pak.dynasite.org/Saqrmisc/reference/outlier_check.md):
  Detect outliers using IQR, Z-score, or Mahalanobis distance methods.

- [`replace_outliers`](https://pak.dynasite.org/Saqrmisc/reference/replace_outliers.md):
  Replace or winsorize outliers.

- [`is_outlier`](https://pak.dynasite.org/Saqrmisc/reference/is_outlier.md)
  /
  [`winsorize_vec`](https://pak.dynasite.org/Saqrmisc/reference/winsorize_vec.md):
  Vectorized functions for use with dplyr.

### Normality Testing

- [`normality_check`](https://pak.dynasite.org/Saqrmisc/reference/normality_check.md):
  Comprehensive normality testing with Shapiro-Wilk, Kolmogorov-Smirnov,
  and visual diagnostics.

### Clustering

- [`clustering`](https://pak.dynasite.org/Saqrmisc/reference/clustering.md):
  Latent profile clustering via the latents package. Supports all 14
  covariance structures.

- [`get_cluster_assignments`](https://pak.dynasite.org/Saqrmisc/reference/get_cluster_assignments.md):
  Extract cluster assignments with optional membership probabilities.

- [`cluster_diagnostics`](https://pak.dynasite.org/Saqrmisc/reference/cluster_diagnostics.md):
  Classification diagnostics.

- [`model_comparison_table`](https://pak.dynasite.org/Saqrmisc/reference/model_comparison_table.md):
  Compare models by BIC, AIC, or ICL.

- [`generate_cluster_report`](https://pak.dynasite.org/Saqrmisc/reference/generate_cluster_report.md):
  Generate comprehensive cluster reports.

### Categorical Analysis

- [`mosaic_analysis`](https://pak.dynasite.org/Saqrmisc/reference/mosaic_analysis.md):
  Mosaic plot analysis with chi-square tests, Fisher's exact test, and
  Cramer's V effect size.

### Network Analysis

- [`estimate_single_network`](https://pak.dynasite.org/Saqrmisc/reference/estimate_single_network.md):
  Estimate psychological networks using bootnet, mgm, and qgraph.

- [`estimate_grouped_networks`](https://pak.dynasite.org/Saqrmisc/reference/estimate_grouped_networks.md):
  Estimate networks by group.

- [`compare_networks`](https://pak.dynasite.org/Saqrmisc/reference/compare_networks.md):
  Compare networks between groups.

## API Design

All functions use a consistent API with quoted strings for variable
names:

- Variable names are passed as character strings (e.g.,
  `group_by = "gender"`)

- Multiple variables use character vectors (e.g.,
  `Vars = c("age", "score")`)

- When `Vars = NULL`, functions auto-select all numeric variables

## Output Formats

Most functions support multiple output formats:

- `format = "gt"`: Publication-ready gt tables (default)

- `format = "data.frame"`: Raw data frames for further processing

## See also

Useful links:

- <https://github.com/mohsaqr/Saqrmisc>

- Report bugs at <https://github.com/mohsaqr/Saqrmisc/issues>

## Author

Mohammed Saqr <saqr@saqr.me>
