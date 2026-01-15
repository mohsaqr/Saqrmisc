# Saqrmisc: Comprehensive Data Analysis and Visualization R Package

[![R-CMD-check](https://github.com/mohsaqr/Saqrmisc/workflows/R-CMD-check/badge.svg)](https://github.com/mohsaqr/Saqrmisc/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-4.0.0+-blue.svg)](https://www.r-project.org/)

## Overview

**Saqrmisc** is a comprehensive R package providing powerful functions for statistical analysis, visualization, and reporting. Designed for exploratory data analysis with robust error handling and publication-ready outputs.

### Key Features

| Function | Purpose |
|----------|---------|
| `compare_groups()` | Group comparisons with t-tests, ANOVA, post-hoc tests, Bayesian analysis |
| `descriptive_table()` | Publication-ready descriptive statistics with gt formatting |
| `categorical_table()` | Frequency tables with cross-tabulation and chi-square tests |
| `clustering()` | Model-based clustering with MoEClust (14 covariance models) |
| `Mosaic()` | Categorical variable analysis with mosaic plots and chi-square tests |
| `estimate_and_plot_network()` | Network estimation and visualization |

## Installation

```r
# Install from GitHub
devtools::install_github("mohsaqr/Saqrmisc")

# Load the package
library(Saqrmisc)
```

### Dependencies

```r
install.packages(c(
  "dplyr", "ggplot2", "ggstatsplot", "gt", "gridExtra",
  "MoEClust", "effectsize", "effsize",
  "bootnet", "qgraph", "mgm",
  "BayesFactor", "TOSTER"
))
```

---

# Function Reference

## 1. Group Comparisons (`compare_groups`)

Compare groups using publication-ready visualizations powered by `ggstatsplot`. **Automatic test selection** based on the number of groups:

| Groups | Parametric | Nonparametric | Effect Size |
|--------|------------|---------------|-------------|
| 2 | t-test | Mann-Whitney U | Cohen's d / rank-biserial r |
| 3+ | ANOVA | Kruskal-Wallis | Eta-squared / epsilon-squared |

### Basic Usage

```r
# Two-group comparison (t-test)
results <- compare_groups(
  data = mydata,
  category = gender,
  Vars = c("score1", "score2")
)

# View plot
results$plots$gender_vs_score1

# View summary table
results$summary_table
```

### ANOVA with Post-hoc Tests (3+ groups)

```r
# Sample data
set.seed(42)
data <- data.frame(
  country = rep(c("USA", "UK", "Germany"), each = 40),
  score = c(rnorm(40, 75, 10), rnorm(40, 70, 11), rnorm(40, 85, 9))
)

# Run analysis with post-hoc comparisons
results <- compare_groups(
  data = data,
  category = country,
  Vars = c("score"),
  posthoc = TRUE,
  posthoc_method = "tukey",      # or "games-howell" (default)
  pairwise_display = "all"       # Show all comparisons on plot
)

# View post-hoc pairwise comparison table
results$summary_data$posthoc_results[[1]]
```

**Post-hoc Output:**
```
  variable  comparison       diff   ci_lower  ci_upper   p_adj significance
1    score  UK-Germany    -14.56     -20.10     -9.02  <0.001          ***
2    score USA-Germany    -10.83     -16.37     -5.29  <0.001          ***
3    score      USA-UK      3.73      -1.81      9.27   0.251
```

### Full ANOVA Text Report (APA Style)

```r
# Print the complete ANOVA report
cat(results$summary_data$anova_report[1])
```

**Output:**
```
======================================================================
ANOVA RESULTS: score by country
======================================================================

DESCRIPTIVE STATISTICS:
  Germany:        M =  85.44, SD =  8.71, n = 40
  UK:             M =  70.88, SD = 10.08, n = 40
  USA:            M =  74.60, SD = 12.22, n = 40

OMNIBUS TEST:
  F(2, 117) = 21.00, p < .001, eta-sq = 0.264 (large effect)

  A one-way ANOVA revealed a statistically significant difference in score
  between country groups, F(2, 117) = 21.00, p < .001, eta-sq = 0.264.

POST-HOC COMPARISONS (Tukey HSD):
  UK-Germany:               diff = -14.56, p < .001 ***
  USA-Germany:              diff = -10.83, p < .001 ***
  USA-UK:                   diff =   3.73, p = 0.251

CONCLUSION:
  Significant pairwise differences were found between:
  - UK-Germany (p < .001)
  - USA-Germany (p < .001)
======================================================================
```

### Stratified Analysis (Separate Tests per Subgroup)

```r
# Compare gender WITHIN each country
data <- data.frame(
  country = rep(c("USA", "UK", "Germany"), each = 60),
  gender = rep(c("Male", "Female"), times = 90),
  score = c(
    rnorm(30, 80, 8), rnorm(30, 65, 8),   # USA: Males higher
    rnorm(30, 72, 10), rnorm(30, 70, 10), # UK: No difference
    rnorm(30, 68, 9), rnorm(30, 78, 9)    # Germany: Females higher
  )
)

results <- compare_groups(
  data = data,
  category = gender,
  Vars = c("score"),
  repeat_category = country
)

# Access results by country
results$USA$summary_data
results$UK$plots$gender_vs_score
results$Germany$summary_table
results$metadata  # Analysis information
```

### Bayesian Analysis

```r
results <- compare_groups(
  data = data,
  category = group,
  Vars = c("score"),
  bayesian = TRUE
)

# View Bayes Factors
results$summary_data[, c("group", "mean", "bf10", "bf_interpretation")]
#>     group  mean    bf10        bf_interpretation
#> 1 Control  5.07  407.70  Extreme evidence for H1
#> 2   Treat  6.38  407.70  Extreme evidence for H1
```

### Nonparametric Tests

Use the `type` parameter for easy test selection:

```r
# Using type parameter (recommended)
results <- compare_groups(
  data = data,
  category = country,
  Vars = c("score"),
  type = "kw"  # Kruskal-Wallis for 3+ groups
)

# Shorthand options:
type = "p"      # Parametric (t-test/ANOVA)
type = "np"     # Nonparametric (Mann-Whitney/Kruskal-Wallis)
type = "kw"     # Kruskal-Wallis (3+ groups)
type = "mw"     # Mann-Whitney (2 groups)
type = "bayes"  # Bayesian tests

# Or use the nonparametric parameter
results <- compare_groups(
  data = data,
  category = group,
  Vars = c("score"),
  nonparametric = TRUE
)
```

### Equivalence Testing (TOST)

```r
results <- compare_groups(
  data = data,
  category = gender,
  Vars = c("score"),
  equivalence = TRUE,
  equivalence_bounds = c(-0.3, 0.3)  # Small effect bounds
)

# Check equivalence conclusions
results$summary_data[, c("gender", "mean", "equivalence_conclusion")]
```

### All Parameters

| Parameter | Description | Default |
|-----------|-------------|---------|
| `data` | Data frame | Required |
| `category` | Grouping variable (unquoted) | Required |
| `Vars` | Character vector of numeric variables | Required |
| `repeat_category` | Stratification variable (unquoted) | `NULL` |
| `plots` | Generate plots? | `TRUE` |
| `table` | Generate summary table? | `TRUE` |
| `type` | Test type: `"auto"`, `"p"`, `"np"`, `"kw"`, `"mw"`, `"bayes"` | `"auto"` |
| `bayesian` | Compute Bayes Factors? | `FALSE` |
| `equivalence` | Perform TOST? | `FALSE` |
| `equivalence_bounds` | TOST bounds (Cohen's d) | `c(-0.5, 0.5)` |
| `nonparametric` | Use nonparametric tests? | `FALSE` |
| `p_adjust_method` | P-value adjustment method | `"none"` |
| `posthoc` | Compute post-hoc tests? (3+ groups) | `TRUE` |
| `posthoc_method` | `"games-howell"` or `"tukey"` | `"games-howell"` |
| `pairwise_display` | `"significant"`, `"all"`, `"none"` | `"significant"` |
| `min_threshold` | Min proportion for repeat_category | `0.05` |
| `min_subcategory` | Min observations per subgroup | `5` |
| `colors` | Custom color palette | `NULL` |
| `verbose` | Print progress? | `TRUE` |

---

## 2. Descriptive Statistics (`descriptive_table`)

Generate publication-ready descriptive statistics tables with flexible statistics selection and beautiful gt formatting.

### Basic Usage

```r
# Default statistics (n, mean, sd, median, min, max)
descriptive_table(
  data = mydata,
  Vars = c("age", "score", "income")
)
```

### Extended Statistics

```r
descriptive_table(
  data = mydata,
  Vars = c("age", "score"),
  stats = c("n", "missing", "mean", "sd", "se", "median", "iqr", "skewness", "kurtosis")
)
```

### Stratified by Group

```r
descriptive_table(
  data = mydata,
  Vars = c("age", "score"),
  group_by = gender,
  overall = TRUE  # Include overall row
)
```

### Custom Labels and Themes

```r
descriptive_table(
  data = mydata,
  Vars = c("age", "score", "income"),
  labels = c(
    age = "Age (years)",
    score = "Test Score",
    income = "Annual Income ($)"
  ),
  title = "Sample Characteristics",
  subtitle = "N = 100 participants",
  theme = "colorful"  # Options: default, minimal, dark, colorful
)
```

### Available Statistics

| Statistic | Description |
|-----------|-------------|
| `n` | Sample size (non-missing) |
| `missing` | Count of missing values |
| `missing_pct` | Percentage missing |
| `mean` | Arithmetic mean |
| `sd` | Standard deviation |
| `se` | Standard error |
| `var` | Variance |
| `median` | Median (50th percentile) |
| `min`, `max` | Minimum, maximum |
| `range` | Range (max - min) |
| `iqr` | Interquartile range |
| `q1`, `q3` | 25th, 75th percentiles |
| `skewness` | Skewness coefficient |
| `kurtosis` | Excess kurtosis |
| `cv` | Coefficient of variation (%) |

### Export Options

```r
# Get as data frame
df <- descriptive_table(data, Vars, format = "data.frame")

# Export gt table
table <- descriptive_table(data, Vars)
gtsave(table, "descriptives.html")  # or .docx, .pdf, .png
```

---

## 3. Categorical Frequency Tables (`categorical_table`)

Generate publication-ready frequency tables for categorical variables with cross-tabulation, chi-square tests, and Cramer's V effect size.

### Single Variable Frequency Table

```r
# Basic frequency table
categorical_table(data, var = gender)

# Sorted by frequency
categorical_table(data, var = education, sort_by = "frequency")
```

**Output:**
```
| gender | n (%)      | Cumulative % |
|--------|------------|--------------|
| Male   | 93 (46.5%) | 46.5         |
| Female | 81 (40.5%) | 87.0         |
| Other  | 26 (13.0%) | 100.0        |
| Total  | 200 (100%) | 100.0        |
```

### Cross-Tabulation with Chi-Square Test

```r
categorical_table(
  data = data,
  var = gender,
  by = education,
  chi_square = TRUE,
  cramers_v = TRUE
)
```

The table includes a footnote with test results:
> Chi-square(6) = 2.88, p = 0.824; Cramer's V = 0.085 (negligible effect)

### Custom Labels and Themes

```r
categorical_table(
  data = data,
  var = education,
  labels = c(
    "High School" = "Secondary",
    "Bachelor" = "BSc",
    "Master" = "MSc",
    "PhD" = "Doctorate"
  ),
  title = "Educational Attainment",
  theme = "colorful"  # Options: default, minimal, dark, colorful
)
```

### Parameters

| Parameter | Description | Default |
|-----------|-------------|---------|
| `var` | Primary categorical variable (unquoted) | Required |
| `by` | Second variable for cross-tabulation | `NULL` |
| `percentages` | `"col"`, `"row"`, `"total"` | auto |
| `show_total` | Include totals row/column | `TRUE` |
| `show_missing` | Include missing as category | `FALSE` |
| `chi_square` | Compute chi-square test | `TRUE` |
| `cramers_v` | Compute effect size | `TRUE` |
| `sort_by` | `"none"`, `"frequency"`, `"alphabetical"` | `"none"` |
| `combine` | Show as "n (%)" format | `TRUE` |
| `theme` | Visual theme | `"default"` |

---

## 4. Model-Based Clustering (`clustering`)

Perform model-based clustering using Gaussian Mixture Models via MoEClust. Tests 14 different covariance structures to find the optimal solution.

### Basic Usage

```r
# Run clustering analysis
results <- clustering(
  data = mydata,
  vars = c("var1", "var2", "var3"),
  n_clusters = 2:5,
  scaling = "standardize"
)

# Print summary
print(results)

# Plot results
plot(results)                      # Profile plot (default)
plot(results, type = "heatmap")    # Cluster means heatmap
plot(results, type = "barchart")   # Grouped bar chart
plot(results, type = "sizes")      # Cluster size distribution
plot(results, type = "comparison") # Model comparison (BIC)
plot(results, type = "all")        # All plot types
```

### Get Cluster Assignments

```r
# Add cluster assignments to original data
data_with_clusters <- get_cluster_assignments(results)

# Include membership probabilities
data_with_clusters <- get_cluster_assignments(
  results,
  include_probabilities = TRUE
)

# Specify a particular model
data_with_clusters <- get_cluster_assignments(
  results,
  model_name = "VVV"
)
```

### Model Comparison Table

```r
# View all models ranked by BIC (formatted gt table)
model_comparison_table(results)

# Sort by different criteria
model_comparison_table(results, sort_by = "aic", top_n = 5)
model_comparison_table(results, sort_by = "icl")
```

### Assess Cluster Stability (Bootstrap)

```r
# Bootstrap stability assessment
stability <- assess_cluster_stability(
  results,
  model_name = "VVV",  # Optional: defaults to best model
  n_boot = 100,
  seed = 123
)

print(stability)
#> Cluster Stability Assessment
#> Model: VVV (3 clusters)
#> Bootstrap iterations: 100
#> Overall stability (ARI): 0.847
#> 95% CI: [0.792, 0.901]
#> Interpretation: Good stability

# View per-observation stability
hist(stability$observation_stability, main = "Observation Stability")
```

### Generate Cluster Reports

```r
# Print to console
generate_cluster_report(results)

# Get as gt tables
tables <- generate_cluster_report(results, output_format = "gt")

# Get as markdown
md <- generate_cluster_report(results, output_format = "markdown")
```

### Covariance Models

The function tests 14 different covariance structures:

| Model | Volume | Shape | Orientation | Description |
|-------|--------|-------|-------------|-------------|
| EII | Equal | Equal | - | Spherical, equal volume |
| VII | Variable | Equal | - | Spherical, variable volume |
| EEI | Equal | Equal | Axis-aligned | Diagonal, equal volume & shape |
| VEI | Variable | Equal | Axis-aligned | Diagonal, variable volume |
| EVI | Equal | Variable | Axis-aligned | Diagonal, equal volume |
| VVI | Variable | Variable | Axis-aligned | Diagonal, variable volume & shape |
| EEE | Equal | Equal | Equal | Ellipsoidal, equal all |
| VEE | Variable | Equal | Equal | Ellipsoidal, variable volume |
| EVE | Equal | Variable | Equal | Ellipsoidal, variable shape |
| VVE | Variable | Variable | Equal | Ellipsoidal, equal orientation |
| EEV | Equal | Equal | Variable | Ellipsoidal, variable orientation |
| VEV | Variable | Equal | Variable | Ellipsoidal, equal shape |
| EVV | Equal | Variable | Variable | Ellipsoidal, equal volume |
| VVV | Variable | Variable | Variable | Unconstrained |

### Parameters

| Parameter | Description | Default |
|-----------|-------------|---------|
| `data` | Data frame | Required |
| `vars` | Character vector of clustering variables | Required |
| `n_clusters` | Number(s) of clusters to test | Required |
| `scaling` | `"standardize"`, `"center"`, `"minmax"`, `"none"` | `"standardize"` |
| `models` | Covariance models to test (or `"all"`) | `"all"` |
| `verbose` | Print progress? | `TRUE` |
| `na_action` | How to handle NAs: `"omit"` or `"impute"` | `"omit"` |

---

## 5. Categorical Analysis (`Mosaic`)

Analyze relationships between categorical variables with mosaic plots, chi-square tests, Fisher's exact test, and Cramer's V effect size.

### Basic Usage

```r
# Analyze categorical relationship
results <- Mosaic(
  data = mydata,
  var1 = gender,
  var2 = preference
)

# S3 methods
print(results)
summary(results)
plot(results)
```

### Output

- Chi-square test statistic and p-value
- Fisher's exact test (for small expected frequencies)
- Cramer's V effect size with interpretation
- Contingency table with percentages
- Publication-ready mosaic plot

### Parameters

| Parameter | Description | Default |
|-----------|-------------|---------|
| `data` | Data frame | Required |
| `var1` | First categorical variable (unquoted) | Required |
| `var2` | Second categorical variable (unquoted) | Required |
| `verbose` | Print results? | `TRUE` |
| `save_plot` | Save plot to file? | `FALSE` |
| `plot_path` | Path for saved plot | `NULL` |

---

## 6. Network Analysis (`estimate_and_plot_network`)

Estimate and visualize psychological networks using bootnet, mgm, and qgraph.

### Basic Usage

```r
# Estimate network
results <- estimate_and_plot_network(
  data = mydata,
  variables = c("var1", "var2", "var3", "var4", "var5")
)

# View network plot
results$plot

# View centrality measures
results$centrality

# View edge weights
results$edges
```

### Network Comparison

```r
# Compare networks between two groups
comparison <- compare_networks(
  data = mydata,
  group_var = "condition",
  variables = c("var1", "var2", "var3")
)

comparison$plot_group1
comparison$plot_group2
comparison$comparison_stats
```

### Parameters

| Parameter | Description | Default |
|-----------|-------------|---------|
| `data` | Data frame | Required |
| `variables` | Character vector of variables | Required |
| `estimation` | `"EBICglasso"`, `"pcor"`, `"mgm"` | `"EBICglasso"` |
| `layout` | `"spring"`, `"circle"` | `"spring"` |
| `threshold` | Edge weight threshold | `0` |

---

## Complete Workflow Example

```r
library(Saqrmisc)
library(dplyr)

# Generate sample data
set.seed(123)
data <- data.frame(
  id = 1:200,
  treatment = rep(c("Control", "Treatment"), each = 100),
  site = sample(c("Site_A", "Site_B", "Site_C"), 200, replace = TRUE),
  var1 = rnorm(200, mean = c(50, 60)[factor(rep(c("Control", "Treatment"), each = 100))], sd = 10),
  var2 = rnorm(200, mean = c(30, 35)[factor(rep(c("Control", "Treatment"), each = 100))], sd = 8),
  var3 = rnorm(200, mean = 100, sd = 15),
  category = sample(c("A", "B", "C"), 200, replace = TRUE)
)

# =====================================================
# 1. GROUP COMPARISONS
# =====================================================

# Basic comparison with ANOVA text report
comparison <- compare_groups(
  data = data,
  category = treatment,
  Vars = c("var1", "var2", "var3"),
  posthoc = TRUE,
  bayesian = TRUE,
  verbose = FALSE
)

# View results
comparison$summary_table
cat(comparison$summary_data$anova_report[1])

# Stratified analysis by site
stratified <- compare_groups(
  data = data,
  category = treatment,
  Vars = c("var1", "var2"),
  repeat_category = site,
  verbose = FALSE
)

# =====================================================
# 2. CLUSTERING
# =====================================================

# Find optimal clusters
clusters <- clustering(
  data = data,
  vars = c("var1", "var2", "var3"),
  n_clusters = 2:4,
  scaling = "standardize"
)

# View model comparison
model_comparison_table(clusters)

# Add cluster assignments
data <- get_cluster_assignments(clusters)

# Assess stability
stability <- assess_cluster_stability(clusters, n_boot = 50)
print(stability)

# Generate report
generate_cluster_report(clusters)

# =====================================================
# 3. COMPARE CLUSTERS
# =====================================================

# Now compare the identified clusters on outcomes
cluster_comparison <- compare_groups(
  data = data,
  category = cluster,
  Vars = c("var1", "var2", "var3"),
  posthoc = TRUE
)

cat(cluster_comparison$summary_data$anova_report[1])

# =====================================================
# 4. CATEGORICAL ANALYSIS
# =====================================================

# Analyze treatment x category relationship
mosaic_result <- Mosaic(
  data = data,
  var1 = treatment,
  var2 = category
)

# =====================================================
# 5. NETWORK ANALYSIS
# =====================================================

network <- estimate_and_plot_network(
  data = data,
  variables = c("var1", "var2", "var3")
)
```

---

## Citation

If you use this package in your research, please cite:

```r
citation("Saqrmisc")
```

```
Saqr, M. (2024). Saqrmisc: Comprehensive Data Analysis and Visualization Tools for R.
GitHub: https://github.com/mohsaqr/Saqrmisc
```

## License

MIT License - see [LICENSE](LICENSE) for details.

## Author

**Mohammed Saqr**
- Email: saqr@saqr.me
- GitHub: [@mohsaqr](https://github.com/mohsaqr)

## Links

- **GitHub Repository**: https://github.com/mohsaqr/Saqrmisc
- **Issues**: https://github.com/mohsaqr/Saqrmisc/issues

---

**Made with care for the R community**
