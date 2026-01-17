# Saqrmisc: Comprehensive Data Analysis and Visualization for R

<!-- badges: start -->
[![R-CMD-check](https://github.com/mohsaqr/Saqrmisc/workflows/R-CMD-check/badge.svg)](https://github.com/mohsaqr/Saqrmisc/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-4.1.0+-blue.svg)](https://www.r-project.org/)
<!-- badges: end -->

## Overview

**Saqrmisc** is a comprehensive R package for statistical analysis, data transformation, and visualization. It provides a consistent, user-friendly API for common data analysis tasks with publication-ready outputs.

### Key Features

- **Consistent API**: All functions use quoted strings for variable names
- **Publication-Ready Output**: Beautiful gt tables and ggplot2 visualizations
- **Comprehensive Statistics**: From descriptives to advanced clustering and networks
- **Robust Error Handling**: Clear error messages and input validation

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
 "dplyr", "ggplot2", "gt", "tibble", "tidyr", "rlang",
 "MoEClust", "mclust", "vcd", "gridExtra",
 "bootnet", "qgraph", "mgm",
 "BayesFactor", "effectsize", "effsize"
))

# Optional (for full functionality)
install.packages(c("ggstatsplot", "TOSTER", "kableExtra"))

# For AI interpretation (pass function)
install.packages("httr2")
```

---

## Function Overview

| Category | Functions | Description |
|----------|-----------|-------------|
| **Correlations** | `correlations()`, `correlation_matrix()` | Full correlation tables, matrices with CIs |
| **Comparisons** | `compare_groups()` | t-tests, ANOVA, post-hoc, Bayesian, TOST |
| **Descriptives** | `descriptive_table()`, `categorical_table()`, `auto_describe()` | Summary statistics, frequency tables |
| **Transformation** | `center()`, `scale_vars()`, `standardize()`, `reverse_code()` | Data transformations with group support |
| **Missing Data** | `missing_analysis()`, `replace_missing()` | Missing patterns, MCAR test, imputation |
| **Outliers** | `outlier_check()`, `replace_outliers()` | Detection and treatment |
| **Clustering** | `clustering()`, `assess_cluster_stability()` | Model-based clustering with 14 models |
| **Networks** | `estimate_single_network()`, `compare_networks()` | Psychological network analysis |
| **Categorical** | `mosaic_analysis()` | Mosaic plots, chi-square, Cramer's V |
| **AI Interpretation** | `pass()` | Pipe R output to AI for scientific interpretation |

---

## Quick Start

```r
library(Saqrmisc)

# Use mtcars for examples
data(mtcars)

# Correlation matrix (auto-selects all numeric variables)
correlation_matrix(mtcars)

# Full correlation table with statistics
correlations(mtcars, Vars = c("mpg", "hp", "wt", "qsec"))

# Descriptive statistics
descriptive_table(mtcars, Vars = c("mpg", "hp", "wt"))

# Group comparisons
compare_groups(mtcars, category = "am", Vars = c("mpg", "hp"))

# Clustering
results <- clustering(mtcars, vars = c("mpg", "hp", "wt"), n_clusters = 2:4)
```

---

# Detailed Function Reference

## 1. Correlation Analysis

### `correlations()` - Full Correlation Table

Generates a comprehensive pairwise correlation table with complete statistics including correlation coefficients, confidence intervals, t-statistics, degrees of freedom, p-values, and sample sizes.

```r
# Basic usage - correlate all numeric variables
correlations(mtcars)

# Specify variables
correlations(mtcars, Vars = c("mpg", "hp", "wt", "qsec"))

# Partial correlations (controlling for other variables)
correlations(mtcars, Vars = c("mpg", "hp", "wt"), type = "partial")

# Spearman correlations
correlations(mtcars, Vars = c("mpg", "hp", "wt"), method = "spearman")

# Filter by significance or minimum correlation
correlations(mtcars, sig_only = TRUE)
correlations(mtcars, min_r = 0.5)

# Group-wise correlations
correlations(mtcars, Vars = c("mpg", "hp", "wt"), group_by = "cyl")

# Multilevel (within-cluster) correlations
correlations(mtcars, Vars = c("mpg", "hp", "wt"),
            multilevel = TRUE, id = "cyl")
```

**Output includes:**
- Correlation coefficient (r)
- 95% confidence interval
- t-statistic and degrees of freedom
- p-value with significance stars
- Sample size (n)

### `correlation_matrix()` - Correlation Matrix

Creates a publication-ready correlation matrix with significance stars and optional heatmap.

```r
# Basic matrix (auto-selects all numeric variables)
correlation_matrix(mtcars)

# Specify variables
correlation_matrix(mtcars, Vars = c("mpg", "hp", "wt", "qsec", "drat"))

# Include confidence intervals
correlation_matrix(mtcars, Vars = c("mpg", "hp", "wt"), show_ci = TRUE)

# Partial correlations
correlation_matrix(mtcars, Vars = c("mpg", "hp", "wt", "qsec"), type = "partial")

# Custom formatting
correlation_matrix(mtcars,
                  Vars = c("mpg", "hp", "wt"),
                  title = "Motor Trend Car Variables",
                  digits = 2,
                  theme = "colorful")
```

---

## 2. Group Comparisons

### `compare_groups()` - Statistical Group Comparisons

Compare groups using t-tests, ANOVA, or nonparametric alternatives with publication-ready visualizations.

```r
# Two-group comparison (automatic t-test)
results <- compare_groups(
 data = mtcars,
 category = "am",
 Vars = c("mpg", "hp", "wt")
)

# View results
results$plots$am_vs_mpg
results$summary_table
results$summary_data

# Three+ groups (automatic ANOVA with post-hoc)
results <- compare_groups(
 data = mtcars,
 category = "cyl",
 Vars = c("mpg", "hp"),
 posthoc = TRUE,
 posthoc_method = "tukey"
)

# View post-hoc comparisons
results$summary_data$posthoc_results[[1]]

# Full ANOVA text report (APA style)
cat(results$summary_data$anova_report[1])

# Nonparametric tests
results <- compare_groups(
 data = mtcars,
 category = "cyl",
 Vars = c("mpg"),
 type = "np"  # Kruskal-Wallis
)

# Bayesian analysis
results <- compare_groups(
 data = mtcars,
 category = "am",
 Vars = c("mpg"),
 bayesian = TRUE
)

# Equivalence testing (TOST)
results <- compare_groups(
 data = mtcars,
 category = "am",
 Vars = c("mpg"),
 equivalence = TRUE,
 equivalence_bounds = c(-0.5, 0.5)
)

# Stratified analysis
results <- compare_groups(
 data = mtcars,
 category = "am",
 Vars = c("mpg", "hp"),
 repeat_category = "cyl"
)

# Access stratified results
results$`4`$summary_data
results$`6`$plots$am_vs_mpg
```

**Parameters:**

| Parameter | Description | Default |
|-----------|-------------|---------|
| `category` | Grouping variable (quoted string) | Required |
| `Vars` | Numeric variables to compare | Required |
| `repeat_category` | Stratification variable | `NULL` |
| `type` | `"auto"`, `"p"`, `"np"`, `"bayes"` | `"auto"` |
| `posthoc` | Compute post-hoc tests? | `TRUE` |
| `posthoc_method` | `"games-howell"` or `"tukey"` | `"games-howell"` |
| `bayesian` | Compute Bayes Factors? | `FALSE` |
| `equivalence` | Perform TOST? | `FALSE` |

---

## 3. Descriptive Statistics

### `descriptive_table()` - Summary Statistics

Generate publication-ready descriptive statistics with flexible statistics selection.

```r
# Default statistics (n, mean, sd, median, min, max)
descriptive_table(mtcars, Vars = c("mpg", "hp", "wt"))

# Extended statistics
descriptive_table(
 mtcars,
 Vars = c("mpg", "hp", "wt"),
 stats = c("n", "mean", "sd", "se", "median", "iqr", "skewness", "kurtosis")
)

# Stratified by group
descriptive_table(
 mtcars,
 Vars = c("mpg", "hp"),
 group_by = "cyl",
 overall = TRUE
)

# Custom labels and themes
descriptive_table(
 mtcars,
 Vars = c("mpg", "hp", "wt"),
 labels = c(mpg = "Miles per Gallon", hp = "Horsepower", wt = "Weight (1000 lbs)"),
 title = "Motor Trend Car Data",
 theme = "colorful"
)
```

**Available Statistics:**
`n`, `missing`, `missing_pct`, `mean`, `sd`, `se`, `var`, `median`, `min`, `max`, `range`, `iqr`, `q1`, `q3`, `skewness`, `kurtosis`, `cv`

### `categorical_table()` - Frequency Tables

```r
# Single variable frequency table
categorical_table(mtcars, var = "cyl")

# Cross-tabulation with chi-square
categorical_table(
 mtcars,
 var = "cyl",
 by = "am",
 chi_square = TRUE,
 cramers_v = TRUE
)

# Stratified analysis
categorical_table(
 mtcars,
 var = "am",
 by = "gear",
 group_by = "cyl"
)
```

### `auto_describe()` - Automatic Description

Automatically detects and describes all variables in a data frame.

```r
# Describe all variables
results <- auto_describe(mtcars)

# With grouping
results <- auto_describe(mtcars, group_by = "am")

# Access results
results$numeric      # Numeric variables table
results$categorical  # List of categorical tables
results$variable_types
```

---

## 4. Data Transformation

All transformation functions support group-wise operations for multilevel data.

### `center()` - Mean Centering

```r
# Grand-mean centering
mtcars_c <- center(mtcars, Vars = c("mpg", "hp", "wt"))

# Group-mean centering
mtcars_c <- center(mtcars, Vars = c("mpg", "hp"), group_by = "cyl")

# With dplyr (vectorized version)
library(dplyr)
mtcars %>% mutate(mpg_c = center_vec(mpg))
mtcars %>% group_by(cyl) %>% mutate(mpg_c = center_vec(mpg))
```

### `standardize()` - Z-Score Standardization

```r
# Grand standardization
mtcars_z <- standardize(mtcars, Vars = c("mpg", "hp", "wt"))

# Group-wise standardization
mtcars_z <- standardize(mtcars, Vars = c("mpg", "hp"), group_by = "cyl")

# With dplyr
mtcars %>% mutate(mpg_z = standardize_vec(mpg))
```

### `scale_vars()` - Scaling

```r
# Scale by SD
mtcars_s <- scale_vars(mtcars, Vars = c("mpg", "hp"), method = "sd")

# Min-max scaling (0-1)
mtcars_s <- scale_vars(mtcars, Vars = c("mpg", "hp"), method = "range")

# Scale to custom range (1-10)
mtcars_s <- scale_vars(mtcars, Vars = c("mpg"), method = "range", range = c(1, 10))

# Group-wise scaling
mtcars_s <- scale_vars(mtcars, Vars = c("mpg"), method = "sd", group_by = "cyl")
```

### `reverse_code()` - Reverse Coding

For Likert scales and similar measures.

```r
# Create sample data
df <- data.frame(
 item1 = c(1, 2, 3, 4, 5),
 item2 = c(5, 4, 3, 2, 1),  # reverse-worded
 item3 = c(2, 3, 4, 3, 2)
)

# Reverse code with auto-detected scale
df <- reverse_code(df, Vars = "item2")

# Explicit 1-5 Likert scale
df <- reverse_code(df, Vars = c("item2", "item3"), min = 1, max = 5)

# With dplyr
df %>% mutate(item2_r = reverse_code_vec(item2, min = 1, max = 5))
```

---

## 5. Missing Data Analysis

### `missing_analysis()` - Comprehensive Missing Data Analysis

```r
# Add some missing values
df <- mtcars
df$mpg[c(1, 5, 10)] <- NA
df$hp[c(2, 5, 15)] <- NA

# Run analysis
results <- missing_analysis(df)

# Components
results$summary      # gt table with per-variable missing counts
results$patterns     # Missing data patterns
results$mcar         # Little's MCAR test results
results$plot         # Missing pattern visualization
```

### `replace_missing()` - Imputation

```r
# Replace with mean
df_imp <- replace_missing(df, Vars = "mpg", method = "mean")

# Replace with median
df_imp <- replace_missing(df, Vars = "mpg", method = "median")

# Group-wise imputation
df_imp <- replace_missing(df, Vars = "mpg", method = "mean", group_by = "cyl")

# Keep original and add new column
df_imp <- replace_missing(df, Vars = "mpg", method = "mean", suffix = "_imp")
```

---

## 6. Outlier Analysis

### `outlier_check()` - Detection

```r
# IQR method (default)
results <- outlier_check(mtcars, Vars = c("mpg", "hp", "wt"))

# Z-score method
results <- outlier_check(mtcars, Vars = c("mpg", "hp"), method = "zscore", threshold = 3)

# Mahalanobis distance (multivariate)
results <- outlier_check(mtcars, Vars = c("mpg", "hp", "wt"), method = "mahalanobis")

# Components
results$summary      # Outlier counts per variable
results$outliers     # Data frame of outlier rows
results$plot         # Box plot visualization
```

### `replace_outliers()` - Treatment

```r
# Winsorize outliers
mtcars_w <- replace_outliers(mtcars, Vars = c("mpg", "hp"), method = "winsorize")

# Replace with NA
mtcars_na <- replace_outliers(mtcars, Vars = c("mpg", "hp"), method = "na")

# Replace with median
mtcars_med <- replace_outliers(mtcars, Vars = c("mpg", "hp"), method = "median")
```

---

## 7. Normality Testing

### `normality_check()` - Normality Assessment

```r
# Check normality
results <- normality_check(mtcars, Vars = c("mpg", "hp", "wt"))

# Components
results$summary      # Test results (Shapiro-Wilk, skewness, kurtosis)
results$plots        # Q-Q plots and histograms
results$overall      # Overall assessment
```

---

## 8. Model-Based Clustering

### `clustering()` - Gaussian Mixture Models

```r
# Run clustering analysis
results <- clustering(
 data = mtcars,
 vars = c("mpg", "hp", "wt"),
 n_clusters = 2:5,
 scaling = "standardize"
)

# View results
print(results)
plot(results)                      # Profile plot
plot(results, type = "heatmap")    # Cluster means heatmap
plot(results, type = "comparison") # Model comparison (BIC)

# Model comparison table
model_comparison_table(results)

# Get cluster assignments
mtcars_clustered <- get_cluster_assignments(results)
mtcars_clustered <- get_cluster_assignments(results, include_probabilities = TRUE)

# Assess cluster stability
stability <- assess_cluster_stability(results, n_boot = 100)
print(stability)

# Generate report
generate_cluster_report(results)
```

**Covariance Models:**
The function tests 14 different covariance structures (EII, VII, EEI, VEI, EVI, VVI, EEE, VEE, EVE, VVE, EEV, VEV, EVV, VVV) representing different assumptions about volume, shape, and orientation of clusters.

---

## 9. Network Analysis

### `estimate_single_network()` - Network Estimation

```r
# Estimate network
results <- estimate_single_network(
 data = mtcars,
 variables = c("mpg", "hp", "wt", "qsec", "drat")
)

# Components
results$plot         # Network visualization
results$centrality   # Centrality measures
results$edges        # Edge weights
```

### `compare_networks()` - Network Comparison

```r
# Compare networks between groups
mtcars$am_factor <- factor(mtcars$am, labels = c("Automatic", "Manual"))

comparison <- compare_networks(
 data = mtcars,
 group_var = "am_factor",
 variables = c("mpg", "hp", "wt", "qsec")
)
```

---

## 10. Categorical Analysis

### `mosaic_analysis()` - Mosaic Plots

```r
# Analyze categorical relationship
mtcars$cyl_f <- factor(mtcars$cyl)
mtcars$am_f <- factor(mtcars$am, labels = c("Auto", "Manual"))

results <- mosaic_analysis(
 data = mtcars,
 var1 = "cyl_f",
 var2 = "am_f"
)

# Components
print(results)       # Summary statistics
summary(results)     # Detailed summary
results$plot         # Mosaic plot
results$chi_test     # Chi-square test results
results$cramers_v    # Effect size
```

---

## 11. AI Interpretation

### `pass()` - Pipe R Output to AI

Pass any R output (test results, model summaries, tables) to an AI model for publication-ready scientific interpretation.

```r
# Basic usage - pipe test results to AI
t.test(mpg ~ am, data = mtcars) |> pass()

# Regression with equation and references
lm(mpg ~ wt + hp, data = mtcars) |> summary() |> pass(action = "write")

# Correlation interpretation
cor.test(mtcars$mpg, mtcars$wt) |> pass(action = "write")

# ANOVA with post-hoc interpretation
aov(mpg ~ factor(cyl), data = mtcars) |> summary() |> pass(action = "write")
```

**Providers:**

```r
# Cloud providers
pass(result, provider = "anthropic")  # Claude (default)
pass(result, provider = "openai")     # GPT-4
pass(result, provider = "gemini")     # Gemini

# Local servers (LM Studio, Ollama, vLLM)
pass(result, base_url = "http://127.0.0.1:1234")
```

**Actions:**

```r
pass(result, action = "interpret")  # Interpret results (default)
pass(result, action = "explain")    # Explain methodology
pass(result, action = "write")      # Publication-ready Methods & Results
pass(result, action = "summarize")  # Brief summary
pass(result, action = "critique")   # Critical evaluation
pass(result, action = "suggest")    # Suggest follow-up analyses
```

**Styles:**

```r
pass(result, style = "scientific")  # APA-style (default)
pass(result, style = "simple")      # Plain language
pass(result, style = "detailed")    # Comprehensive (default for action="write")
pass(result, style = "brief")       # Key takeaway only
```

**Output Formats:**

```r
pass(result, output = "text")      # Plain text (default)
pass(result, output = "markdown")  # Markdown formatted
pass(result, output = "latex")     # LaTeX for papers
pass(result, output = "html")      # HTML formatted
```

**Customization:**

```r
# Add study context
t.test(score ~ group, data = mydata) |>
  pass(context = "RCT comparing drug vs placebo, N=200 patients")

# Custom AI instructions
result |> pass(system_message = "Focus on clinical implications")

# Specific request
result |> pass(prompt = "Explain the interaction effect")

# Combine all
lm(outcome ~ treatment * age, data = mydata) |> summary() |>
  pass(
    action = "write",
    output = "latex",
    context = "Phase 3 clinical trial for hypertension",
    system_message = "Emphasize clinical significance",
    prompt = "Focus on the treatment-age interaction"
  )
```

**Example Output (action = "write"):**

```
**Methods**

A multiple linear regression was conducted to examine the relationship between
miles per gallon (mpg) and two predictor variables: vehicle weight (wt) and
horsepower (hp). The analysis was performed in R (R Core Team, 2024) using the
base `lm()` function. The regression equation is:

  mpg = β₀ + β₁(wt) + β₂(hp) + ε

**Results**

The regression model was statistically significant, F(2, 29) = 69.21, p < .001.
The model explained 82.7% of variance in mpg (R² = .827, Adjusted R² = .815).

| Predictor | Estimate | SE   | t      | p      |
|-----------|----------|------|--------|--------|
| Intercept | 37.23    | 1.60 | 23.29  | < .001 |
| wt        | –3.88    | 0.63 | –6.13  | < .001 |
| hp        | –0.032   | 0.009| –3.52  | .001   |

**References**

R Core Team. (2024). R: A language and environment for statistical computing.
R Foundation for Statistical Computing. https://www.R-project.org/
```

**Setup:**

```r
# Set API key for session
set_api_key("your-api-key", provider = "anthropic")
set_api_key("your-api-key", provider = "openai")
set_api_key("your-api-key", provider = "gemini")

# Or use environment variables (recommended)
# Add to .Renviron:
# ANTHROPIC_API_KEY=your-key
# OPENAI_API_KEY=your-key
# GEMINI_API_KEY=your-key
```

---

## API Design

All functions use a **consistent quoted-string API**:

```r
# Variable names as quoted strings
compare_groups(data, category = "gender", Vars = c("score1", "score2"))
correlations(data, Vars = c("x", "y", "z"), group_by = "group")
center(data, Vars = "score", group_by = "cluster")

# NULL for auto-selection of all numeric variables
correlations(data)  # Correlates all numeric variables
correlation_matrix(data)  # Matrix of all numeric variables
```

---

## Citation

If you use this package in your research, please cite:

```r
citation("Saqrmisc")
```

```
Saqr, M. (2025). Saqrmisc: Comprehensive Data Analysis and Visualization Tools for R.
GitHub: https://github.com/mohsaqr/Saqrmisc
```

---

## License

MIT License - see [LICENSE](LICENSE) for details.

## Author

**Mohammed Saqr**
- Email: saqr@saqr.me
- GitHub: [@mohsaqr](https://github.com/mohsaqr)

## Links

- **GitHub Repository**: https://github.com/mohsaqr/Saqrmisc
- **Bug Reports**: https://github.com/mohsaqr/Saqrmisc/issues

---

**Made with care for the R community**
