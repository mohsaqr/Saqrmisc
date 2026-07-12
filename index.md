# Saqrmisc

A comprehensive R package for statistical analysis, data transformation,
and visualization with publication-ready outputs. All functions use a
consistent quoted-string API and support multiple output formats (gt,
plain, markdown, latex, kable).

## Table of Contents

- [Installation](#installation)
- [Correlation Analysis](#correlation-analysis)
  - [correlations()](#correlations)
  - [correlation_matrix()](#correlation_matrix)
- [Group Comparisons](#group-comparisons)
  - [compare_groups()](#compare_groups)
- [Descriptive Statistics](#descriptive-statistics)
  - [descriptive_table()](#descriptive_table)
  - [categorical_table()](#categorical_table)
  - [auto_describe()](#auto_describe)
- [Data Transformation](#data-transformation)
  - [center()](#center)
  - [standardize()](#standardize)
  - [scale_vars()](#scale_vars)
  - [reverse_code()](#reverse_code)
- [Missing Data](#missing-data)
  - [missing_analysis()](#missing_analysis)
  - [replace_missing()](#replace_missing)
- [Outlier Analysis](#outlier-analysis)
  - [outlier_check()](#outlier_check)
  - [replace_outliers()](#replace_outliers)
- [Normality Testing](#normality-testing)
  - [normality_check()](#normality_check)
- [Clustering](#clustering)
  - [clustering()](#clustering-1)
  - [assess_cluster_stability()](#assess_cluster_stability)
- [Network Analysis](#network-analysis)
  - [estimate_single_network()](#estimate_single_network)
  - [compare_networks()](#compare_networks)
- [Categorical Analysis](#categorical-analysis)
  - [mosaic_analysis()](#mosaic_analysis)
- [AI Interpretation](#ai-interpretation)
  - [pass()](#pass)
- [Output Formats](#output-formats)
- [Vectorized Functions](#vectorized-functions)
- [Citation](#citation)

------------------------------------------------------------------------

## Installation

``` r

devtools::install_github("mohsaqr/Saqrmisc")
library(Saqrmisc)
```

**Dependencies:**

``` r

install.packages(c(
  "dplyr", "ggplot2", "gt", "tibble", "tidyr", "rlang",
  "MoEClust", "mclust", "vcd", "gridExtra",
  "bootnet", "qgraph", "mgm",
  "BayesFactor", "effectsize", "effsize"
))

# Optional
install.packages(c("ggstatsplot", "TOSTER", "kableExtra", "httr2"))
```

------------------------------------------------------------------------

## Correlation Analysis

### `correlations()`

Full pairwise correlation table with coefficients, CIs, t-statistics,
df, p-values, and sample sizes.

``` r

correlations(data, Vars = NULL, type = "bivariate", method = "pearson",
             p_adjust = "none", ci_level = 0.95, min_r = NULL, sig_only = FALSE,
             multilevel = FALSE, id = NULL, between = FALSE, group_by = NULL,
             include = NULL, exclude = NULL, digits = 3, format = "gt")
```

| Parameter    | Description                                   | Default       |
|--------------|-----------------------------------------------|---------------|
| `Vars`       | Variables to correlate (`NULL` = all numeric) | `NULL`        |
| `type`       | `"bivariate"`, `"partial"`, `"semi-partial"`  | `"bivariate"` |
| `method`     | `"pearson"`, `"spearman"`, `"kendall"`        | `"pearson"`   |
| `p_adjust`   | `"none"`, `"bonferroni"`, `"holm"`, `"fdr"`   | `"none"`      |
| `min_r`      | Show only correlations above this threshold   | `NULL`        |
| `sig_only`   | Show only significant correlations            | `FALSE`       |
| `multilevel` | Within-cluster correlations (requires `id`)   | `FALSE`       |
| `group_by`   | Stratified correlations by group              | `NULL`        |

``` r

correlations(mtcars, Vars = c("mpg", "hp", "wt"))
correlations(mtcars, Vars = c("mpg", "hp", "wt"), type = "partial", sig_only = TRUE)
```

**Returns:** List with `$table`, `$data`, `$n_pairs`, `$n_significant`.

------------------------------------------------------------------------

### `correlation_matrix()`

Publication-ready correlation matrix with significance stars and
optional heatmap.

``` r

correlation_matrix(data, Vars = NULL, type = "bivariate", method = "pearson",
                   triangle = "lower", diagonal = "dash", show_n = FALSE,
                   show_ci = FALSE, show_p = FALSE, p_adjust = "none",
                   stars = TRUE, heatmap = FALSE, digits = 2, format = "gt")
```

| Parameter  | Description                    | Default   |
|------------|--------------------------------|-----------|
| `triangle` | `"lower"`, `"upper"`, `"full"` | `"lower"` |
| `show_ci`  | Display confidence intervals   | `FALSE`   |
| `show_n`   | Display pairwise sample sizes  | `FALSE`   |
| `heatmap`  | Generate heatmap visualization | `FALSE`   |

``` r

correlation_matrix(mtcars, Vars = c("mpg", "hp", "wt", "qsec"), show_ci = TRUE)
```

**Returns:** List with `$table`, `$correlation_matrix`, `$p_matrix`,
`$heatmap`.

------------------------------------------------------------------------

## Group Comparisons

### `compare_groups()`

Compare groups using t-tests, ANOVA, or nonparametric alternatives with
publication-ready tables and plots. Automatically selects the
appropriate test based on group count and data properties.

``` r

compare_groups(data, category, Vars = NULL, repeat_category = NULL,
               compare_by = NULL, compare_mode = "between", type = "auto",
               posthoc = TRUE, posthoc_method = "games-howell",
               bayesian = FALSE, equivalence = FALSE, equivalence_bounds = NULL,
               pivot = FALSE, pivot_stat = "mean", format = "gt")
```

| Parameter | Description | Default |
|----|----|----|
| `category` | Grouping variable (required) | – |
| `Vars` | Numeric variables to compare (`NULL` = all) | `NULL` |
| `type` | `"auto"`, `"p"` (parametric), `"np"` (nonparametric), `"bayes"` | `"auto"` |
| `posthoc` | Compute post-hoc tests (3+ groups) | `TRUE` |
| `posthoc_method` | `"games-howell"` or `"tukey"` | `"games-howell"` |
| `bayesian` | Compute Bayes Factors | `FALSE` |
| `equivalence` | Perform TOST equivalence testing | `FALSE` |
| `repeat_category` | Stratify analysis by this variable | `NULL` |
| `pivot` | Compact table with categories as columns | `FALSE` |
| `pivot_stat` | `"mean"`, `"mean_sd"`, `"median"`, `"n"` | `"mean"` |
| `compare_by` | Factors to test within each category | `NULL` |
| `compare_mode` | `"between"` or `"within"` | `"between"` |

``` r

# Two-group comparison (t-test)
results <- compare_groups(mtcars, category = "am", Vars = c("mpg", "hp"))

# ANOVA with post-hoc
results <- compare_groups(mtcars, category = "cyl", Vars = c("mpg", "hp"))
```

**Returns:** List with `$summary_table`, `$summary_data`, `$plots`,
`$pivot_table`, `$within_table`.

#### Pivot Tables

Compact summary with category levels as columns and a `p (ES)` column:

``` r

results <- compare_groups(df, category = "group", Vars = c("x", "y"),
                          pivot = TRUE, pivot_stat = "mean_sd")
results$pivot_table
```

#### Within-Group Analysis

Test whether factors have effects within each category level:

``` r

results <- compare_groups(df, category = "llm", Vars = "score",
                          compare_by = c("pronoun", "support"),
                          compare_mode = "within")
results$within_table
```

------------------------------------------------------------------------

## Descriptive Statistics

### `descriptive_table()`

Flexible summary statistics table with selectable statistics.

``` r

descriptive_table(data, Vars = NULL,
                  stats = c("n", "mean", "sd", "median", "min", "max"),
                  group_by = NULL, overall = FALSE, labels = NULL,
                  digits = 3, format = "gt")
```

| Parameter | Description | Default |
|----|----|----|
| `stats` | Statistics to include (see below) | `c("n","mean","sd","median","min","max")` |
| `group_by` | Compute statistics by group | `NULL` |
| `overall` | Include overall/total row | `FALSE` |
| `labels` | Named vector of variable labels | `NULL` |

**Available statistics:** `n`, `missing`, `missing_pct`, `mean`, `sd`,
`se`, `var`, `median`, `min`, `max`, `range`, `iqr`, `q1`, `q3`,
`skewness`, `kurtosis`, `cv`

``` r

descriptive_table(mtcars, Vars = c("mpg", "hp", "wt"), group_by = "cyl", overall = TRUE)
```

------------------------------------------------------------------------

### `categorical_table()`

Frequency and cross-tabulation tables with optional chi-square test.

``` r

categorical_table(data, var, by = NULL, chi_square = FALSE,
                  cramers_v = FALSE, group_by = NULL, format = "gt")
```

``` r

categorical_table(mtcars, var = "cyl", by = "am", chi_square = TRUE, cramers_v = TRUE)
```

------------------------------------------------------------------------

### `auto_describe()`

Automatically detects variable types and produces appropriate summaries
for each.

``` r

auto_describe(data, group_by = NULL, format = "gt")
```

**Returns:** List with `$numeric`, `$categorical`, `$variable_types`.

------------------------------------------------------------------------

## Data Transformation

All transformation functions add new columns with a configurable suffix
and support group-wise operations via `group_by`. Each has a `_vec()`
variant for use in
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

### `center()`

Mean centering (grand-mean or group-mean).

``` r

center(data, Vars, suffix = "_c", group_by = NULL)
```

``` r

center(mtcars, Vars = c("mpg", "hp"), group_by = "cyl")
```

------------------------------------------------------------------------

### `standardize()`

Z-score standardization.

``` r

standardize(data, Vars, suffix = "_z", group_by = NULL)
```

------------------------------------------------------------------------

### `scale_vars()`

Scale by standard deviation or rescale to a custom range.

``` r

scale_vars(data, Vars, method = "sd", range = c(0, 1), suffix = "_s", group_by = NULL)
```

| Parameter | Description                                  | Default   |
|-----------|----------------------------------------------|-----------|
| `method`  | `"sd"` (divide by SD) or `"range"` (min-max) | `"sd"`    |
| `range`   | Target range for `"range"` method            | `c(0, 1)` |

------------------------------------------------------------------------

### `reverse_code()`

Reverse code items for Likert scales. Auto-detects or accepts explicit
`min`/`max`.

``` r

reverse_code(data, Vars, min = NULL, max = NULL, suffix = "_r")
```

``` r

reverse_code(df, Vars = c("item2", "item5"), min = 1, max = 5)
```

------------------------------------------------------------------------

## Missing Data

### `missing_analysis()`

Comprehensive missing data diagnostics including patterns, Little’s MCAR
test, and visualizations.

``` r

missing_analysis(data, Vars = NULL, pattern_plot = TRUE,
                 mcar_test = TRUE, correlations = FALSE, digits = 2)
```

**Returns:** List with `$summary`, `$patterns`, `$mcar`, `$plot`.

------------------------------------------------------------------------

### `replace_missing()`

Simple imputation by mean or median, with optional group-wise
computation.

``` r

replace_missing(data, Vars, method = "mean", group_by = NULL, suffix = "_imp")
```

| Parameter | Description                                        | Default  |
|-----------|----------------------------------------------------|----------|
| `method`  | `"mean"` or `"median"`                             | `"mean"` |
| `suffix`  | Suffix for new column (`NULL` to replace in-place) | `"_imp"` |

------------------------------------------------------------------------

## Outlier Analysis

### `outlier_check()`

Detect outliers using multiple methods.

``` r

outlier_check(data, Vars, method = "zscore", threshold = 3,
              flag = TRUE, plot = TRUE)
```

| Parameter | Description | Default |
|----|----|----|
| `method` | `"zscore"`, `"iqr"`, `"percentile"`, `"mahalanobis"` | `"zscore"` |
| `threshold` | Detection threshold (method-dependent) | `3` |

**Returns:** List with `$summary`, `$data`, `$outlier_indices`, `$plot`.

------------------------------------------------------------------------

### `replace_outliers()`

Treat outliers by winsorizing, replacing with NA, or substituting with
median/mean.

``` r

replace_outliers(data, Vars, method = "winsorize", threshold = 0.05,
                 suffix = "_treated")
```

| Parameter | Description                                 | Default       |
|-----------|---------------------------------------------|---------------|
| `method`  | `"winsorize"`, `"na"`, `"median"`, `"mean"` | `"winsorize"` |

------------------------------------------------------------------------

## Normality Testing

### `normality_check()`

Shapiro-Wilk test, skewness, kurtosis, Q-Q plots, and histograms.

``` r

normality_check(data, Vars, digits = 3)
```

**Returns:** List with `$summary`, `$plots`, `$overall`.

------------------------------------------------------------------------

## Clustering

### `clustering()`

Model-based clustering using Gaussian Mixture Models. Tests 14
covariance structures (EII through VVV) and selects the best model by
BIC.

``` r

clustering(data, vars, n_clusters, scaling = "standardize",
           n_init = 10, seed = NULL, verbose = TRUE)
```

| Parameter | Description | Default |
|----|----|----|
| `n_clusters` | Range of clusters to test (e.g., `2:5`) | required |
| `scaling` | `"standardize"`, `"center"`, `"minmax"`, `"none"` | `"standardize"` |

``` r

results <- clustering(mtcars, vars = c("mpg", "hp", "wt"), n_clusters = 2:4)
plot(results)                          # Profile plot
model_comparison_table(results)        # Compare all models
```

**Returns:** Object of class `"moe_analysis"` with
[`print()`](https://rdrr.io/r/base/print.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods.

#### Helper Functions

| Function | Description |
|----|----|
| `get_cluster_assignments(results)` | Extract cluster labels (optionally with probabilities) |
| `assess_cluster_stability(results, n_boot)` | Bootstrap stability assessment (Jaccard similarity) |
| `model_comparison_table(results)` | Publication-ready model comparison table |
| `generate_cluster_report(results)` | Full text report of the analysis |

------------------------------------------------------------------------

## Network Analysis

### `estimate_single_network()`

Estimate psychological networks using EBICglasso regularized partial
correlations.

``` r

estimate_single_network(df, Vars = NULL, layout = "circle",
                        default = "EBICglasso", verbose = TRUE,
                        compute_centrality = TRUE)
```

**Returns:** Object with `$plot`, `$centrality`, `$edges`,
`$network_obj`.

------------------------------------------------------------------------

### `compare_networks()`

Estimate and compare networks between groups.

``` r

compare_networks(data, group_var, variables = NULL,
                 method = "EBICglasso", layout = "circle")
```

------------------------------------------------------------------------

## Categorical Analysis

### `mosaic_analysis()`

Mosaic plot with chi-square test and Cramer’s V.

``` r

mosaic_analysis(data, var1, var2, show_percentages = TRUE,
                percentage_base = "total")
```

**Returns:** Object with `$plot`, `$chi_test`, `$cramers_v`, `$summary`.

------------------------------------------------------------------------

## AI Interpretation

### `pass()`

Pipe any R output to an AI model for scientific interpretation.

``` r

pass(result, action = "interpret", style = "scientific", output = "text",
     provider = "openai", model = NULL, context = NULL,
     system_message = NULL, prompt = NULL, base_url = NULL)
```

| Parameter | Options | Default |
|----|----|----|
| `action` | `"interpret"`, `"explain"`, `"write"`, `"summarize"`, `"critique"`, `"suggest"` | `"interpret"` |
| `style` | `"scientific"`, `"simple"`, `"detailed"`, `"brief"` | `"scientific"` |
| `output` | `"text"`, `"markdown"`, `"latex"`, `"html"` | `"text"` |
| `provider` | `"openai"`, `"anthropic"`, `"gemini"`, `"openrouter"`, or use `base_url` for local | `"openai"` |

``` r

t.test(mpg ~ am, data = mtcars) |> pass()
lm(mpg ~ wt + hp, data = mtcars) |> summary() |> pass(action = "write")
```

#### API Key Setup

``` r

set_openai_key("sk-...")
set_claude_key("sk-ant-...")
set_gemini_key("AIza...")
set_openrouter_key("sk-or-...")

# Or via environment variables in .Renviron:
# OPENAI_API_KEY=your-key
# ANTHROPIC_API_KEY=your-key
```

------------------------------------------------------------------------

## Output Formats

All table-producing functions share these parameters:

| Parameter | Options | Default |
|----|----|----|
| `format` | `"gt"`, `"plain"`, `"markdown"`, `"latex"`, `"kable"` | `"gt"` |
| `show_header` | `TRUE` / `FALSE` | `TRUE` |

Results can also be converted after the fact with
[`to_gt()`](https://pak.dynasite.org/Saqrmisc/reference/to_gt.md),
[`to_markdown()`](https://pak.dynasite.org/Saqrmisc/reference/to_markdown.md),
[`to_latex()`](https://pak.dynasite.org/Saqrmisc/reference/to_latex.md),
[`to_html()`](https://pak.dynasite.org/Saqrmisc/reference/to_html.md),
[`to_kable()`](https://pak.dynasite.org/Saqrmisc/reference/to_kable.md),
and
[`to_dataframe()`](https://pak.dynasite.org/Saqrmisc/reference/to_dataframe.md).

------------------------------------------------------------------------

## Vectorized Functions

For use in
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
and [`across()`](https://dplyr.tidyverse.org/reference/across.html)
workflows:

| Function                           | Description          |
|------------------------------------|----------------------|
| `center_vec(x)`                    | Mean-center a vector |
| `standardize_vec(x)`               | Z-score standardize  |
| `scale_vec(x, method, range)`      | Scale by SD or range |
| `reverse_code_vec(x, min, max)`    | Reverse code         |
| `winsorize_vec(x, probs)`          | Cap extreme values   |
| `is_outlier(x, method, threshold)` | Logical outlier flag |

These respect
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
for group-wise operations.

------------------------------------------------------------------------

## Citation

    Saqr, M. (2025). Saqrmisc: Comprehensive Data Analysis and Visualization Tools for R.
    GitHub: https://github.com/mohsaqr/Saqrmisc

## License

MIT License - see [LICENSE](https://pak.dynasite.org/Saqrmisc/LICENSE)
for details.

## Author

**Mohammed Saqr** - <saqr@saqr.me> -
[@mohsaqr](https://github.com/mohsaqr)
