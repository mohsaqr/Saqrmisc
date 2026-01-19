# Saqrmisc Features Reference

A comprehensive list of all features in the Saqrmisc package.

---

## Quick Reference Table

| Category | Function | Description |
|----------|----------|-------------|
| **Correlations** | `correlations()` | Full pairwise correlation table with CIs, p-values |
| | `correlation_matrix()` | Publication-ready correlation matrix with stars |
| **Comparisons** | `compare_groups()` | t-tests, ANOVA, post-hoc, Bayesian, TOST, pivot tables |
| **Descriptives** | `descriptive_table()` | Summary statistics (n, mean, sd, median, etc.) |
| | `categorical_table()` | Frequency tables with chi-square |
| | `auto_describe()` | Auto-detect and describe all variables |
| **Transformation** | `center()` | Mean centering (grand or group-wise) |
| | `standardize()` | Z-score standardization |
| | `scale_vars()` | Scaling (SD, range, custom) |
| | `reverse_code()` | Reverse coding for Likert scales |
| **Missing Data** | `missing_analysis()` | Missing patterns, MCAR test, visualization |
| | `replace_missing()` | Imputation (mean, median, group-wise) |
| **Outliers** | `outlier_check()` | Detection (IQR, z-score, Mahalanobis) |
| | `replace_outliers()` | Treatment (winsorize, NA, median) |
| **Normality** | `normality_check()` | Shapiro-Wilk, skewness, kurtosis, Q-Q plots |
| **Clustering** | `clustering()` | Model-based clustering (14 covariance models) |
| | `assess_cluster_stability()` | Bootstrap stability assessment |
| **Networks** | `estimate_single_network()` | Network estimation and visualization |
| | `compare_networks()` | Compare networks between groups |
| **Categorical** | `mosaic_analysis()` | Mosaic plots, chi-square, Cramer's V |
| **AI** | `pass()` | Pipe R output to AI for interpretation |

---

## Correlation Analysis

### `correlations()`
Full pairwise correlation table with complete statistics.

**Features:**
- Pearson, Spearman, Kendall correlations
- Partial correlations (controlling for other variables)
- Confidence intervals (95% CI)
- t-statistics and degrees of freedom
- p-values with significance stars
- Sample size per pair
- Filter by significance (`sig_only = TRUE`)
- Filter by minimum correlation (`min_r = 0.5`)
- Group-wise correlations (`group_by = "group"`)
- Multilevel/within-cluster correlations (`multilevel = TRUE`)

### `correlation_matrix()`
Publication-ready correlation matrix.

**Features:**
- Lower triangle with significance stars
- Optional confidence intervals (`show_ci = TRUE`)
- Partial correlation matrix
- Multiple themes (default, colorful, minimal)
- Custom titles and digits

---

## Group Comparisons

### `compare_groups()`
Comprehensive group comparison function.

**Statistical Tests:**
- 2 groups: t-test (parametric) or Mann-Whitney U (nonparametric)
- 3+ groups: ANOVA (parametric) or Kruskal-Wallis (nonparametric)
- Bayesian t-test and ANOVA (`bayesian = TRUE`)
- Equivalence testing/TOST (`equivalence = TRUE`)

**Post-hoc Tests:**
- Tukey HSD (`posthoc_method = "tukey"`)
- Games-Howell (`posthoc_method = "games-howell"`)
- Pairwise Wilcoxon (nonparametric)
- Adjustable p-value correction (`p_adjust_method`)

**Analysis Modes:**
- Between-group (`compare_mode = "between"`) - Default
- Within-group (`compare_mode = "within"`) - Test factors within each category

**Pivot Tables:**
- Compact table with categories as columns (`pivot = TRUE`)
- Statistics: mean, mean_sd, median, n (`pivot_stat`)
- p-value and effect size column (`p (ES)`)
- Stratified pivot tables with `repeat_category`

**Stratified Analysis:**
- Repeat analysis for each level of a variable (`repeat_category`)
- Combined summary tables
- Filter levels by threshold or explicit list

**Output:**
- Publication-ready gt tables
- ggstatsplot visualizations
- Multiple plot styles (points, boxplot, bar, violin)
- ANOVA text reports (APA style)

---

## Descriptive Statistics

### `descriptive_table()`
Flexible summary statistics table.

**Available Statistics:**
`n`, `missing`, `missing_pct`, `mean`, `sd`, `se`, `var`, `median`, `min`, `max`, `range`, `iqr`, `q1`, `q3`, `skewness`, `kurtosis`, `cv`

**Features:**
- Custom statistic selection
- Group-wise statistics (`group_by`)
- Overall row option (`overall = TRUE`)
- Custom variable labels
- Multiple themes

### `categorical_table()`
Frequency and cross-tabulation tables.

**Features:**
- Single variable frequencies
- Cross-tabulation (`by = "variable"`)
- Chi-square test (`chi_square = TRUE`)
- Cramer's V effect size (`cramers_v = TRUE`)
- Stratified tables (`group_by`)

### `auto_describe()`
Automatic variable description.

**Features:**
- Auto-detect numeric vs categorical
- Appropriate statistics for each type
- Group-wise descriptions
- Combined output object

---

## Data Transformation

### `center()`
Mean centering.

**Features:**
- Grand-mean centering
- Group-mean centering (`group_by`)
- Vectorized version for dplyr: `center_vec()`

### `standardize()`
Z-score standardization.

**Features:**
- Grand standardization
- Group-wise standardization (`group_by`)
- Vectorized version: `standardize_vec()`

### `scale_vars()`
Flexible scaling.

**Methods:**
- `"sd"` - Divide by standard deviation
- `"range"` - Min-max scaling (0-1 default)
- Custom range: `range = c(1, 10)`
- Group-wise scaling

### `reverse_code()`
Reverse coding for scales.

**Features:**
- Auto-detect scale range
- Explicit min/max specification
- Vectorized version: `reverse_code_vec()`

---

## Missing Data

### `missing_analysis()`
Comprehensive missing data analysis.

**Output:**
- Per-variable missing counts and percentages
- Missing data patterns
- Little's MCAR test
- Missing pattern visualization

### `replace_missing()`
Missing data imputation.

**Methods:**
- `"mean"` - Replace with mean
- `"median"` - Replace with median
- Group-wise imputation (`group_by`)
- Keep original with suffix (`suffix = "_imp"`)

---

## Outlier Analysis

### `outlier_check()`
Outlier detection.

**Methods:**
- `"iqr"` - Interquartile range (1.5 * IQR)
- `"zscore"` - Z-score threshold
- `"mahalanobis"` - Multivariate outliers

**Output:**
- Summary counts per variable
- Data frame of outlier rows
- Box plot visualization

### `replace_outliers()`
Outlier treatment.

**Methods:**
- `"winsorize"` - Cap at percentile
- `"na"` - Replace with NA
- `"median"` - Replace with median
- `"mean"` - Replace with mean

---

## Normality Testing

### `normality_check()`
Normality assessment.

**Tests:**
- Shapiro-Wilk test
- Skewness and kurtosis
- Q-Q plots
- Histograms with density

---

## Clustering

### `clustering()`
Model-based clustering using Gaussian Mixture Models.

**Features:**
- Test range of cluster numbers (`n_clusters = 2:5`)
- 14 covariance structures (EII to VVV)
- Automatic model selection (BIC)
- Multiple scaling options
- Profile plots and heatmaps
- Cluster assignments with probabilities

**Covariance Models:**
EII, VII, EEI, VEI, EVI, VVI, EEE, VEE, EVE, VVE, EEV, VEV, EVV, VVV

### `assess_cluster_stability()`
Bootstrap stability assessment.

**Output:**
- Jaccard similarity coefficients
- Cluster-wise stability
- Overall stability summary

---

## Network Analysis

### `estimate_single_network()`
Estimate psychological networks.

**Features:**
- EBICglasso regularization
- Network visualization
- Centrality measures (strength, betweenness, closeness)
- Edge weights

### `compare_networks()`
Compare networks between groups.

**Features:**
- Network comparison test
- Side-by-side visualization
- Centrality comparison

---

## Categorical Analysis

### `mosaic_analysis()`
Mosaic plot analysis.

**Output:**
- Mosaic plot visualization
- Chi-square test
- Cramer's V effect size
- Residuals analysis

---

## AI Interpretation

### `pass()`
Pipe R output to AI for scientific interpretation.

**Providers:**
- OpenAI (GPT-4, GPT-4o)
- Anthropic (Claude)
- Google (Gemini)
- OpenRouter (multiple models)
- Local servers (LM Studio, Ollama, vLLM)

**Actions:**
- `"interpret"` - Interpret results (default)
- `"explain"` - Explain methodology
- `"write"` - Publication-ready Methods & Results
- `"summarize"` - Brief summary
- `"critique"` - Critical evaluation
- `"suggest"` - Suggest follow-up analyses

**Styles:**
- `"scientific"` - APA style (default)
- `"simple"` - Plain language
- `"detailed"` - Comprehensive
- `"brief"` - Key takeaway only

**Output Formats:**
- `"text"` - Plain text (default)
- `"markdown"` - Markdown
- `"latex"` - LaTeX
- `"html"` - HTML

**Setup Functions:**
- `set_openai_key()`
- `set_claude_key()`
- `set_gemini_key()`
- `set_openrouter_key()`
- `set_api_key()`

---

## Output Formats

All table functions support multiple output formats:

| Format | Description | Use Case |
|--------|-------------|----------|
| `"gt"` | Publication-ready gt table | Default, interactive |
| `"plain"` | Raw data frame | Further processing |
| `"markdown"` | Markdown table | R Markdown, GitHub |
| `"latex"` | LaTeX tabular | Academic papers |
| `"kable"` | knitr::kable | R Markdown documents |

**Header Control:**
- `show_header = TRUE` - Show title/subtitle (default)
- `show_header = FALSE` - Hide title/subtitle

---

## Vectorized Functions for dplyr

Use within `mutate()` for tidyverse workflows:

```r
library(dplyr)

df %>%
  mutate(
    x_centered = center_vec(x),
    x_standardized = standardize_vec(x),
    x_reversed = reverse_code_vec(x, min = 1, max = 5)
  )

df %>%
  group_by(group) %>%
  mutate(
    x_group_centered = center_vec(x),
    x_group_standardized = standardize_vec(x)
  )
```

---

## API Design Principles

1. **Quoted Strings**: All variable names as quoted strings
   ```r
   compare_groups(data, category = "group", Vars = c("x", "y"))
   ```

2. **NULL for Auto-Selection**: Omit `Vars` to select all numeric variables
   ```r
   correlations(data)  # All numeric variables
   ```

3. **Consistent Parameters**: Same parameter names across functions
   - `Vars` - Variables to analyze
   - `group_by` - Grouping variable
   - `format` - Output format
   - `show_header` - Show/hide header

4. **Sensible Defaults**: Functions work with minimal arguments
   ```r
   compare_groups(data, category = "group")  # Uses all numeric Vars
   ```

---

## Version

Current version: 0.9.0

## Author

Mohammed Saqr (saqr@saqr.me)

## Links

- GitHub: https://github.com/mohsaqr/Saqrmisc
- Issues: https://github.com/mohsaqr/Saqrmisc/issues
