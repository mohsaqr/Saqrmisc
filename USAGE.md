# Saqrmisc Package Usage Guide

## Quick Start

### Installation

```r
# Install from GitHub
devtools::install_github("mohsaqr/Saqrmisc")

# Load the package
library(Saqrmisc)
```

### Basic Examples

#### 1. Clustering Analysis

```r
# Generate sample data
set.seed(123)
data <- data.frame(
  var1 = rnorm(100, mean = c(2, 5, 8)[sample(1:3, 100, replace = TRUE)]),
  var2 = rnorm(100, mean = c(1, 4, 7)[sample(1:3, 100, replace = TRUE)]),
  var3 = rnorm(100, mean = c(3, 6, 9)[sample(1:3, 100, replace = TRUE)])
)

# Run clustering analysis
results <- run_full_moe_analysis(
  input_data = data,
  cluster_vars = c("var1", "var2", "var3"),
  n_clusters = 3
)

# View results
view_results(results, what = "plots", scale = "original")
```

#### 2. Comparison Analysis

```r
# Generate sample data
data <- data.frame(
  group = sample(c("Control", "Treatment"), 100, replace = TRUE),
  score1 = rnorm(100, mean = c(50, 60)[factor(sample(c("Control", "Treatment"), 100, replace = TRUE))]),
  score2 = rnorm(100, mean = c(30, 40)[factor(sample(c("Control", "Treatment"), 100, replace = TRUE))])
)

# Run comparison analysis
results <- generate_comparison_plots(
  data = data,
  category = group,
  Vars = c("score1", "score2"),
  plots = TRUE,
  table = TRUE
)
```

#### 3. Mosaic Analysis

```r
# Generate sample data
data <- data.frame(
  gender = sample(c("Male", "Female"), 200, replace = TRUE),
  education = sample(c("High School", "Bachelor", "Master"), 200, replace = TRUE)
)

# Run mosaic analysis
results <- mosaic_analysis(
  data = data,
  var1 = gender,
  var2 = education,
  min_count = 10,
  title = "Gender vs Education"
)
```

## Function Reference

### run_full_moe_analysis()

**Purpose**: Comprehensive model-based clustering analysis

**Parameters**:
- `input_data`: Data frame with your data
- `cluster_vars`: Character vector of variable names for clustering
- `n_clusters`: Number of clusters to find
- `scaling_method`: "standardize", "center", "minmax", or "none"

**Returns**: List with results for each successfully fitted model

### generate_comparison_plots()

**Purpose**: Statistical comparison plots with testing

**Parameters**:
- `data`: Data frame with your data
- `category`: Unquoted name of grouping variable
- `Vars`: Character vector of variables to compare
- `repeat_category`: Optional stratification variable
- `plots`: Logical, whether to generate plots
- `table`: Logical, whether to generate tables

**Returns**: List with plots and summary statistics

### mosaic_analysis()

**Purpose**: Categorical variable analysis with chi-square testing

**Parameters**:
- `data`: Data frame with your data
- `var1`, `var2`: Unquoted names of categorical variables
- `min_count`: Minimum observations per category
- `title`: Plot title
- `show_percentages`: Whether to include percentages

**Returns**: List with mosaic plot, chi-square results, and summary tables

### view_results()

**Purpose**: Easy visualization of clustering results

**Parameters**:
- `results`: Results from run_full_moe_analysis()
- `what`: "plots" or "tables"
- `scale`: "original" or "scaled"

## Common Use Cases

### Customer Segmentation

```r
# Load customer data
customer_data <- read.csv("customers.csv")

# Run clustering
segments <- run_full_moe_analysis(
  input_data = customer_data,
  cluster_vars = c("age", "income", "spending", "satisfaction"),
  n_clusters = 4
)

# View segment profiles
view_results(segments, what = "plots", scale = "original")
```

### Treatment Effect Analysis

```r
# Load experimental data
experiment_data <- read.csv("experiment.csv")

# Compare treatment groups
treatment_effects <- generate_comparison_plots(
  data = experiment_data,
  category = treatment_group,
  Vars = c("pre_score", "post_score", "improvement"),
  plots = TRUE,
  table = TRUE
)
```

### Survey Analysis

```r
# Load survey data
survey_data <- read.csv("survey.csv")

# Analyze demographic relationships
demographics <- mosaic_analysis(
  data = survey_data,
  var1 = age_group,
  var2 = education_level,
  min_count = 20,
  title = "Age vs Education Distribution"
)
```

## Tips and Best Practices

### Data Preparation
- Clean your data before analysis
- Handle missing values appropriately
- Ensure variables are the correct type (numeric for clustering, categorical for mosaic)

### Parameter Selection
- Start with 2-5 clusters for clustering analysis
- Use "standardize" scaling for most cases
- Set appropriate minimum counts for categorical analysis

### Interpretation
- Examine both statistical and practical significance
- Consider effect sizes, not just p-values
- Validate results with domain knowledge

## Troubleshooting

### Common Issues

1. **Convergence errors in clustering**
   - Try different scaling methods
   - Reduce the number of clusters
   - Check for multicollinearity

2. **Small sample sizes**
   - Increase minimum count thresholds
   - Consider combining categories
   - Use larger datasets when possible

3. **Missing data**
   - Handle missing values before analysis
   - Consider imputation methods
   - Document missing data patterns

### Getting Help

- Check function documentation: `?function_name`
- Review the vignette: `vignette("Saqrmisc-introduction")`
- Report issues on GitHub
- Check the package README for examples

## Advanced Usage

### Customizing Plots

```r
# Customize mosaic plot appearance
results <- mosaic_analysis(
  data = data,
  var1 = var1,
  var2 = var2,
  fontsize = 10,
  title = "Custom Title",
  var1_label = "Custom Label 1",
  var2_label = "Custom Label 2"
)
```

### Stratified Analysis

```r
# Run comparison analysis by subgroups
results <- generate_comparison_plots(
  data = data,
  category = main_group,
  Vars = c("var1", "var2"),
  repeat_category = subgroup,
  min_threshold = 0.05
)
```

### Accessing Individual Results

```r
# Get specific model results
if ("VEV" %in% names(clustering_results)) {
  vev_model <- clustering_results$VEV
  print(vev_model$model$BIC)
  print(vev_model$outputs_original$means_table)
}
```

## Performance Considerations

- Clustering analysis can be computationally intensive
- Large datasets may require more time
- Consider using parallel processing for multiple analyses
- Monitor memory usage with very large datasets

## Integration with Other Packages

The Saqrmisc package works well with other R packages:

- **ggplot2**: Customize plots further
- **dplyr**: Data manipulation
- **knitr/rmarkdown**: Create reports
- **shiny**: Create interactive applications

## Citation

If you use this package in your research, please cite:

```r
citation("Saqrmisc")
```

## Support

For questions and support:
- GitHub Issues: https://github.com/mohsaqr/Saqrmisc/issues
- Package Documentation: https://mohsaqr.github.io/Saqrmisc/
- Email: saqr@saqr.me 