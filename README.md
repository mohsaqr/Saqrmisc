# Saqrmisc: Comprehensive Data Analysis and Visualization R Package

[![R-CMD-check](https://github.com/mohsaqr/Saqrmisc/workflows/R-CMD-check/badge.svg)](https://github.com/mohsaqr/Saqrmisc/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-4.0.0+-blue.svg)](https://www.r-project.org/)

## 📦 Overview

**Saqrmisc** is a comprehensive R package that provides a collection of powerful functions for data processing, statistical analysis, and visualization. The package is designed to simplify complex statistical tasks while providing robust error handling and comprehensive documentation.

### 🎯 Key Features

- **Model-Based Clustering Analysis**: Complete MoEClust analysis with multiple covariance models
- **Statistical Comparison Plots**: Automated generation of comparison plots with statistical testing
- **Mosaic Plot Analysis**: Comprehensive categorical variable analysis with chi-square testing
- **Robust Error Handling**: Built-in error handling to prevent analysis failures
- **Comprehensive Documentation**: Detailed documentation with examples and use cases

## 🚀 Installation

### From GitHub (Recommended)

```r
# Install devtools if you haven't already
if (!require(devtools)) install.packages("devtools")

# Install Saqrmisc from GitHub
devtools::install_github("mohsaqr/Saqrmisc")
```

### From Source

```r
# Clone the repository
git clone https://github.com/mohsaqr/Saqrmisc.git
cd Saqrmisc

# Install the package
devtools::install()
```

## 📚 Package Functions

### 1. `run_full_moe_analysis()` - Model-Based Clustering

Performs comprehensive model-based clustering analysis using the MoEClust package. Iterates through all available covariance models and provides detailed outputs for both original and scaled data.

#### Usage

```r
library(Saqrmisc)

# Example with sample data
set.seed(123)
sample_data <- data.frame(
  var1 = rnorm(100, mean = c(2, 5, 8)[sample(1:3, 100, replace = TRUE)]),
  var2 = rnorm(100, mean = c(1, 4, 7)[sample(1:3, 100, replace = TRUE)]),
  var3 = rnorm(100, mean = c(3, 6, 9)[sample(1:3, 100, replace = TRUE)])
)

# Run comprehensive clustering analysis
results <- run_full_moe_analysis(
  input_data = sample_data,
  cluster_vars = c("var1", "var2", "var3"),
  n_clusters = 3,
  scaling_method = "standardize"
)

# View results
view_results(results, what = "plots", scale = "original")
```

#### Parameters

- `input_data`: Data frame containing the dataset
- `cluster_vars`: Character vector of column names for clustering
- `n_clusters`: Number of clusters to fit
- `scaling_method`: Preprocessing method ("standardize", "center", "minmax", "none")

#### Returns

A nested list containing:
- Model objects for each successfully fitted covariance model
- Cluster means tables (original and scaled scales)
- Profile plots (original and scaled scales)
- Cluster size tables with percentages

### 2. `generate_comparison_plots()` - Statistical Comparison Analysis

Creates comprehensive comparison plots using `ggbetweenstats` to compare a primary category across multiple variables. Supports optional stratification by a repeat category.

#### Usage

```r
# Create sample data
sample_data <- data.frame(
  group = sample(c("Control", "Treatment"), 100, replace = TRUE),
  anxiety = rnorm(100, mean = c(3, 2)[factor(sample(c("Control", "Treatment"), 100, replace = TRUE))]),
  depression = rnorm(100, mean = c(2, 1.5)[factor(sample(c("Control", "Treatment"), 100, replace = TRUE))]),
  stress = rnorm(100, mean = c(4, 3)[factor(sample(c("Control", "Treatment"), 100, replace = TRUE))]),
  source = sample(c("Study_A", "Study_B", "Study_C"), 100, replace = TRUE)
)

# Basic comparison analysis
results <- generate_comparison_plots(
  data = sample_data,
  category = group,
  Vars = c("anxiety", "depression", "stress"),
  plots = TRUE,
  table = TRUE
)

# Stratified analysis by source
results_by_source <- generate_comparison_plots(
  data = sample_data,
  category = group,
  Vars = c("anxiety", "depression"),
  repeat_category = source,
  plots = TRUE,
  table = TRUE,
  min_threshold = 0.05
)
```

#### Parameters

- `data`: Data frame containing the variables
- `category`: Unquoted name of the primary categorical variable
- `Vars`: Character vector of variable names to compare
- `repeat_category`: Optional stratification variable
- `plots`: Logical, whether to generate plots
- `table`: Logical, whether to generate summary tables
- `min_threshold`: Minimum percentage threshold for repeat categories

### 3. `mosaic_analysis()` - Categorical Variable Analysis

Performs comprehensive mosaic plot analysis for two categorical variables, including chi-square testing, Cramér's V calculation, and detailed summary tables.

#### Usage

```r
# Create sample categorical data
sample_data <- data.frame(
  gender = sample(c("Male", "Female"), 200, replace = TRUE),
  education = sample(c("High School", "Bachelor", "Master", "PhD"), 200, replace = TRUE),
  region = sample(c("North", "South", "East", "West"), 200, replace = TRUE)
)

# Perform mosaic analysis
results <- mosaic_analysis(
  data = sample_data,
  var1 = gender,
  var2 = education,
  min_count = 10,
  fontsize = 8,
  title = "Gender vs Education Level",
  var1_label = "Gender",
  var2_label = "Education Level",
  show_percentages = TRUE,
  percentage_base = "total"
)

# Access results
print(results$chi_square_test)
print(results$cramers_v)
print(results$summary_table)
```

#### Parameters

- `data`: Data frame containing the variables
- `var1`, `var2`: Unquoted names of categorical variables
- `min_count`: Minimum observations required per category
- `fontsize`: Font size for plot labels
- `title`: Plot title
- `var1_label`, `var2_label`: Custom labels for variables
- `show_percentages`: Whether to include percentages in summary
- `percentage_base`: Base for percentage calculation ("total", "row", "column")

## 🔧 Dependencies

The package requires the following R packages:

### Core Dependencies
- **MoEClust**: Model-based clustering
- **mclust**: Model-based clustering utilities
- **dplyr**: Data manipulation
- **ggplot2**: Plotting
- **ggstatsplot**: Statistical plots
- **vcd**: Visualizing categorical data
- **grid**: Grid graphics
- **tibble**: Modern data frames
- **rlang**: Advanced R programming
- **gridExtra**: Grid graphics utilities
- **gt**: Tables
- **janitor**: Data cleaning
- **prcr**: Profile analysis
- **ggcharts**: Chart utilities
- **MASS**: Statistical functions
- **tidyverse**: Data science tools

### Suggested Dependencies
- **testthat**: Testing framework
- **knitr**: Dynamic report generation
- **rmarkdown**: R Markdown
- **devtools**: Development tools
- **roxygen2**: Documentation

## 📖 Documentation

### Vignettes

The package includes comprehensive vignettes demonstrating usage:

```r
# View available vignettes
vignette(package = "Saqrmisc")

# Open specific vignettes
vignette("clustering_analysis", package = "Saqrmisc")
vignette("comparison_analysis", package = "Saqrmisc")
vignette("mosaic_analysis", package = "Saqrmisc")
```

### Function Documentation

Access detailed function documentation:

```r
# View function help
?run_full_moe_analysis
?generate_comparison_plots
?mosaic_analysis
?view_results
```

## 🧪 Examples

### Complete Workflow Example

```r
library(Saqrmisc)

# 1. Generate sample data
set.seed(123)
data <- data.frame(
  id = 1:200,
  group = sample(c("Control", "Treatment"), 200, replace = TRUE),
  age = rnorm(200, mean = 45, sd = 10),
  score1 = rnorm(200, mean = c(50, 60)[factor(sample(c("Control", "Treatment"), 200, replace = TRUE))]),
  score2 = rnorm(200, mean = c(30, 40)[factor(sample(c("Control", "Treatment"), 200, replace = TRUE))]),
  score3 = rnorm(200, mean = c(70, 80)[factor(sample(c("Control", "Treatment"), 200, replace = TRUE))]),
  category = sample(c("A", "B", "C"), 200, replace = TRUE),
  region = sample(c("North", "South"), 200, replace = TRUE)
)

# 2. Clustering analysis
clustering_results <- run_full_moe_analysis(
  input_data = data,
  cluster_vars = c("age", "score1", "score2", "score3"),
  n_clusters = 3
)

# 3. Comparison analysis
comparison_results <- generate_comparison_plots(
  data = data,
  category = group,
  Vars = c("score1", "score2", "score3"),
  plots = TRUE,
  table = TRUE
)

# 4. Mosaic analysis
mosaic_results <- mosaic_analysis(
  data = data,
  var1 = group,
  var2 = category,
  min_count = 10,
  title = "Group vs Category Distribution"
)

# 5. View results
view_results(clustering_results, what = "plots", scale = "original")
```

## 🤝 Contributing

We welcome contributions! Please feel free to submit issues, feature requests, or pull requests.

### Development Setup

```r
# Clone the repository
git clone https://github.com/mohsaqr/Saqrmisc.git
cd Saqrmisc

# Install development dependencies
devtools::install_dev_deps()

# Load the package for development
devtools::load_all()

# Run tests
devtools::test()

# Check the package
devtools::check()
```

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 👨‍💻 Author

**Mohammed Saqr**
- Email: saqr@saqr.me
- GitHub: [@mohsaqr](https://github.com/mohsaqr)

## 🙏 Acknowledgments

- The R community for excellent packages and documentation
- Contributors and users who provide feedback and suggestions
- Academic researchers whose work inspired the statistical methods implemented

## 📊 Citation

If you use this package in your research, please cite:

```r
citation("Saqrmisc")
```

## 🔗 Links

- **GitHub Repository**: https://github.com/mohsaqr/Saqrmisc
- **Issues**: https://github.com/mohsaqr/Saqrmisc/issues
- **Documentation**: https://mohsaqr.github.io/Saqrmisc/

---

**Made with ❤️ for the R community**
