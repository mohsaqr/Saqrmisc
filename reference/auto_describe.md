# Automatically Generate Descriptive Statistics for All Variables

Automatically detects numeric and categorical variables in a data frame
and generates appropriate descriptive statistics tables for each type.
Numeric variables get summary statistics (mean, SD, etc.) and
categorical variables get frequency tables.

## Usage

``` r
auto_describe(
  data,
  group_by = NULL,
  numeric_stats = c("n", "mean", "sd", "median", "min", "max"),
  digits = 2,
  exclude = NULL,
  force_categorical = NULL,
  title_numeric = "Numeric Variables",
  title_categorical = "Categorical Variables",
  theme = "default",
  format = "gt",
  print = TRUE
)
```

## Arguments

- data:

  A data frame to describe.

- group_by:

  Optional character. Name of a grouping variable for stratified
  statistics.

- numeric_stats:

  Character vector of statistics for numeric variables. Default:
  \`c("n", "mean", "sd", "median", "min", "max")\`.

- digits:

  Integer. Decimal places for numeric output. Default: \`2\`.

- exclude:

  Character vector of column names to exclude. Default: \`NULL\`.

- force_categorical:

  Character vector of numeric column names to treat as categorical
  instead. Default: \`NULL\`.

- title_numeric:

  Title for numeric table. Default: \`"Numeric Variables"\`.

- title_categorical:

  Title for categorical tables. Default: \`"Categorical Variables"\`.

- theme:

  Visual theme: \`"default"\`, \`"minimal"\`, \`"dark"\`,
  \`"colorful"\`. Default: \`"default"\`.

- format:

  Output format: \`"gt"\` or \`"data.frame"\`. Default: \`"gt"\`.

- print:

  Logical. Print tables to console? Default: TRUE.

## Value

A list with:

- numeric: Descriptive table for numeric variables (or NULL if none)

- categorical: List of frequency tables for categorical variables

- variable_types: Data frame showing detected variable types

## Examples

``` r
if (FALSE) { # \dontrun{
# Automatic description of all variables
data <- data.frame(
  age = rnorm(100, 35, 10),
  score = rnorm(100, 75, 15),
  gender = sample(c("M", "F"), 100, replace = TRUE),
  education = sample(c("HS", "BA", "MA", "PhD"), 100, replace = TRUE)
)

# Describe all variables automatically
results <- auto_describe(data)

# With grouping
results <- auto_describe(data, group_by = "gender")

# Access individual tables
results$numeric
results$categorical$education
} # }
```
