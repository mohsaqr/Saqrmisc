# Saqrmisc Package: Global Variable Declarations
#
# This file declares global variables to avoid R CMD CHECK notes about
# "no visible binding for global variable" when using non-standard evaluation
# (NSE) in tidyverse packages like dplyr and ggplot2.

# Declare global variables used in NSE contexts
utils::globalVariables(c(
  # dplyr/tidyr variables
  ".",
  "n",
  "Variable",
  "Value",
  "Group",
  "Type",
  "Total",
  "Statistic",
  "Mean",
  "SD",
  "Min",
  "Max",
  "Median",
  "N",
  "Pct",
  "Count",
  "Freq",
  "Percent",
  "Category",
  "Level",


  # Comparison/statistical variables
  "p_value",
  "p_adj",
  "effect_size",
  "statistic",
  "df",
  "comparison",
  "variable",
  "method",
  "sig",
  "Sig",
  "r",
  "CI_low",
  "CI_high",

  # Correlation variables
  "Var1",
  "Var2",
  "correlation",
  "label",
  "Variable1",
  "Variable2",

  # Clustering variables
  "Cluster",
  "cluster",
  "Proportion",
  "Model",
  "BIC",
  "ICL",
  "AIC",
  "loglik",
  "G",
  "expert_type",
  "gating_type",

  # Network variables
  "from",
  "to",
  "weight",
  "edge",
  "node",
  "strength",
  "betweenness",
  "closeness",
  "expected_influence",

  # Plot aesthetics
  "x",
  "y",
  "fill",
  "color",
  "colour",
  "label",
  "group",
  "size",
  "alpha",
  "shape",
  "linetype",

  # Table formatting
  "everything",
  "where",
  "any_of",
  "all_of",
  "starts_with",
  "ends_with",
  "contains",
  "matches",

  # gt table variables
  "cells_body",
  "cells_column_labels",
  "cells_title",

  # Miscellaneous
  "rowname",
  "name",
  "value",
  "data",
  "result",
  "output",

  # Additional variables found in check
  "efsz",
  "mean_formatted",
  "sd_formatted",
  "ES_formatted",
  "p_formatted",
  "Difference",
  "follows",
  "Row",
  "Missing",
  "Index",
  "Distance",
  "Outlier",
  "se",
  "error",
  "ci"
))
