# Estimate a Single Network

Estimates a single psychological network from a data frame using
regularized partial correlations (via bootnet) and Mixed Graphical
Models (via mgm). Provides network visualization with predictability
metrics (R-squared pie charts).

## Usage

``` r
estimate_single_network(
  df,
  Vars = NULL,
  layout = "circle",
  color = DEFAULT_NODE_COLOR,
  title = "Between-person network",
  default = "EBICglasso",
  var_types = NULL,
  qgraph_args = list(),
  network_args = list(),
  verbose = TRUE,
  compute_centrality = TRUE
)
```

## Arguments

- df:

  A data frame containing the variables to be analyzed.

- Vars:

  A character vector specifying the names of variables to include. If
  NULL (default), all columns of the data frame are used.

- layout:

  Character string specifying the layout algorithm for the network plot.
  Options: "circle", "spring", "groups", etc. Defaults to "circle".

- color:

  Character string or vector specifying node colors. Defaults to
  "#EEEEEE".

- title:

  Title for the network plot. Defaults to "Between-person network".

- default:

  Network estimation method for bootnet::estimateNetwork. Options:
  "EBICglasso" (default), "ggmModSelect", "pcor", etc.

- var_types:

  Character vector of variable types for MGM ("g" = Gaussian, "c" =
  categorical, "p" = Poisson). If NULL (default), types are
  auto-detected.

- qgraph_args:

  List of additional arguments passed to qgraph::qgraph.

- network_args:

  List of additional arguments passed to bootnet::estimateNetwork.

- verbose:

  Logical. If TRUE, prints progress messages. Defaults to TRUE.

- compute_centrality:

  Logical. If TRUE, computes centrality measures. Defaults to TRUE.

## Value

A list of class "network_analysis" containing:

- `network_object`: Full network object from bootnet

- `network_matrix`: Adjacency/weight matrix

- `prediction`: MGM prediction results (R2, RMSE)

- `centrality`: Centrality measures (if compute_centrality = TRUE)

- `qgraph`: The qgraph plot object

- `var_types`: Variable types used for MGM

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage
result <- estimate_single_network(
  df = my_data,
  Vars = c("var1", "var2", "var3", "var4", "var5")
)

# Access network matrix
result$network_matrix

# View predictability
result$prediction

# View centrality
result$centrality

# Custom settings
result <- estimate_single_network(
  df = my_data,
  Vars = c("var1", "var2", "var3"),
  layout = "spring",
  default = "ggmModSelect",
  var_types = c("g", "g", "c"),  # Manual type specification
  verbose = FALSE
)
} # }
```
