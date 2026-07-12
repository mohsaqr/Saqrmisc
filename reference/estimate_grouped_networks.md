# Estimate Grouped Networks

Estimates networks for different groups within a data frame, using
\`estimate_single_network\` internally. Produces separate network
visualizations and metrics for each group.

## Usage

``` r
estimate_grouped_networks(
  df,
  Vars = NULL,
  GroupVar,
  layout = "circle",
  color = DEFAULT_NODE_COLOR,
  default = "EBICglasso",
  var_types = NULL,
  qgraph_args = list(),
  network_args = list(),
  verbose = TRUE,
  min_group_size = 30
)
```

## Arguments

- df:

  A data frame containing the variables and group variable.

- Vars:

  Character vector of variable names to include in the network. If NULL
  (default), all columns except GroupVar are used.

- GroupVar:

  Character string specifying the grouping variable name.

- layout:

  Layout algorithm for network plots. Defaults to "circle".

- color:

  Node color(s). Defaults to "#EEEEEE".

- default:

  Network estimation method. Defaults to "EBICglasso".

- var_types:

  Variable types for MGM. If NULL, auto-detected.

- qgraph_args:

  Additional arguments for qgraph.

- network_args:

  Additional arguments for bootnet::estimateNetwork.

- verbose:

  Print progress messages. Defaults to TRUE.

- min_group_size:

  Minimum group size required. Groups with fewer observations are
  skipped. Defaults to 30.

## Value

A list of class "grouped_network_analysis" containing:

- `networks`: List of network objects per group

- `network_matrices`: List of adjacency matrices per group

- `predictions`: List of prediction results per group

- `centralities`: List of centrality measures per group

- `qgraphs`: List of qgraph plot objects per group

- `group_sizes`: Sample sizes per group

- `failed_groups`: Groups that failed estimation

## Examples

``` r
if (FALSE) { # \dontrun{
# Estimate networks by group
results <- estimate_grouped_networks(
  df = my_data,
  Vars = c("var1", "var2", "var3", "var4"),
  GroupVar = "group"
)

# Access results for specific group
results$networks$GroupA
results$predictions$GroupA

# Compare networks
compare_networks(results, groups = c("GroupA", "GroupB"))
} # }
```
