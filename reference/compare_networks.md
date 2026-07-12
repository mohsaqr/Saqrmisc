# Compare Two Networks

Performs Network Comparison Test (NCT) to compare network structure and
global strength between two groups.

## Usage

``` r
compare_networks(results, groups, it = 1000, test = "both", verbose = TRUE)
```

## Arguments

- results:

  Object from estimate_grouped_networks

- groups:

  Character vector of length 2 specifying which groups to compare

- it:

  Number of permutations for NCT. Defaults to 1000.

- test:

  Which tests to perform: "network" (structure), "global" (strength),
  "both" (default).

- verbose:

  Print progress. Defaults to TRUE.

## Value

A list containing NCT results with p-values for network structure and
global strength differences.

## Examples

``` r
if (FALSE) { # \dontrun{
# First estimate grouped networks
grouped_results <- estimate_grouped_networks(df, Vars, GroupVar = "group")

# Then compare two groups
comparison <- compare_networks(
  grouped_results,
  groups = c("GroupA", "GroupB"),
  it = 500
)
} # }
```
