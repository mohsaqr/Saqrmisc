# Create a saqr_result object

Creates a standardized result object for Saqrmisc functions that
includes both the original data and a clean markdown representation for
AI interpretation.

## Usage

``` r
saqr_result(data = NULL, table = NULL, type = "analysis", markdown = NULL, ...)
```

## Arguments

- data:

  The main data (data frame or tibble with results)

- table:

  Optional gt table or formatted table

- type:

  Character describing the analysis type (e.g., "group_comparison",
  "correlation", "descriptive")

- markdown:

  Optional pre-built markdown string. If NULL, auto-generated from data.

- ...:

  Additional components to include in the result

## Value

A list with class "saqr_result" containing data, table, markdown, and
other components
