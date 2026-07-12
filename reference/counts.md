# Count Events per ID

Converts long-format event data to a frequency table with counts per ID.
Useful for transforming event logs, clickstream data, or any repeated
measures into a wide format suitable for analysis.

## Usage

``` r
counts(data, id = NULL, event = NULL, na_values = NULL)
```

## Arguments

- data:

  A data frame containing event data in long format.

- id:

  Character vector. Name(s) of ID column(s). If multiple IDs are
  provided, they are combined. If NULL (default), uses the first column.

- event:

  Character. Name of the event/action column to count. If NULL
  (default), uses the second column.

- na_values:

  Character vector of values to treat as missing. Default includes
  common missing indicators.

## Value

A data frame with one row per unique ID combination and columns for each
unique event value containing counts.

## Examples

``` r
if (FALSE) { # \dontrun{
# Event log data
events <- data.frame(
  student = c(1, 1, 1, 2, 2, 2, 3, 3),
  action = c("login", "view", "submit", "login", "view", "view", "login", "submit")
)

# Count events per student
counts(events, id = "student", event = "action")

# Multiple ID columns
events2 <- data.frame(
  student = c(1, 1, 1, 1, 2, 2),
  course = c("A", "A", "B", "B", "A", "A"),
  action = c("view", "submit", "view", "view", "view", "submit")
)

counts(events2, id = c("student", "course"), event = "action")
} # }
```
