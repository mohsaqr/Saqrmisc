# Tables from an authors_clean() result

Tables from an authors_clean() result

## Usage

``` r
# S3 method for class 'saqr_authors'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  what = c("data", "changes", "authors"),
  ...
)
```

## Arguments

- x:

  An \[authors_clean()\] result.

- row.names, optional:

  Ignored; present for the generic.

- what:

  Which table: \`"data"\` (the cleaned papers, default), \`"changes"\`
  (as \[authors_changes()\]) or \`"authors"\` (one row per author per
  paper). See \[authors_clean()\] for their columns.

- ...:

  Ignored.

## Value

A data frame.
