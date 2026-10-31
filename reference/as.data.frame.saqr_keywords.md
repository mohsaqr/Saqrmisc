# Tables from a keywords_clean() result

Tables from a keywords_clean() result

## Usage

``` r
# S3 method for class 'saqr_keywords'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  what = c("data", "changes", "terms"),
  ...
)
```

## Arguments

- x:

  A \[keywords_clean()\] result.

- row.names, optional:

  Ignored; present for the generic.

- what:

  Which table: \`"data"\` (the cleaned papers, default), \`"changes"\`
  (as \[keywords_changes()\]) or \`"terms"\` (one row per cleaned
  keyword). See \[keywords_clean()\] for their columns.

- ...:

  Ignored.

## Value

A data frame.
