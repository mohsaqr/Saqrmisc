# Tables from an institutions_clean() result

Tables from an institutions_clean() result

## Usage

``` r
# S3 method for class 'saqr_institutions'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  what = c("data", "institutions", "addresses"),
  ...
)
```

## Arguments

- x:

  An \[institutions_clean()\] result.

- row.names, optional:

  Ignored; present for the generic.

- what:

  \`"data"\` (the cleaned papers, default), \`"institutions"\` or
  \`"addresses"\` (as \[institutions_changes()\]); see
  \[institutions_clean()\].

- ...:

  Ignored.

## Value

A data frame.
