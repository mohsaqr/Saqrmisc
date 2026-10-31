# Tables from a keyword review report

Tables from a keyword review report

## Usage

``` r
# S3 method for class 'saqr_review'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  what = c("decisions", "replace"),
  ...
)
```

## Arguments

- x:

  A \[review_report()\] result.

- row.names, optional:

  Ignored; present for the generic.

- what:

  \`"decisions"\` (default) or \`"replace"\`; see \[review_report()\].

- ...:

  Ignored.

## Value

A data frame.
