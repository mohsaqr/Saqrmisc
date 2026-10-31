# Tables of suggested keyword merges

Tables of suggested keyword merges

## Usage

``` r
# S3 method for class 'saqr_proposals'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  what = c("pairs", "evidence"),
  ...
)
```

## Arguments

- x:

  A \[suggest_merges()\] or \[ask_ai()\] result.

- row.names, optional:

  Ignored; present for the generic.

- what:

  \`"pairs"\` (default) or \`"evidence"\`; see \[suggest_merges()\].

- ...:

  Ignored.

## Value

A data frame with one row per pair.
