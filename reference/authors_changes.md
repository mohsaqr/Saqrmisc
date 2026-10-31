# The changes and the merges to check

Two verbs on an \[authors_clean()\] result. \`authors_changes()\` gives
one row per person whose spelling changed, who was joined across Scopus
IDs, or who shares a spelling. \`authors_check()\` gives the rows of
those whose spellings do not look like one name, to be checked by hand.
The cleaned papers are \`as.data.frame()\` of the result.

## Usage

``` r
authors_changes(x)

authors_check(x)
```

## Arguments

- x:

  An \[authors_clean()\] result.

## Value

A data frame with one row per person, with the columns listed under
Value in \[authors_clean()\].

## Errors

Raises \`saqrmisc_bad_input\` when \`x\` is not an \[authors_clean()\]
result.

## Examples

``` r
scopus_file <- tempfile(fileext = ".csv")
utils::write.csv(data.frame(
  Authors = c("Lugrin B., Kim J.", "Leclere B., Kim J."),
  `Author(s) ID` = c("111;201;", "111;202;"),
  DOI = c("10.1/a", "10.1/b"),
  EID = c("2-s2.0-1", "2-s2.0-2"),
  check.names = FALSE
), scopus_file, row.names = FALSE, fileEncoding = "UTF-8")
papers <- data.frame(
  eid = c("2-s2.0-1", "2-s2.0-2"),
  AU = c("LUGRIN B;KIM J", "LECLERE B;KIM J"),
  DI = c("10.1/a", "10.1/b"),
  PY = c(2019, 2020)
)
authors <- authors_clean(papers, scopus = scopus_file, openalex = FALSE)
authors_changes(authors)
#>   person_key      name            spellings papers scopus_ids merged check
#> 1 scopus:111 LECLERE B LECLERE B | LUGRIN B      2          1   TRUE  TRUE
#> 2 scopus:201     KIM J                KIM J      1          1  FALSE FALSE
#> 3 scopus:202 KIM J (2)                KIM J      1          1  FALSE FALSE
#>   homonym orcid_conflict split
#> 1   FALSE          FALSE FALSE
#> 2    TRUE          FALSE FALSE
#> 3    TRUE          FALSE FALSE
authors_check(authors)
#>   person_key      name            spellings papers scopus_ids merged check
#> 1 scopus:111 LECLERE B LECLERE B | LUGRIN B      2          1   TRUE  TRUE
#>   homonym orcid_conflict split
#> 1   FALSE          FALSE FALSE
```
