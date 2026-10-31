# What each address became

One row per paper and institution of an \[institutions_clean()\] result:
the address it came from and the institution it became.

## Usage

``` r
institutions_changes(x, source = NULL)
```

## Arguments

- x:

  An \[institutions_clean()\] result.

- source:

  Only the addresses resolved this way: \`"openalex"\`, \`"openalex
  address"\`, \`"openalex name"\`, \`"review"\` or \`"address"\`
  (several allowed). Default \`NULL\` keeps all.

## Value

A data frame with one row per paper and institution: \`paper\`,
\`address\` (the text it came from; empty when OpenAlex named it
directly), \`institution\`, \`openalex_id\`, \`source\`.

## Errors

Raises \`saqrmisc_bad_input\` when \`x\` is not an
\[institutions_clean()\] result or \`source\` is none of the above.

## Examples

``` r
papers <- data.frame(
  C1 = c("UNIVERSITY OF OULU, OULU, FINLAND", "UNIV OULU, FINLAND"),
  DI = c("10.1/a", "10.1/b")
)
institutions <- institutions_clean(papers, openalex = FALSE)
institutions_changes(institutions)
#>   paper                           address        institution openalex_id
#> 1     1 UNIVERSITY OF OULU, OULU, FINLAND UNIVERSITY OF OULU        <NA>
#> 2     2                UNIV OULU, FINLAND          UNIV OULU        <NA>
#>    source
#> 1 address
#> 2 address
```
