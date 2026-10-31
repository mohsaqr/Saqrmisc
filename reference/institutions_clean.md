# Clean institution names by OpenAlex institution ID

Gives each institution one name and keeps different institutions apart,
using OpenAlex institution IDs (with their ROR IDs where OpenAlex has
them). For every paper with a DOI, OpenAlex says which institutions its
authors belong to. Papers OpenAlex does not cover, or authors it gives
only an address for, are resolved from the address text:

1.  an address OpenAlex has already linked to an institution on another
    paper gets that institution (\`source = "openalex address"\`);

2.  otherwise the institution part of the address is taken (the part
    naming a university, college, academy or hospital, else an
    institute, centre, lab or company, else the first part), and if
    OpenAlex knows that name, the institution behind it (\`"openalex
    name"\`);

3.  otherwise that part is kept as written (\`"address"\`).

Two institutions OpenAlex keeps apart that share a name get their
country code: \`NORTHEASTERN UNIVERSITY\`, \`NORTHEASTERN UNIVERSITY,
CN\`. The names from step 3 are the ones left to \[suggest_merges()\],
which never pairs two institutions OpenAlex has identified.

## Usage

``` r
institutions_clean(
  data,
  affiliations = "C1",
  doi = "DI",
  openalex = TRUE,
  replace = NULL,
  mailto = NULL
)
```

## Arguments

- data:

  Your papers, one row each (a bibliometrix data frame). The result of
  another \`clean\_\*()\` function works too, so the cleaners chain: \`M
  \|\> authors_clean(scopus = "Dlds") \|\> keywords_clean()\`.

- affiliations:

  The column of author addresses, \`;\`-separated. Default \`"C1"\`.

- doi:

  The DOI column. Default \`"DI"\`.

- openalex:

  Ask OpenAlex (\`TRUE\`, the default)? With \`FALSE\` every institution
  comes from the address text.

- replace:

  Reviewed merges to apply: an \[ask_ai()\] result, a review sheet from
  \[review_sheet()\], or \`OLD\`/\`NEW\` tables, as in
  \[keywords_clean()\].

- mailto:

  An e-mail address for OpenAlex's polite pool (faster, more reliable).
  Sent only to OpenAlex, and only when you give it.

## Value

A \`saqr_institutions\` object. Print it for an overview.
\`as.data.frame(x)\` gives your data with \`AU_UN\` (each paper's
institutions, \`;\`-separated, in author order) and \`AU_UN_ID\` (their
OpenAlex IDs, \`NA\` where none). \`what = "institutions"\`: one row per
institution: \`institution\`, \`openalex_id\`, \`ror\`, \`country\`,
\`type\`, \`papers\`, \`source\`. \[institutions_changes()\]: one row
per paper and institution: \`paper\`, \`address\` (the text it came
from; empty when OpenAlex named it directly), \`institution\`,
\`openalex_id\`, \`source\`. \`summary()\` counts papers and
institutions per \`source\`; names renamed by \`replace\` have \`source
= "review"\`. A review never renames an institution OpenAlex identified:
a name from address text renamed to an identified institution's name
joins it, ID and all.

## Errors

Raises \`saqrmisc_bad_input\` for a missing or non-character column, or
a bad \`openalex\` or \`mailto\`; \`saqrmisc_missing_package\` when
\`openalex = TRUE\` without httr2. Failed OpenAlex requests warn
\`saqrmisc_openalex_failed\`; those papers fall back to their addresses.

## References

Priem, J., Piwowar, H., & Orr, R. (2022). OpenAlex: A fully-open index
of scholarly works, authors, venues, institutions, and concepts.
\*arXiv\*.
[doi:10.48550/arXiv.2205.01833](https://doi.org/10.48550/arXiv.2205.01833)

## See also

\[suggest_merges()\], \[authors_clean()\], \[keywords_clean()\]

## Examples

``` r
papers <- data.frame(
  DI = NA_character_,
  C1 = c("DEPARTMENT OF COMPUTING, UNIVERSITY OF TURKU, TURKU, FINLAND",
         "SCHOOL OF EDUCATION, UNIVERSITY OF EASTERN FINLAND, JOENSUU, FINLAND")
)
institutions <- institutions_clean(papers, openalex = FALSE)
institutions
#> Institutions by OpenAlex ID (OpenAlex not asked)
#>   2 institutions in 2 of 2 papers: 0 identified by OpenAlex, 2 from address text only
#>   links: openalex 0, openalex address 0, openalex name 0, review 0, address 2
#> as.data.frame(x) gives the cleaned papers; institutions_changes(x) what each address became.
#> suggest_merges(x) proposes merges among the names OpenAlex did not identify.
```
