# Clean author names with Scopus author IDs and ORCID

Rewrites the author field of a bibliometric data frame so that each
person has one spelling and different people never share one. Identity
comes from IDs only, never from names:

\* all spellings of one Scopus ID become one spelling (the most
frequent, then the most recent, then the longest), with the accents
kept; \* with \`openalex = TRUE\`, OpenAlex is asked by DOI for each
author's ORCID, and Scopus IDs whose papers carry the same ORCID are
joined into one person. An author without a usable Scopus ID takes its
OpenAlex author ID; \* different people who share a spelling stay apart,
labelled \`"KIM J"\`, \`"KIM J (2)"\`, ... by number of papers; \* an
author identified by neither source keeps its spelling, and a paper with
no identified author is left untouched.

A merge whose spellings do not look like one name ("ENDRASS B" and
"LUGRIN B" under one Scopus ID) is kept but flagged \`check = TRUE\`.
Lost accents ("KRMER N"), initials against given names ("MOORE RL",
"MOORE ROBERT L") and a surname part dropped ("LOPEZ SS", "SAFFON LOPEZ
S") count as one name. Write the merges to a file with
\[review_sheet()\], mark the ones that are wrong \`split\`, and pass the
file back as \`review\`.

Scopus writes a generational suffix as its own name ("Huff E.W., Jr."),
and names may hold HTML entities (\`L&#x00F3;pez\`); both are repaired.

## Usage

``` r
authors_clean(
  data,
  scopus = NULL,
  author_ids = "Author(s) ID",
  eid = "eid",
  authors = "AU",
  doi = "DI",
  year = "PY",
  openalex = TRUE,
  mailto = NULL,
  review = NULL
)
```

## Arguments

- data:

  A data frame with one row per paper, such as a bibliometrix data
  frame. The result of another \`clean\_\*()\` function works too, so
  the cleaners chain: \`M \|\> authors_clean(scopus = "Dlds") \|\>
  keywords_clean()\`.

- scopus:

  Paths to the raw Scopus CSV exports, or a folder holding them. They
  need the columns \`Authors\`, \`Author(s) ID\`, \`DOI\` and \`EID\`.
  Leave it out when \`data\` itself holds the Scopus author IDs (column
  \`author_ids\`): bibliometrix drops them, so a bibliometrix data frame
  needs \`scopus\`. Given both, \`scopus\` is used, since its names keep
  their accents.

- author_ids:

  Name of the column in \`data\` holding the Scopus author IDs
  (\`;\`-separated, in the order of the authors), read when \`scopus\`
  is left out. Default \`"Author(s) ID"\`, Scopus's own name.

- eid:

  Name of the column in \`data\` holding the Scopus EID
  (\`"2-s2.0-..."\`), which links each paper to its Scopus row.

- authors, doi, year:

  Names of the author (\`;\`-separated), DOI and publication-year
  columns in \`data\`. The defaults are bibliometrix's.

- openalex:

  Logical. Consult OpenAlex for the gaps described above? Needs the
  httr2 package and an internet connection. Default \`TRUE\`.

- mailto:

  Optional e-mail address sent to OpenAlex to use its polite pool.
  Default \`NULL\` sends none.

- review:

  Optional reviewed merges: the \`.csv\` or \`.xlsx\` file from
  \[review_sheet()\], or a data frame with its columns \`person_key\`
  and \`decision\`. A person marked \`split\` is taken apart again: into
  its Scopus IDs when ORCID joined several, otherwise into its Scopus
  spellings. An empty \`decision\` or \`keep\` keeps the merge. Default
  \`NULL\`.

## Value

A \`saqr_authors\` object:

- \`as.data.frame(x)\`: \`data\` with the author column rewritten and a
  new \`AU_ID\` column holding the matching person keys in the same
  order (\`-\` for an unidentified author; \`NA\` for an untouched
  paper).

- \[authors_changes()\]: one row per person whose spelling changed, who
  was joined across Scopus IDs, or who shares a spelling, most papers
  first: \`person_key\`, \`name\`, \`spellings\`, \`papers\`,
  \`scopus_ids\`, \`merged\` (one Scopus ID, several spellings),
  \`check\` (a merged or joined person whose spellings do not look like
  one name), \`homonym\`, \`orcid_conflict\` (the person's papers show
  more than one ORCID; flagged, not split) and \`split\` (a part of a
  person \`review\` took apart; a part split by spelling has the key
  \`\<key\>\|\<spelling\>\`). \[authors_check()\] gives its rows with
  \`check = TRUE\`.

- \`as.data.frame(x, what = "authors")\`: one row per author per paper
  (for tracing a single decision): \`eid\`, \`doi\`, \`year\`,
  \`position\`, \`name_before\`, \`name_scopus\`, \`scopus_id\`,
  \`openalex_id\`, \`orcid\`, \`person_key\` (\`scopus:\<id\>\`,
  \`orcid:\<id\>\`, \`openalex:\<id\>\`, or \`NA\`), \`split_from\` (the
  key \`review\` split, else \`NA\`), \`source\` and \`name\`.

\`summary()\` returns the counts as a data frame.

## Errors

Raises \`saqrmisc_bad_input\` when \`data\` lacks a named column, has
repeated EIDs, \`scopus\` names no readable Scopus export, or \`review\`
is not a review sheet or holds a decision other than \`keep\` or
\`split\`; \`saqrmisc_missing_package\` when \`openalex = TRUE\` without
httr2, or for an \`.xlsx\` \`review\` without readxl. A
\`saqrmisc_review_unmatched\` warning names reviewed people this run
does not have. A \`saqrmisc_openalex_failed\` warning reports OpenAlex
requests that failed; the authors involved keep their Scopus identity.

## References

Priem, J., Piwowar, H., & Orr, R. (2022). OpenAlex: A fully-open index
of scholarly works, authors, venues, institutions, and concepts.
arXiv:2205.01833.

Baas, J., Schotten, M., Plume, A., Côté, G., & Karimi, R. (2020). Scopus
as a curated, high-quality bibliometric data source for academic
research in quantitative science studies. \*Quantitative Science
Studies\*, 1(1), 377-386.
[doi:10.1162/qss_a_00019](https://doi.org/10.1162/qss_a_00019)

## Examples

``` r
scopus_file <- tempfile(fileext = ".csv")
utils::write.csv(data.frame(
  Authors = c("Pérez-Marín D., Kim J.", "Perez-Marin D., Kim J."),
  `Author(s) ID` = c("111;201;", "111;202;"),
  DOI = c("10.1/a", "10.1/b"),
  EID = c("2-s2.0-1", "2-s2.0-2"),
  check.names = FALSE
), scopus_file, row.names = FALSE, fileEncoding = "UTF-8")
papers <- data.frame(
  eid = c("2-s2.0-1", "2-s2.0-2"),
  AU = c("PREZ-MARN D;KIM J", "PEREZ-MARIN D;KIM J"),
  DI = c("10.1/a", "10.1/b"),
  PY = c(2019, 2020)
)
cleaned <- authors_clean(papers, scopus = scopus_file, openalex = FALSE)
cleaned
#> Author cleaning by Scopus ID
#>   2 of 2 papers rewritten; 3 people
#>   1 Scopus IDs had several spellings, now one; 0 people joined across Scopus IDs by ORCID
#>   0 of these merges to check: their spellings do not look like one name
#>   2 share a spelling and are numbered; 0 ORCID conflicts flagged
#>   0 author rows unidentified, kept as they were
#> 
#> as.data.frame(x) gives the cleaned papers; review_sheet(x, file) writes every merge for you to check.
as.data.frame(cleaned)
#>        eid                      AU     DI   PY                 AU_ID
#> 1 2-s2.0-1     PEREZ-MARIN D;KIM J 10.1/a 2019 scopus:111;scopus:201
#> 2 2-s2.0-2 PEREZ-MARIN D;KIM J (2) 10.1/b 2020 scopus:111;scopus:202
authors_changes(cleaned)
#>   person_key          name                   spellings papers scopus_ids merged
#> 1 scopus:111 PEREZ-MARIN D PEREZ-MARIN D | PREZ-MARN D      2          1   TRUE
#> 2 scopus:201         KIM J                       KIM J      1          1  FALSE
#> 3 scopus:202     KIM J (2)                       KIM J      1          1  FALSE
#>   check homonym orcid_conflict split
#> 1 FALSE   FALSE          FALSE FALSE
#> 2 FALSE    TRUE          FALSE FALSE
#> 3 FALSE    TRUE          FALSE FALSE

# Merges to check go to a file; mark a wrong one `split` and pass it back.
sheet <- tempfile(fileext = ".csv")
review_sheet(cleaned, sheet)
#> file21ed4b2d8722.csv: 1 merge(s), 0 to check. Write split in decision for a wrong one, then authors_clean(review = "file21ed4b2d8722.csv").
undone <- authors_clean(papers, scopus = scopus_file, openalex = FALSE,
                        review = data.frame(person_key = "scopus:111", decision = "split"))
as.data.frame(undone)
#>        eid                      AU     DI   PY
#> 1 2-s2.0-1     PÉREZ-MARÍN D;KIM J 10.1/a 2019
#> 2 2-s2.0-2 PEREZ-MARIN D;KIM J (2) 10.1/b 2020
#>                                 AU_ID
#> 1 scopus:111|PÉREZ-MARÍN D;scopus:201
#> 2 scopus:111|PEREZ-MARIN D;scopus:202
```
