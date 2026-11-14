# Clean institution names

Gives each institution one name in \`AU_UN\` and returns the data frame,
ready for bibliometrix. Institutions are identified by their OpenAlex
institution ID: for every paper with a DOI, OpenAlex says which
institutions its authors belong to.

The confirmed steps, applied with no review: 1. \*\*OpenAlex ID\*\*: the
institutions OpenAlex links to a paper's authors, under OpenAlex's name.
Two institutions OpenAlex keeps apart that share a name get their
country code (\`NORTHEASTERN UNIVERSITY, US\`, \`NORTHEASTERN
UNIVERSITY, CN\`). 2. \*\*Address OpenAlex linked\*\*: an address
OpenAlex linked to one institution on any paper gets that institution on
every paper (\`source\` \`openalex address\`). 3. \*\*OpenAlex name\*\*:
otherwise the institution part of the address is taken (the part naming
a university, college, academy or hospital, else an institute, centre,
lab or company, else the first part); if it is the name of one
institution OpenAlex knows, accents lost or not, it is that institution
(\`openalex name\`). 4. \*\*Address\*\*: otherwise that part is kept as
written, cleaned as \[keywords_clean()\] cleans keywords, your
\`replace\` tables applied, and names written differently (case,
punctuation, accents, spaces, plural) made one. A name that becomes an
identified institution's name joins it.

Papers OpenAlex does not cover, and authors it gives only an address
for, go through steps 2 to 4. Two institutions OpenAlex identified are
never merged.

OpenAlex's answers are kept in \`cache\`, so a second run, or a run that
stopped, asks only for the papers not asked yet. Requests run ten at a
time, at most ten a second (OpenAlex's limit), 100 DOIs each.

\`level = "all"\` merges nothing: it returns \`data\` unchanged and
writes every merge to the \`decisions\` sheet, for a person or a model
to check outside R, with a short prompt beside it, as
\[keywords_clean()\] does. The sheet has one group per \`name\` (the
institution the group becomes): its \`confirmed\` merges (steps 2 to 4)
and its \`needs_revision\` candidates, found as \[keywords_clean()\]
finds them (\`source\`: \`spelling\`, \`word order\`, \`ngram\`,
\`stem\`, \`initials\`). An identified institution is always the
\`name\` of its group. Only institutions in at least \`min_papers\`
papers go to the sheet, each of them, grouped or alone (\`single\`).
Bring it back with \[institutions_merge()\].

## Usage

``` r
institutions_clean(
  data,
  affiliations = "C1",
  doi = "DI",
  level = c("confirmed", "all"),
  decisions = NULL,
  min_papers = 5,
  replace = NULL,
  openalex = TRUE,
  cache = .in_default_cache(),
  mailto = NULL,
  output = c("minimal", "all")
)
```

## Arguments

- data:

  A data frame with one row per paper, such as a bibliometrix data
  frame.

- affiliations:

  The column of author addresses, \`;\`-separated. Default \`"C1"\`.

- doi:

  The DOI column. Default \`"DI"\`.

- level:

  \`"confirmed"\` (default) or \`"all"\`; see above.

- decisions:

  With \`level = "all"\`: the sheet to write, a new path ending in
  \`.xlsx\` or \`.csv\` (an existing file is never replaced). Its
  columns: \`group\`, \`name\`, \`institution\`, \`institution_backup\`
  (a copy of \`institution\` that \[institutions_merge()\] matches on),
  \`status\` (\`single\`, \`confirmed\`, \`needs_revision\`),
  \`papers\`, \`openalex_id\`, \`source\` and \`example\` (an address
  naming the institution). The model or person checking it adds a column
  \`merge\`. The prompt goes to \`\<sheet\>\_prompt.txt\` and, in an
  \`.xlsx\`, to a second tab \`prompt\`.

- min_papers:

  Only institutions in at least this many papers go into the sheet.
  Default 5.

- replace:

  Your replacement table for names from address text: a data frame with
  columns \`OLD\` and \`NEW\`, or the path to a \`.csv\`, \`.xlsx\` or
  \`.xls\` file holding one, as in \[keywords_clean()\]. It never
  renames an identified institution; a name renamed to an identified
  institution's name joins it.

- openalex:

  Ask OpenAlex (\`TRUE\`, the default)? With \`FALSE\` every institution
  comes from the address text.

- cache:

  A folder for OpenAlex's answers, or \`NULL\` to keep none. Default: a
  folder in \[tools::R_user_dir()\]. Delete it to ask again.

- mailto:

  An e-mail address for OpenAlex's polite pool (faster, more reliable).
  Sent only to OpenAlex, and only when you give it.

- output:

  \`"minimal"\` (default) returns \`data\` with \`AU_UN\` written and
  nothing else added. \`"all"\` also adds \`AU_UN_ID\` (the OpenAlex IDs
  of each paper's institutions, \`;\`-separated, \`NA\` where none) and,
  when \`data\` has an \`AU_UN\` already, \`AU_UN_ORIGINAL\`.

## Value

With \`level = "confirmed"\`: \`data\`, same class and rows, with
\`AU_UN\` (each paper's institutions, \`"; "\`-separated, in author
order, \`NA\` where none) added or replaced; with \`output = "all"\`,
plus the columns above. With \`level = "all"\`: \`data\` unchanged,
invisibly (the sheet is the result). A message counts the institutions
found each way.

## Errors

Raises \`saqrmisc_bad_input\` for a missing or non-character column, a
bad \`openalex\`, \`mailto\`, \`cache\` or \`min_papers\`, \`decisions\`
given with \`level = "confirmed"\` or missing or existing with
\`"all"\`, and the \`replace\` errors of \[keywords_clean()\];
\`saqrmisc_missing_package\` when \`openalex = TRUE\` without httr2, or
for \`.xlsx\` without readxl / writexl. Failed OpenAlex requests warn
\`saqrmisc_openalex_failed\`; those papers fall back to their addresses
and are asked again on the next run.

## References

Priem, J., Piwowar, H., & Orr, R. (2022). OpenAlex: A fully-open index
of scholarly works, authors, venues, institutions, and concepts.
\*arXiv\*.
[doi:10.48550/arXiv.2205.01833](https://doi.org/10.48550/arXiv.2205.01833)

## See also

\[institutions_merge()\], \[authors_clean()\], \[keywords_clean()\]

## Examples

``` r
papers <- data.frame(
  TI = c("Paper one", "Paper two", "Paper three"),
  DI = NA_character_,
  C1 = c("DEPARTMENT OF COMPUTING, UNIVERSITY OF TURKU, TURKU, FINLAND",
         "SCHOOL OF EDUCATION, UNIVERSITY OF EASTERN FINLAND, JOENSUU, FINLAND",
         "UNIVERSITY-OF-TURKU, FINLAND; UNIV EASTERN FINLAND, FINLAND")
)
institutions_clean(papers, openalex = FALSE)
#> institutions: 3 institutions in 3 of 3 papers; links by OpenAlex ID 0, by an address OpenAlex linked 0, by OpenAlex name 0, from address text 4, by your table or sheet 0
#>            TI   DI
#> 1   Paper one <NA>
#> 2   Paper two <NA>
#> 3 Paper three <NA>
#>                                                                     C1
#> 1         DEPARTMENT OF COMPUTING, UNIVERSITY OF TURKU, TURKU, FINLAND
#> 2 SCHOOL OF EDUCATION, UNIVERSITY OF EASTERN FINLAND, JOENSUU, FINLAND
#> 3          UNIVERSITY-OF-TURKU, FINLAND; UNIV EASTERN FINLAND, FINLAND
#>                                       AU_UN
#> 1                       UNIVERSITY OF TURKU
#> 2             UNIVERSITY OF EASTERN FINLAND
#> 3 UNIVERSITY OF TURKU; UNIV EASTERN FINLAND

# Every merge to a sheet, for a person or a model to check.
sheet <- tempfile(fileext = ".csv")
unchanged <- institutions_clean(papers, level = "all", decisions = sheet, min_papers = 1,
                                openalex = FALSE)
#> file1ef0998c97b.csv: 3 groups and single institutions, 1 confirmed merges and 0 to check, institutions in 1 or more papers. Nothing is merged yet: add a column `merge` (yes or no), then institutions_merge(data, decisions = "file1ef0998c97b.csv"). A prompt for a model is in file1ef0998c97b_prompt.txt.
utils::read.csv(sheet)
#>   group                          name                   institution
#> 1     1           UNIVERSITY OF TURKU           UNIVERSITY-OF-TURKU
#> 2     2          UNIV EASTERN FINLAND          UNIV EASTERN FINLAND
#> 3     3 UNIVERSITY OF EASTERN FINLAND UNIVERSITY OF EASTERN FINLAND
#>              institution_backup    status papers openalex_id        source
#> 1           UNIVERSITY-OF-TURKU confirmed      1          NA same spelling
#> 2          UNIV EASTERN FINLAND    single      1          NA              
#> 3 UNIVERSITY OF EASTERN FINLAND    single      1          NA              
#>                                                                example
#> 1                                         UNIVERSITY-OF-TURKU, FINLAND
#> 2                                        UNIV EASTERN FINLAND, FINLAND
#> 3 SCHOOL OF EDUCATION, UNIVERSITY OF EASTERN FINLAND, JOENSUU, FINLAND
```
