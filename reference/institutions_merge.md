# Bring a checked institution sheet back into the data

Applies the sheet that \[institutions_clean()\] wrote with \`level =
"all"\`, after it was checked outside R, and returns the data frame. The
confirmed steps of \[institutions_clean()\] run again (OpenAlex's
answers come from the cache), then the sheet, read one of two ways:

\* \`by = "merge"\` (default), for a person or a model that adds a
column \`merge\` and answers \`yes\` or \`no\` in it: \* a
\`needs_revision\` row with \`yes\` becomes its \`name\`, and joins it
when the name is an identified institution; \* a \`confirmed\` row with
\`no\` keeps its own name, also when it came to an identified
institution by its address or name; \* anything else keeps what the
row's \`status\` says. \* \`by = "edit"\`, for a tool that rewrites the
names themselves, such as OpenRefine clustering the \`institution\`
column: every institution whose \`institution\` no longer equals its
untouched \`institution_backup\` becomes the new text. An edited
\`confirmed\` row moves only that spelling. Identified institutions keep
their names.

Pass the same arguments as to \[institutions_clean()\].

## Usage

``` r
institutions_merge(
  data,
  decisions,
  by = c("merge", "edit"),
  affiliations = "C1",
  doi = "DI",
  replace = NULL,
  openalex = TRUE,
  cache = .in_default_cache(),
  mailto = NULL,
  output = c("minimal", "all")
)
```

## Arguments

- data:

  A data frame with one row per paper.

- decisions:

  The checked sheet, \`.xlsx\` or \`.csv\`.

- by:

  \`"merge"\` (default) reads the \`merge\` column; \`"edit"\` reads the
  edits of \`institution\` against \`institution_backup\`.

- affiliations, doi, replace, openalex, cache, mailto, output:

  As in \[institutions_clean()\].

## Value

\`data\`, same class and rows, with \`AU_UN\` written as by
\[institutions_clean()\]; with \`output = "all"\`, plus its columns. A
message counts the rows applied from the sheet.

## Errors

Raises \`saqrmisc_bad_input\` for a missing sheet; with \`by =
"merge"\`, one without \`name\`, \`status\`, \`merge\` and
\`institution_backup\` (or \`institution\`), or a \`merge\` other than
empty, \`yes\` or \`no\`; with \`by = "edit"\`, one without
\`institution\` and \`institution_backup\`; and the errors of
\[institutions_clean()\].

## See also

\[institutions_clean()\]

## Examples

``` r
papers <- data.frame(
  DI = NA_character_,
  C1 = c("UNIVERSITY OF TURKU, FINLAND", "UNIVERSITY OF TURKU, FINLAND", "TURKU UNIVERSITY, FINLAND",
         "UNIVERSITY OF OULU, FINLAND")
)
sheet <- tempfile(fileext = ".csv")
unchanged <- institutions_clean(papers, level = "all", decisions = sheet, min_papers = 1,
                                openalex = FALSE)
#> file1ef06adf95e7.csv: 3 groups and single institutions, 0 confirmed merges and 0 to check, institutions in 1 or more papers. Nothing is merged yet: add a column `merge` (yes or no), then institutions_merge(data, decisions = "file1ef06adf95e7.csv"). A prompt for a model is in file1ef06adf95e7_prompt.txt.
checked <- utils::read.csv(sheet)
checked$merge <- ifelse(checked$institution == "TURKU UNIVERSITY", "yes", "no")
utils::write.csv(checked, sheet, row.names = FALSE)
institutions_merge(papers, decisions = sheet, openalex = FALSE)
#> institutions_merge: from file1ef06adf95e7.csv, 0 merge(s) applied, 0 confirmed merge(s) undone
#> institutions: 3 institutions in 4 of 4 papers; links by OpenAlex ID 0, by an address OpenAlex linked 0, by OpenAlex name 0, from address text 4, by your table or sheet 0
#>     DI                           C1               AU_UN
#> 1 <NA> UNIVERSITY OF TURKU, FINLAND UNIVERSITY OF TURKU
#> 2 <NA> UNIVERSITY OF TURKU, FINLAND UNIVERSITY OF TURKU
#> 3 <NA>    TURKU UNIVERSITY, FINLAND    TURKU UNIVERSITY
#> 4 <NA>  UNIVERSITY OF OULU, FINLAND  UNIVERSITY OF OULU
```
