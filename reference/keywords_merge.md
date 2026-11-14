# Bring a checked keyword sheet back into the data

Applies the sheet that \[keywords_clean()\] wrote with \`level =
"all"\`, after a person or a model has filled in its \`merge\` column,
and returns the data frame. The confirmed steps of \[keywords_clean()\]
are applied again, then the sheet: \* a \`needs_revision\` row with
\`merge\` \`yes\` becomes its \`name\`; \* a \`confirmed\` row is merged
unless \`merge\` says \`no\`; with \`no\` it keeps its own spelling; \*
an edited \`name\` renames its whole group; \* an empty \`merge\` keeps
what the row's \`status\` says.

Rows are matched on \`keyword_backup\` (on \`keyword\` when a sheet has
no backup column). Pass the same \`columns\` and \`replace\` as to
\[keywords_clean()\], and the data as it was before cleaning: a
confirmed merge marked \`no\` can only be undone from there (on cleaned
data everything else gives the same result).

## Usage

``` r
keywords_merge(
  data,
  decisions,
  columns = "DE",
  replace = NULL,
  output = c("minimal", "all")
)
```

## Arguments

- data:

  A data frame with one row per paper.

- decisions:

  The checked sheet, \`.xlsx\` or \`.csv\`.

- columns, replace, output:

  As in \[keywords_clean()\].

## Value

\`data\`, same class, rows and columns, with the keyword columns
rewritten where they change; with \`output = "all"\`, plus the
\`\_ORIGINAL\` columns. A message reports the counts per column and the
rows applied from the sheet.

## Errors

Raises \`saqrmisc_bad_input\` for a missing sheet, one without \`name\`,
\`status\`, \`merge\` and \`keyword_backup\` (or \`keyword\`), or a
\`merge\` other than empty, \`yes\` or \`no\`; and the errors of
\[keywords_clean()\].

## See also

\[keywords_clean()\]

## Examples

``` r
papers <- data.frame(
  TI = c("Chatbots in class", "Bots that talk", "Fairness at work", "Unfair bosses"),
  DE = c("CHATBOT; NATURAL LANGUAGE PROCESSING (NLP)", "CHAT-BOTS; NLP",
         "INTERPERSONAL JUSTICE", "INTERPERSONAL INJUSTICE; NLP")
)
sheet <- tempfile(fileext = ".csv")
cleaned <- keywords_clean(papers, level = "all", decisions = sheet, min_papers = 1)
#> keywords: DE: 6 written forms became 5 keywords; 2 papers changed
#> file1e5c5a47b325.csv: 3 groups, 1 confirmed merges (applied) and 2 to check, keywords in 1 or more papers. Fill in `merge` (yes or no), then keywords_merge(data, decisions = "file1e5c5a47b325.csv"). A prompt for a model is in file1e5c5a47b325_prompt.txt.
checked <- utils::read.csv(sheet)
checked$merge <- ifelse(checked$keyword == "NLP", "yes", "no")
utils::write.csv(checked, sheet, row.names = FALSE)
keywords_merge(papers, decisions = sheet)
#> keywords: DE: 6 written forms became 5 keywords; 3 papers changed
#> keywords_merge: from file1e5c5a47b325.csv, 1 merge(s) and renames applied, 1 confirmed merge(s) undone
#>                  TI                                                   DE
#> 1 Chatbots in class                 CHATBOT; NATURAL LANGUAGE PROCESSING
#> 2    Bots that talk               CHAT-BOTS; NATURAL LANGUAGE PROCESSING
#> 3  Fairness at work                                INTERPERSONAL JUSTICE
#> 4     Unfair bosses INTERPERSONAL INJUSTICE; NATURAL LANGUAGE PROCESSING
```
