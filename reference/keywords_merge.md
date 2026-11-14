# Bring a checked keyword sheet back into the data

Applies the sheet that \[keywords_clean()\] wrote with \`level =
"all"\`, after it was checked outside R, and returns the data frame. The
confirmed steps of \[keywords_clean()\] are applied again, then the
sheet, read one of two ways:

\* \`by = "merge"\` (default), for a person or a model that answers
\`yes\` or \`no\` in \`merge\` (or in the column \`answers\` names): a
\`decisions\` row with \`yes\` merges its keyword, with all its
spellings, into \`merge_into\` (edit \`merge_into\` to send it
elsewhere); a row left empty is not merged and is counted in the
message. The \`confirmed\` table is written with \`yes\` in every row; a
\`no\` there keeps the keyword's other spellings apart. \* \`by =
"edit"\`, for a tool that rewrites the keywords themselves, such as
OpenRefine clustering the \`keyword\` column of \`all_keywords\` (or of
\`decisions\`): every keyword whose \`keyword\` no longer equals its
untouched \`keyword_backup\` becomes the new text, with all its
spellings. \`merge\` is ignored, and an edit of case or spacing alone
changes nothing.

Pass the same \`columns\` and \`replace\` as to \[keywords_clean()\],
and the data as it was before cleaning: a confirmed merge marked \`no\`
can only be undone from there (on cleaned data everything else gives the
same result).

## Usage

``` r
keywords_merge(
  data,
  decisions,
  by = c("merge", "edit"),
  answers = "merge",
  columns = "DE",
  replace = NULL,
  output = c("minimal", "all")
)
```

## Arguments

- data:

  A data frame with one row per paper.

- decisions:

  The checked sheet, \`.xlsx\` or \`.csv\` (the decisions table; the
  other tables are read from beside it when they are there).

- by:

  \`"merge"\` (default) reads the \`merge\` column; \`"edit"\` reads the
  edits of \`keyword\` against \`keyword_backup\`. See above.

- answers:

  With \`by = "merge"\`: the column of the decisions table holding the
  yes / no answers. Default \`"merge"\`; name another when an
  application writes its answers to a column of its own.

- columns, replace, output:

  As in \[keywords_clean()\].

## Value

\`data\`, same class, rows and columns, with the keyword columns
rewritten where they change; with \`output = "all"\`, plus the
\`\_ORIGINAL\` columns. A message reports the counts per column and the
rows applied from the sheet.

## Errors

Raises \`saqrmisc_bad_input\` for a missing sheet; with \`by =
"merge"\`, a decisions table without \`merge_into\`, the \`answers\`
column and \`keyword_backup\` (or \`keyword\`), or an answer other than
empty, \`yes\` or \`no\`; with \`by = "edit"\`, no table with
\`keyword\` and \`keyword_backup\`; and the errors of
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
#> file1ec9729856a8.csv: 1 merges to decide, 1 keywords with confirmed spellings, 5 keywords in 1 or more papers. Nothing is merged yet: write yes or no in `merge`, then keywords_merge(data, decisions = "file1ec9729856a8.csv"). A prompt for a model is in file1ec9729856a8_prompt.txt.
checked <- utils::read.csv(sheet)
checked$merge <- ifelse(checked$keyword == "NLP", "yes", "no")
utils::write.csv(checked, sheet, row.names = FALSE)
keywords_merge(papers, decisions = sheet)
#> keywords: DE: 6 written forms became 4 keywords; 3 papers changed
#> keywords_merge: from file1ec9729856a8.csv, 1 merge(s) applied, 0 confirmed keyword(s) kept apart, 0 row(s) unanswered (not merged)
#>                  TI                                                   DE
#> 1 Chatbots in class                 CHATBOT; NATURAL LANGUAGE PROCESSING
#> 2    Bots that talk                 CHATBOT; NATURAL LANGUAGE PROCESSING
#> 3  Fairness at work                                INTERPERSONAL JUSTICE
#> 4     Unfair bosses INTERPERSONAL INJUSTICE; NATURAL LANGUAGE PROCESSING

# The same sheet edited the way OpenRefine edits it: keyword rewritten,
# keyword_backup untouched.
edited <- utils::read.csv(sheet)
edited$keyword[edited$keyword == "NLP"] <- "NATURAL LANGUAGE PROCESSING"
utils::write.csv(edited, sheet, row.names = FALSE)
keywords_merge(papers, decisions = sheet, by = "edit")
#> keywords: DE: 6 written forms became 4 keywords; 3 papers changed
#> keywords_merge: from file1ec9729856a8.csv, 1 edited keyword(s) applied, 0 to a spelling not in the data
#>                  TI                                                   DE
#> 1 Chatbots in class                 CHATBOT; NATURAL LANGUAGE PROCESSING
#> 2    Bots that talk                 CHATBOT; NATURAL LANGUAGE PROCESSING
#> 3  Fairness at work                                INTERPERSONAL JUSTICE
#> 4     Unfair bosses INTERPERSONAL INJUSTICE; NATURAL LANGUAGE PROCESSING
```
