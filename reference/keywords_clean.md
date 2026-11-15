# Clean bibliometric keywords

Gives each concept one spelling in the author keywords (bibliometrix's
\`DE\`) and returns the data frame, ready for bibliometrix. Only papers
whose keywords change are rewritten; the others keep their text exactly.
Nothing is ever deleted.

The confirmed steps, applied with no review: 1. \*\*Cleanup\*\*: HTML
entities decoded, dashes and quotes unified, \`&\` written \`AND\`, a
leading \`#\` and trailing punctuation dropped, spacing squeezed, upper
case. A keyword cleanup would leave empty (\`.\`) keeps its written
form. 2. \*\*Brackets\*\*: a plural marker goes (\`VOICE
ASSISTANT(S)\`); an acronym in brackets goes (\`NATURAL LANGUAGE
PROCESSING (NLP)\`, also \`B-RNN (BI-DIRECTIONAL ...)\`); other brackets
lose only the brackets. 3. \*\*Your tables\*\*: the \`OLD\` to \`NEW\`
rows of \`replace\`; chains (\`A\` to \`B\`, \`B\` to \`C\`) resolve to
the last term, and a later row overrides an earlier one that contradicts
it. 4. \*\*Same keyword, written differently\*\*: forms that differ only
by case, punctuation, accents, spaces, hyphens or plural (\`CHAT BOT\`,
\`CHAT-BOT\`, \`CHATBOTS\`) become one, in word order (\`TEXT TO
SPEECH\` and \`SPEECH TO TEXT\` stay apart): a \`NEW\` term of
\`replace\` if one is among them, else the form in most papers. 5. Each
keyword once per paper.

\`level = "all"\` merges nothing: it returns \`data\` unchanged and
writes every merge to the \`decisions\` sheet, for a person, a model or
a tool such as OpenRefine to check outside R, with a short prompt for a
model beside it. The sheet has one group per \`name\` (the keyword the
group becomes): its \`confirmed\` merges (the steps above) and its
\`needs_revision\` candidates, found in six ways (\`source\`), none by
comparing all pairs: \* \`spelling\`: character trigrams, Jaccard 0.7 or
more, by MinHash; \* \`word order\`: the same words in another order
(\`PSYCHOLOGY, CLINICAL\`); \* \`ngram\`: the same letter pairs
(OpenRefine's 2-gram fingerprint); \* \`stem\`: the words share their
stems (Porter; needs SnowballC); \* \`acronym\`: an acronym whose long
form is written in brackets elsewhere in the data; \* \`initials\`: an
acronym spelled by the initials of a keyword (\`NLP\`, \`NATURAL
LANGUAGE PROCESSING\`).

Most of these propose some wrong pairs (\`APPLICATION\` /
\`APPLICABILITY\` share a stem), which is why candidates wait for a
\`yes\`. Spelled alike is not the same concept (\`JUSTICE\` /
\`INJUSTICE\`). Only keywords in at least \`min_papers\` papers go to
the sheet, each of them, grouped or alone (\`single\`), so a tool such
as OpenRefine can also cluster the whole list itself. Bring the checked
sheet back with \[keywords_merge()\]: by its \`merge\` answers, or by
the edits of \`keyword\` against the untouched \`keyword_backup\`.

## Usage

``` r
keywords_clean(
  data,
  columns = "DE",
  level = c("confirmed", "all"),
  decisions = NULL,
  min_papers = 5,
  replace = NULL,
  titles = "TI",
  output = c("minimal", "all")
)
```

## Arguments

- data:

  A data frame with one row per paper, such as a bibliometrix data
  frame. The cleaners chain: \`M \|\> authors_clean(ids =
  "scopus_exports") \|\> keywords_clean()\`.

- columns:

  Names of the keyword columns, \`;\`-separated. Default \`"DE"\`
  (author keywords).

- level:

  \`"confirmed"\` (default) or \`"all"\`; see above.

- decisions:

  With \`level = "all"\`: the sheet to write, a new path ending in
  \`.xlsx\` or \`.csv\` (an existing file is never replaced). Its
  columns: \`group\`, \`name\`, \`keyword\`, \`keyword_backup\` (a copy
  of \`keyword\` that \[keywords_merge()\] matches on, so an edit of
  \`keyword\` does no harm), \`status\` (\`single\` for a keyword in no
  group, \`confirmed\` or \`needs_revision\`), \`papers\`, \`source\`
  (\`same spelling\`, \`your table\`, or the ways a candidate was found,
  see above) and \`example\` (the title of a paper using the keyword).
  The prompt goes to \`\<sheet\>\_prompt.txt\` and, in an \`.xlsx\`, to
  a second tab \`prompt\`.

- min_papers:

  Only keywords in at least this many papers go into the sheet. Default
  5: on a 2-million-paper psychology corpus, the 10% of author keywords
  in 5 or more papers carry 81% of all keyword uses.

- replace:

  Your replacement table: a data frame with columns \`OLD\` and \`NEW\`
  (more columns are ignored), or the path to a \`.csv\`, \`.xlsx\` or
  \`.xls\` file holding one. An \[ask_ai()\] result or a
  \[review_sheet()\] file also works (its accepted pairs are applied).
  Several tables are applied in order, so a later table corrects an
  earlier one. Reading \`.xlsx\`/\`.xls\` needs readxl.

- titles:

  Name of the title column, used for the \`example\` column of the
  sheet. Default \`"TI"\`; skipped when \`data\` has no such column.

- output:

  \`"minimal"\` (default) returns \`data\` with the keyword columns
  rewritten and nothing added. \`"all"\` also adds
  \`\<column\>\_ORIGINAL\` for each column, as it came in (kept when
  \`data\` already has it).

## Value

With \`level = "confirmed"\`: \`data\`, same class, rows and columns,
with the keyword columns rewritten where they change (\`";
"\`-separated); with \`output = "all"\`, plus the \`\_ORIGINAL\`
columns. With \`level = "all"\`: \`data\` unchanged, invisibly (the
sheet is the result). A message reports, per column, the written forms
that became one keyword and the papers changed.

## Errors

Raises \`saqrmisc_bad_input\` for a missing or non-character keyword
column, a \`replace\` table without \`OLD\` and \`NEW\` or a \`replace\`
file that does not exist or is not \`.csv\`/\`.xlsx\`/\`.xls\`,
\`decisions\` given with \`level = "confirmed"\`, missing or already
existing with \`"all"\`, and a \`min_papers\` that is not one positive
number; \`saqrmisc_missing_package\` for \`.xlsx\` without readxl /
writexl. Warns \`saqrmisc_replace_conflict\`, listing each row, when
\`replace\` has rows overridden by a later row.

## References

Broder, A. Z. (1997). On the resemblance and containment of documents.
\*Proceedings of Compression and Complexity of Sequences\*, 21-29.
[doi:10.1109/SEQUEN.1997.666900](https://doi.org/10.1109/SEQUEN.1997.666900)

Porter, M. F. (1980). An algorithm for suffix stripping. \*Program\*,
14(3), 130-137. \doi10.1108/eb046814

Leskovec, J., Rajaraman, A., & Ullman, J. D. (2020). \*Mining of Massive
Datasets\* (3rd ed.), chapter 3. Cambridge University Press.

## See also

\[keywords_merge()\] to bring the checked sheet back.

## Examples

``` r
papers <- data.frame(
  TI = c("Paper one", "Paper two", "Paper three"),
  DE = c("CHATBOT; NATURAL LANGUAGE PROCESSING (NLP); AI",
         "CHAT-BOTS; NLP; ARTIFICIAL INTELLIGENCE",
         "CHATBOTS; E-LEARNING; SURVEY")
)
keywords_clean(papers)
#> keywords: DE: 9 written forms became 7 keywords; 3 papers changed
#>            TI                                       DE
#> 1   Paper one CHATBOT; NATURAL LANGUAGE PROCESSING; AI
#> 2   Paper two    CHATBOT; NLP; ARTIFICIAL INTELLIGENCE
#> 3 Paper three              CHATBOT; E-LEARNING; SURVEY
keywords_clean(papers, replace = data.frame(OLD = "AI", NEW = "ARTIFICIAL INTELLIGENCE"),
               output = "all")
#> keywords: DE: 9 written forms became 6 keywords; 3 papers changed
#>            TI                                                            DE
#> 1   Paper one CHATBOT; NATURAL LANGUAGE PROCESSING; ARTIFICIAL INTELLIGENCE
#> 2   Paper two                         CHATBOT; NLP; ARTIFICIAL INTELLIGENCE
#> 3 Paper three                                   CHATBOT; E-LEARNING; SURVEY
#>                                      DE_ORIGINAL
#> 1 CHATBOT; NATURAL LANGUAGE PROCESSING (NLP); AI
#> 2        CHAT-BOTS; NLP; ARTIFICIAL INTELLIGENCE
#> 3                   CHATBOTS; E-LEARNING; SURVEY

# Every merge to a sheet, for a person or a model to check.
sheet <- tempfile(fileext = ".csv")
cleaned <- keywords_clean(papers, level = "all", decisions = sheet, min_papers = 1)
#> file1ff627a70ed0.csv: 5 groups and single keywords, 2 confirmed merges and 2 to check, keywords in 1 or more papers. Nothing is merged yet: add a column `merge` (yes or no), then keywords_merge(data, decisions = "file1ff627a70ed0.csv"). A prompt for a model is in file1ff627a70ed0_prompt.txt.
utils::read.csv(sheet)
#>   group                        name    keyword keyword_backup         status
#> 1     1                     CHATBOT  CHAT-BOTS      CHAT-BOTS      confirmed
#> 2     1                     CHATBOT   CHATBOTS       CHATBOTS      confirmed
#> 3     2     ARTIFICIAL INTELLIGENCE         AI             AI needs_revision
#> 4     3                  E-LEARNING E-LEARNING     E-LEARNING         single
#> 5     4 NATURAL LANGUAGE PROCESSING        NLP            NLP needs_revision
#> 6     5                      SURVEY     SURVEY         SURVEY         single
#>   papers        source     example
#> 1      1 same spelling            
#> 2      1 same spelling            
#> 3      1      initials   Paper one
#> 4      1               Paper three
#> 5      1       acronym   Paper two
#> 6      1               Paper three
```
