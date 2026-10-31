# Clean bibliometric keywords

Rewrites keyword columns (such as bibliometrix's \`DE\` and \`ID\`) so
that one concept has one spelling. Every keyword is handled as a whole
term, so its position in the list, the separator spacing, and characters
such as \`(\`, \`.\` or \`+\` make no difference. In order:

1\. \*\*Cleanup\*\*: HTML entities decoded, dashes and quotes unified,
\`&\` written \`AND\`, a leading \`#\` dropped, spacing squeezed, upper
case. 2. \*\*Brackets\*\*: a plural marker is dropped (\`VOICE
ASSISTANT(S)\`); an acronym in brackets is dropped and remembered
(\`NATURAL LANGUAGE PROCESSING (NLP)\`, also \`B-RNN (BI-DIRECTIONAL
...)\`); other brackets lose only the bracket characters. 3.
\*\*Acronyms\*\* (\`expand_acronyms = TRUE\`): a keyword that is exactly
an acronym seen in brackets becomes its long form (\`NLP\` becomes
\`NATURAL LANGUAGE PROCESSING\`), but only when no other keyword in the
data spells the same initials (\`HRI\` stays when both \`HUMAN-ROBOT
INTERFACE\` and \`HUMAN-ROBOT INTERACTION\` occur). 4. \*\*Replace\*\*:
whole-term \`OLD\` to \`NEW\` replacements from \`replace\`, cleaned the
same way; chains (\`A\` to \`B\`, \`B\` to \`C\`) resolve to the last
term, and a later row overrides an earlier one that contradicts it. 5.
\*\*Merge similar\*\* (\`merge_similar = TRUE\`): keywords that differ
only by plural, hyphen, space or punctuation (\`CHAT BOT\`,
\`CHAT-BOT\`, \`CHATBOTS\`) become one: a \`NEW\` term from \`replace\`
if one is among them, else the form in most papers. 6. \*\*Drop\*\* the
\`drop\` keywords, then keep each keyword once per paper.

## Usage

``` r
keywords_clean(
  data,
  columns = "DE",
  replace = NULL,
  drop = NULL,
  merge_similar = TRUE,
  expand_acronyms = TRUE
)
```

## Arguments

- data:

  A data frame with one row per paper. The result of another
  \`clean\_\*()\` function works too, so the cleaners chain: \`M \|\>
  authors_clean(scopus = "Dlds") \|\> keywords_clean()\`.

- columns:

  Names of the keyword columns, \`;\`-separated. Default \`"DE"\`
  (bibliometrix's author keywords); \`c("DE", "ID")\` adds index
  keywords.

- replace:

  Your replacement table: a data frame with columns \`OLD\` and \`NEW\`
  (more columns are ignored), or the path to a \`.csv\`, \`.xlsx\` or
  \`.xls\` file holding one. An \[ask_ai()\] result also works (its
  pairs with \`decision\` \`accept\` are applied), and so does a review
  sheet from \[review_sheet()\] (pairs whose \`decision_2\`, or else
  \`decision\`, is \`accept\`). Several tables or paths are applied in
  order, so a later table corrects an earlier one; within a table a
  later row overrides an earlier one. Reading \`.xlsx\`/\`.xls\` needs
  readxl.

- drop:

  Keywords to delete, e.g. \`c("SURVEY", "CASE STUDY")\`. Similar
  spellings are deleted too (\`"SURVEY"\` also drops \`SURVEYS\`).

- merge_similar:

  Merge keywords that differ only by plural, hyphen, space or
  punctuation? Default \`TRUE\`.

- expand_acronyms:

  Expand a keyword that is an acronym into the long form found in
  brackets elsewhere in the data? Default \`TRUE\`.

## Value

A \`saqr_keywords\` object. Use \`as.data.frame()\` on it:

- \`as.data.frame(x)\`: \`data\` with the keyword columns rewritten,
  \`"; "\`-separated, each keyword once per paper.

- \`as.data.frame(x, what = "changes")\`: one row per keyword that
  changed, most papers first: \`column\`, \`from\` (as written in
  \`data\`), \`to\` (\`NA\` when dropped), \`reason\` (\`cleanup\`,
  \`brackets\`, \`acronym\`, \`replaced\`, \`merged\`, \`dropped\`,
  joined by \`+\`) and \`papers\`.

- \`as.data.frame(x, what = "terms")\`: one row per cleaned keyword,
  most papers first: \`column\`, \`keyword\`, \`papers\` and \`forms\`
  (how many written forms it gathers).

\`summary()\` returns one row per column with counts.

## Errors

Raises \`saqrmisc_bad_input\` for a missing or non-character keyword
column, a \`replace\` table without \`OLD\` and \`NEW\`, or a
\`replace\` file that does not exist or is not
\`.csv\`/\`.xlsx\`/\`.xls\`; \`saqrmisc_missing_package\` for an
\`.xlsx\` file without readxl. Warns \`saqrmisc_replace_conflict\`,
listing each row, when rows are overridden by a later row: the same
\`OLD\` with another \`NEW\`, or a loop (\`A\` to \`B\` above \`B\` to
\`A\`).

## Examples

``` r
papers <- data.frame(
  id = 1:3,
  DE = c("CHATBOT;  NATURAL LANGUAGE PROCESSING (NLP);  AI",
         "CHAT-BOTS;  NLP;  ARTIFICIAL INTELLIGENCE",
         "CHATBOTS;  E-LEARNING;  SURVEY")
)
my_table <- data.frame(OLD = "ARTIFICIAL INTELLIGENCE", NEW = "AI")
cleaned <- keywords_clean(papers, replace = my_table, drop = "SURVEY")
cleaned
#> Keyword cleaning
#>   DE: 9 written forms became 4 keywords in 3 papers (1 replaced, 2 merged, 1 acronyms expanded, 1 dropped)
#> as.data.frame(x) gives the cleaned papers; keywords_changes(x) lists every change.
as.data.frame(cleaned)
#>   id                                       DE
#> 1  1 CHATBOT; NATURAL LANGUAGE PROCESSING; AI
#> 2  2 CHATBOT; NATURAL LANGUAGE PROCESSING; AI
#> 3  3                      CHATBOT; E-LEARNING
```
