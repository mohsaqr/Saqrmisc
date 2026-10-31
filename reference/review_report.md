# Report on a checked keyword review sheet

Reads a sheet from \[review_sheet()\] after you have checked it: how
many pairs were accepted, rejected or are still to check, and how often
the AI agreed with you. Each pair's \`final\` decision is your
\`decision_2\` where you gave one, else the AI's \`decision\`.

## Usage

``` r
review_report(file)
```

## Arguments

- file:

  The review sheet: the \`.xlsx\` or \`.csv\` file from
  \[review_sheet()\]. For \`.csv\`, its \`\_ai.csv\` beside it supplies
  the AI's answers.

## Value

A \`saqr_review\` object. Print it for the report. \`summary()\` gives a
data frame with two rows, \`pairs\` = \`"blind"\` (the audit pairs: an
unbiased measure of the AI) and \`"all"\`: \`total\`, and the \`final\`
decisions \`accept\`, \`reject\`, \`check\`; then, over the pairs where
you gave a \`decision_2\` and the AI said \`same\` or \`different\`,
\`ai_agreed\`, \`ai_disagreed\`, \`agreement\` and its 95% Wilson
interval \`ci_low\`, \`ci_high\`. \`as.data.frame(x)\` gives one row per
pair: \`pair\`, \`keyword\`, \`variant\`, \`decision\` (the AI's),
\`decision_2\` (yours), \`final\`, \`name\`, \`ai_verdict\`, \`blind\`;
\`what = "replace"\` gives the \`OLD\`/\`NEW\` table that
\[keywords_clean()\] applies.

## Errors

Raises \`saqrmisc_bad_input\` for a missing file, a \`.xlsx\` without a
\`review\` sheet, a \`decision\` or \`decision_2\` other than
\`accept\`, \`reject\` or \`check\`, or an accepted pair without a
\`name\`; \`saqrmisc_missing_package\` without readxl.

## References

Wilson, E. B. (1927). Probable inference, the law of succession, and
statistical inference. \*Journal of the American Statistical
Association\*, 22(158), 209-212.

## See also

\[review_sheet()\], \[keywords_clean()\]

## Examples

``` r
if (requireNamespace("writexl", quietly = TRUE) &&
    requireNamespace("readxl", quietly = TRUE)) {
  papers <- data.frame(DE = c("BEHAVIOR CHANGE; SOCIAL NETWORK",
                              "BEHAVIOUR CHANGE; SOCIAL NETWORKING"))
  file <- tempfile(fileext = ".xlsx")
  review_sheet(suggest_merges(papers, meaning = FALSE), file)
  report <- review_report(file)
  report
  summary(report)
  as.data.frame(report)
}
#> file21ed2bc8f9d7.xlsx: 2 pair(s) to check (0 of them blind), 0 accepted and 0 rejected by the AI. Write yours in decision_2.
#>   pair         keyword           variant decision decision_2 final
#> 1    1 BEHAVIOR CHANGE  BEHAVIOUR CHANGE    check       <NA> check
#> 2    2  SOCIAL NETWORK SOCIAL NETWORKING    check       <NA> check
#>              name ai_verdict blind
#> 1 BEHAVIOR CHANGE         NA FALSE
#> 2  SOCIAL NETWORK         NA FALSE
```
