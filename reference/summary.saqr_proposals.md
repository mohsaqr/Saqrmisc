# Count suggested keyword merges

Count suggested keyword merges

## Usage

``` r
# S3 method for class 'saqr_proposals'
summary(object, ...)
```

## Arguments

- object:

  A \[suggest_merges()\] or \[ask_ai()\] result.

- ...:

  Ignored.

## Value

A data frame with one row per \`found_by\`: \`pairs\`, and how many the
AI judged \`same\`, \`different\`, \`unsure\`, \`failed\`, or has not
been asked about (\`not_asked\`).
