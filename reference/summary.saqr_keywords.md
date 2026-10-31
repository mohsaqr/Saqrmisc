# Summarise a keywords_clean() result

Summarise a keywords_clean() result

## Usage

``` r
# S3 method for class 'saqr_keywords'
summary(object, ...)
```

## Arguments

- object:

  A \[keywords_clean()\] result.

- ...:

  Ignored.

## Value

A data frame with one row per keyword column: \`column\`, \`papers\`
(with at least one keyword), \`forms_before\` (distinct written
keywords), \`keywords_after\`, \`changed\` (written keywords that
changed) and one column per reason (\`cleanup\`, \`brackets\`,
\`acronym\`, \`replaced\`, \`merged\`, \`dropped\`) counting the written
keywords it touched.
