# Summarise an institutions_clean() result

Summarise an institutions_clean() result

## Usage

``` r
# S3 method for class 'saqr_institutions'
summary(object, ...)
```

## Arguments

- object:

  An \[institutions_clean()\] result.

- ...:

  Ignored.

## Value

A data frame with one row per \`source\` (\`openalex\`, \`openalex
address\`, \`openalex name\`, \`review\`, \`address\`): \`institutions\`
(distinct names), \`papers\` and \`links\` (paper-institution pairs).
