# The first suggested merges

Keeps the first \`n\` pairs of a \[suggest_merges()\] or \[ask_ai()\]
result, in its order: the closest in meaning (\`cosine\`) first. Use it
to try a model on a few pairs before sending them all.

## Usage

``` r
# S3 method for class 'saqr_proposals'
head(x, n = 20L, ...)
```

## Arguments

- x:

  A \[suggest_merges()\] or \[ask_ai()\] result.

- n:

  How many pairs to keep. Default 20.

- ...:

  Ignored.

## Value

A result of the same class with at most \`n\` pairs.

## Examples

``` r
papers <- data.frame(DE = c("BEHAVIOR CHANGE; SOCIAL NETWORK; E-LEARNING",
                            "BEHAVIOUR CHANGE; SOCIAL NETWORKS; ELEARNING"))
head(suggest_merges(papers, meaning = FALSE), 2)
#> 2 possible merges in DE (spelling 1, word order 1, word ending 1)
#> AI: not asked yet
#> 
#>  pair keyword         variant          found_by              
#>  1    BEHAVIOR CHANGE BEHAVIOUR CHANGE spelling              
#>  2    SOCIAL NETWORK  SOCIAL NETWORKS  word order + word e...
```
