# Summary method for moe_analysis objects

Summary method for moe_analysis objects

## Usage

``` r
# S3 method for class 'moe_analysis'
summary(object, ...)
```

## Arguments

- object:

  An moe_analysis object

- ...:

  Additional arguments (ignored)

## Value

A tibble with one row per fitted model, sorted from best to worst by
BIC. The \`best\` column identifies the selected model.
