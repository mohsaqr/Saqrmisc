# Get Best Model

Get Best Model

## Usage

``` r
get_best_model(results, criterion = "bic", what = c("name", "result", "fit"))
```

## Arguments

- results:

  Object from run_full_moe_analysis

- criterion:

  Selection criterion: "bic", "aic", or "icl"

- what:

  What to return: the model "name", the complete stored "result", or the
  raw fitted MoEClust "fit".

## Value

The requested representation of the best model.
