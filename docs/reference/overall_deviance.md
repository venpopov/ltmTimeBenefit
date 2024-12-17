# Calculate the overall deviance

## Description

This function calculates the overall deviance for a given set of parameters and data.
It calculates the deviance for each list entry of split_data and then sums them

## Usage

```r
overall_deviance(params, split_data, ..., priors = list())
```

## Arguments

* `params`: A vector of parameters.
* `split_data`: a data.frame split by `split()`
* `...`: additional arguments to pass to calcdev()
* `priors`: a named list

## Value

The overall deviance.


