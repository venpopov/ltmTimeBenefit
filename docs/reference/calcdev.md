# Calculate the deviance of a model

## Description

This function calculates the deviance of a serial_recall model given a set of parameters and data.
The deviance is a measure of how well the model fits the data.

## Usage

```r
calcdev(params, dat, exclude_sp1 = FALSE, ...)
```

## Arguments

* `params`: A named vector of model parameters
* `dat`: A data frame containing the data
* `exclude_sp1`: A logical indicating whether to exclude the first item from the likelihood calculation
* `...`: Additional arguments to pass to `serial_recall()`

## Value

The deviance of the model

## Examples

```r
params <- c(prop = 0.5, prop_ltm = 0.3, tau = 0.2, gain = 1, rate = 0.4)
data <- data.frame(
  ISI = c(100, 200, 300), item_in_ltm = c(TRUE, FALSE, TRUE),
  n_correct = c(10, 15, 20), n_total = c(20, 20, 20)
)
calcdev(params, data)
```


