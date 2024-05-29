# Perform bootstrapped estimation

## Description

This function performs bootstrapped estimation using the provided data and parameters.
It uses the serial recall model for estimation

## Usage

```r
boot_est(data, start, exclude_sp1 = TRUE, growth = "asy", ...)
```

## Arguments

* `data`: The input data for estimation.
* `start`: The starting values for estimation. If not provided, default values will be used.
* `exclude_sp1`: Logical value indicating whether to exclude the first item in the data. Default is TRUE.
* `growth`: The growth type for estimation. Default is "asy".
* `...`: Additional arguments to be passed to the estimation function.

## Value

A data frame containing the estimated model parameters.

## Examples

```r
data <- read.csv("data.csv")
boot_est(data)
```


