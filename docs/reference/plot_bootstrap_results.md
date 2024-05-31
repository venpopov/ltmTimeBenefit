# Plot Bootstrap Results

## Description

This function plots the bootstrap results for a given dataset.

## Usage

```r
plot_bootstrap_results(data)
```

## Arguments

* `data`: The dataset containing the bootstrap results.

## Value

A ggplot object displaying the histogram of the bootstrap results for each parameter.

## Examples

```r
tar_load(exp1_data)
data <- replicate(10, boot_est(exp1_data), simplify = FALSE)
data <- do.call(rbind, data)
plot_bootstrap_results(data)
```


