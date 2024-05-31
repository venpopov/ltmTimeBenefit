# Plot Linear RV Recovery

## Description

This function plots the recovery of a linear random variable (RV) over time.

## Usage

```r
plot_linear_rv_recovery(r, t, title = "Uniform distribution")
```

## Arguments

* `r`: A vector representing the recovery rates sampled from some distribution.
* `t`: A vector representing the time values.
* `title`: The title of the plot (default is "Uniform distribution").

## Details

Average recovery is given as a red line, while individual trajectories are given as black lines.

## Value

A ggplot object representing the plot of linear RV recovery over time.

## Examples

```r
r <- runif(1000, 0, 1)
t <- seq(0, 5, by = 0.01)
plot_linear_rv_recovery(r, t, title = "Uniform distribution")
```


