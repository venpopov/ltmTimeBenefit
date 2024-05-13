# Calculate the overall deviance

## Description

This function calculates the overall deviance for a given set of parameters and data.
It splits the dataset by the given column and calculates the deviance for each subset.
The overall deviance is the sum of the deviances for each subset.

## Usage

```r
overall_deviance(params, data, by = c("chunk", "gap"), ..., priors = list())
```

## Arguments

* `params`: A vector of parameters.
* `data`: A data frame containing the data.
* `by`: The column(s) to split the data by.

## Value

The overall deviance.


