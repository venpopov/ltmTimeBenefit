# Aggregate Data

## Description

This function aggregates the given data by grouping it based on the chunk, gap, and itemtype columns.
It then calculates various summary statistics including the total count, the count of correct values,
and the proportion of correct values.

## Usage

```r
aggregate_data(data)
```

## Arguments

* `data`: The input data frame.

## Value

A new data frame with the aggregated data.

## Examples

```r
tar_load(exp1_data)
aggregate_data(exp1_data)
```


