# Generate a bootstrapped dataset

## Description

This function takes in a dataset and generates a bootstrapped dataset by randomly sampling
unique IDs with replacement. For each ID and condition, it calculates the proportion of correct
responses and then generates a new dataset by sampling from a binomial distribution with the
calculated proportion of correct responses.

## Usage

```r
gen_boot_dataset(data)
```

## Arguments

* `data`: The input dataset

## Value

A bootstrapped dataset with aggregated summary statistics

## Examples

```r
data <- read.csv("data.csv")
boot_data <- gen_boot_dataset(data)
```


