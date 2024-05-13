# Inverse Logit Transformation

## Description

This function applies the inverse logit transformation to a given value.

## Usage

```r
inv_logit(x, lb = 0, ub = 1)
```

## Arguments

* `x`: The input value to be transformed.
* `lb`: The lower bound of the transformed value. Default is 0.
* `ub`: The upper bound of the transformed value. Default is 1.

## Value

The transformed value between the lower and upper bounds.

## Examples

```r
inv_logit(0) # returns 0.5
inv_logit(0, lb = 0, ub = 10) # returns 5
```


