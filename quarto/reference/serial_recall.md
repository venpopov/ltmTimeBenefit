# Serial Recall Model

## Description

This function implements the model currently described in the draft on page 19.
It gives the predicted recall probability for each item in a set of items.

## Usage

```r
serial_recall(
  setsize,
  ISI = rep(0.5, setsize),
  item_in_ltm = rep(TRUE, setsize),
  prop = 0.2,
  prop_ltm = 0.5,
  tau = 0.15,
  gain = 25,
  rate = 0.1,
  r_max = 1,
  lambda = 1,
  growth = "linear"
)
```

## Arguments

* `setsize`: The number of items in the set.
* `ISI`: A numeric vector representing the inter-stimulus interval for each item.
* `item_in_ltm`: A logical vector indicating whether each item is in LTM.
* `prop`: The proportion of resources allocated to each item.
* `prop_ltm`: Proportion by which the resources used by LTM items are multiplied.
* `tau`: The threshold for recall probability.
* `gain`: The gain parameter for the recall probability function.
* `rate`: The rate at which resources are recovered.
* `r_max`: The maximum amount of resources.
* `lambda`: The exponent converting resources to strength.
* `growth`: The growth function for resource recovery. Either 'linear' or 'asy'.

## Details

The function uses a simulation approach. It loops over trials

## Value

A numeric vector representing the recall probability for each item.

## Examples

```r
serial_recall(setsize = 3, ISI = rep(0.5, 3))
```


