# Execute an expression and save the result to a file or load the result from a file if it already exists.

## Description

This function allows you to either run an expression or load the result from a file.

## Usage

```r
run_or_load(expression, file, ..., force = FALSE)
```

## Arguments

* `expression`: The expression to be evaluated or loaded.
* `file`: The file path where the result will be saved or loaded from.
* `...`: Additional arguments to be passed to the expression.
* `force`: Logical value indicating whether to force the evaluation of the expression, even if the file exists.

## Value

The result of the expression.

## Examples

```r
# Run an expression and save the result to a file
file <- tempfile(fileext = ".rds")
run_or_load(rnorm(1e7), file)
run_or_load(rnorm(1e7), file) # loads the result from the file
```


