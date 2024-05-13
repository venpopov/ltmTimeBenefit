# Get data object from a file

## Description

This function safely loads the environment from an Rdata file and returns
an object from it.

## Usage

```r
extract_object_from_rdata(path, object_name = "data_an")
```

## Arguments

* `path`: The path to the file containing the Rdata file
* `object_name`: The name of the object to extract from the Rdata file

## Value

The loaded data object.


