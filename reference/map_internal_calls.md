# Map Internal Function Calls

Scans the body of each function in the inventory to find calls to other
project functions.

## Usage

``` r
map_internal_calls(inventory, dir_path = ".")
```

## Arguments

- inventory:

  The dataframe from audit_inventory()

- dir_path:

  String. The project root (required to resolve relative paths in
  inventory).

## Value

A dataframe of edges (from, to)

## Author

Mark London
