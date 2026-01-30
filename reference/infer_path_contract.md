# Infer Path Contract

Scans the code to determine which variables are treated as Inputs (Read)
and which are treated as Outputs (Write).

## Usage

``` r
infer_path_contract(dir_path = ".", config_var = "paths")
```

## Arguments

- dir_path:

  String. Project root.

- config_var:

  String. The name of your paths list variable (default "paths").

## Value

A dataframe describing the contract (Input/Output) for each key.

## Author

Mark London
