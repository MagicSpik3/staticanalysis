# Harvest Variables

Scans a directory of configuration files (CSVs) and compiles a census of
all variables being assigned to (LHS of assignments).

## Usage

``` r
harvest_variables(dir_path)
```

## Arguments

- dir_path:

  String. Path to the directory containing rule files.

## Value

A tibble with columns: variable, count, file_sources.

## Author

Mark London
