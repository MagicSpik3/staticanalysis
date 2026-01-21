# Audit Project Inventory

Scans a project to index all defined functions and checks test coverage.
Also validates if functions are in correctly named files.

## Usage

``` r
audit_inventory(dir_path)
```

## Arguments

- dir_path:

  String. Path to the package root.

## Value

A tibble listing objects, their locations, coverage, and placement
status.
