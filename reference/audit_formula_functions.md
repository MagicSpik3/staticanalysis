# Audit Functions Used in Rules

Scans a directory of rule CSVs, parses the R code in them, and counts
which functions are actually being called.

## Usage

``` r
audit_formula_functions(dir_path)
```

## Arguments

- dir_path:

  String. Path to directory containing CSV rules.

## Value

A frequency table of function calls (e.g., "mutate": 500, "if_else":
20).
