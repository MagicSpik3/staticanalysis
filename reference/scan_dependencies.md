# Scan Project Dependencies (Robust AST Method)

Scans all R scripts for package usage. Returns usage counts to help
identify "bloated" single-use imports.

## Usage

``` r
scan_dependencies(dir_path)
```

## Arguments

- dir_path:

  String. Path to the project root.

## Value

A list containing 'usage_stats', 'ghosts', and 'unused'.
