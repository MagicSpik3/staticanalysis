# Scan Definitions

Parses R files to find all defined functions and global variables.

## Usage

``` r
scan_definitions(files, root_dir)
```

## Arguments

- files:

  Character vector of file paths to scan.

- root_dir:

  String. The root directory (used to calculate relative paths).

## Value

A tibble of definitions (name, type, file).
