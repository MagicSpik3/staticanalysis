# Detect Code Logic Duplicates (Clones)

Scans the project for functions that are structurally identical. Uses
'terminal' token analysis to distinguish operators like + vs \*.

## Usage

``` r
detect_duplicates(dir_path, ignore_constants = TRUE)
```

## Arguments

- dir_path:

  String. Path to the project root.

- ignore_constants:

  Logical. If TRUE, treats 'x \* 12' and 'x \* 52' as identical.

## Value

A tibble grouping functions by their logic signature.
