# Compare Implementations

Verifies that functions defined in a single legacy file (Monolith) are
identical in logic to those split into a directory of files
(Refactored). Uses AST comparison to ignore comments and whitespace
differences.

## Usage

``` r
compare_implementations(monolith_path, refactored_dir)
```

## Arguments

- monolith_path:

  String. Path to the legacy monolithic R script.

- refactored_dir:

  String. Path to the directory containing refactored R files.

## Value

A list of comparison results for each function found.
