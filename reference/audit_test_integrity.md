# Audit Test Integrity

Scans test files for "Mocking the System Under Test" anti-patterns.
Flags test files that define their own functions instead of importing
them.

## Usage

``` r
audit_test_integrity(dir_path = ".")
```

## Arguments

- dir_path:

  String. Project root.

## Value

A dataframe of suspicious test files.

## Author

Mark London
