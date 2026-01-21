# Check Test Coverage

Cross-references an inventory of functions against test files to see if
they are mentioned.

## Usage

``` r
check_test_coverage(inventory, test_files)
```

## Arguments

- inventory:

  A dataframe returned by scan_definitions().

- test_files:

  A character vector of test file paths.

## Value

The inventory dataframe with an added 'called_in_test' logical column.
