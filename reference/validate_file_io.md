# Validate File I/O Requirements (Pre-Flight Check)

Takes the output of scan_file_io() and checks if the files actually
exist on disk, using a provided context map for variables.

## Usage

``` r
validate_file_io(io_report, context_vars = list())
```

## Arguments

- io_report:

  Dataframe. Output from scan_file_io().

- context_vars:

  Named List. Mapping of variable names to real paths. Example:
  list(target_var = "D:/test_dir", base_dir = "/tmp")

## Value

Logical. TRUE if all clear, FALSE if missing files.

## Author

Mark London
