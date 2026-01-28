# Detect Code Smells

Scans the codebase for dangerous patterns and classifies them by
severity.

## Usage

``` r
detect_code_smells(dir_path = ".")
```

## Arguments

- dir_path:

  String. Path to the project root.

## Value

A dataframe with columns: file, line, id, severity, category, message.

## Author

Mark London
