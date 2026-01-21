# Compile Rules

Parses a configuration file into a 'Recipe' state machine. Performs
validation (typos, security) but DOES NOT execute.

## Usage

``` r
compile_rules(file_path, allowed_vars = NULL)
```

## Arguments

- file_path:

  String. Path to the CSV/Excel file.

- allowed_vars:

  Character Vector. The whitelist of allowed outputs. If NULL (default),
  strict checking is disabled (Discovery Mode).

## Author

Mark London
