# Scaffold Migration

Extracts a function from a legacy script and places it in its own file.

## Usage

``` r
scaffold_migration(func_name, source_file, target_dir = ".")
```

## Arguments

- func_name:

  String. The name of the function to migrate.

- source_file:

  String. Path to the legacy R script.

- target_dir:

  String. Root of the package.

## Value

Logical TRUE if successful.
