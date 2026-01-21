# Relocate Functions to New Repositories

Reads the project inventory and copies specific functions into a new
directory structure. Useful for splitting a Monolith into
Micro-services.

## Usage

``` r
relocate_functions(inventory, function_map, source_root, dry_run = TRUE)
```

## Arguments

- inventory:

  The dataframe returned by audit_inventory().

- function_map:

  A named list where keys are destination paths and values are character

- source_root:

  String. The root directory to read source files from. \<– ADD THIS

- dry_run:

  Logical. If TRUE, just logs actions.
