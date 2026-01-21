# Migrate Namespace Usage

Automatically refactors code to replace one package dependency with
another. Useful for migrating from 'tidytable' to 'dplyr' or vice versa.

## Usage

``` r
migrate_namespace(
  dir_path,
  from_pkg = "tidytable",
  to_pkg = "dplyr",
  dry_run = TRUE
)
```

## Arguments

- dir_path:

  String. Path to the project root.

- from_pkg:

  String. The package to remove (e.g., "tidytable").

- to_pkg:

  String. The package to replace it with (e.g., "dplyr").

- dry_run:

  Logical. If TRUE, only shows what would change without writing files.
