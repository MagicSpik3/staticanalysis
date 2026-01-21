# List R Files by Category

Scans a directory for R scripts and separates them into 'source'
(package code) or 'test' (unit tests, tinytests, specs).

## Usage

``` r
list_r_files(dir_path, type = c("source", "test"))
```

## Arguments

- dir_path:

  String. Path to the project root.

- type:

  String. Either "source" (default) or "test".

## Value

A character vector of file paths.
