# Scan Project I/O (Unified)

The authoritative scanner for all File I/O operations. Extracts literal
paths, constructed paths (file.path), and config variables (paths\$x).

## Usage

``` r
scan_project_io(dir_path = ".")
```

## Arguments

- dir_path:

  String. Project root.

## Value

Dataframe with columns: file, line, func, type (READ/WRITE), arg_text
(raw code), arg_type (literal/symbol/call).
