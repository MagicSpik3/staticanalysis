# Audit Function Exports

Checks if functions are exported via Roxygen tags and if they appear in
the NAMESPACE.

## Usage

``` r
audit_exports(inventory, dir_path = ".")
```

## Arguments

- inventory:

  Dataframe. The output of audit_inventory().

- dir_path:

  String. Project root.

## Value

The inventory dataframe with new columns: 'has_export_tag',
'in_namespace', 'status'.
