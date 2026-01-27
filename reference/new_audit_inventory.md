# Constructor: Audit Inventory Object

Validates and creates a standardized audit inventory object. Enforces
the schema to prevent silent column errors (The "Global Coupling" fix).

## Usage

``` r
new_audit_inventory(df)
```

## Arguments

- df:

  Dataframe containing the raw inventory scan.

## Value

An object of class 'audit_inventory'.

## Author

Mark London
