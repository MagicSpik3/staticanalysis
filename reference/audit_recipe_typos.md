# Audit Recipe for Typos

Scans a compiled recipe for suspicious variable names using fuzzy
matching. This is the "Safety Net" when using compile_rules(allowed_vars
= NULL).

## Usage

``` r
audit_recipe_typos(recipe)
```

## Arguments

- recipe:

  A compiled rule_recipe object.

## Value

A tibble of suspicious pairs.
