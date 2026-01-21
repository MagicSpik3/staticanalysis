# Detect Potential Typos

Analyzes a variable census to find "near neighbors" — variables that
look similar but are likely typos (e.g., "rate" vs "ratw").

## Usage

``` r
detect_typos(census, max_distance = 1)
```

## Arguments

- census:

  A tibble returned by harvest_variables().

- max_distance:

  Integer. Maximum number of character edits. Default is 1.

## Value

A tibble of suspicious pairs.

## Author

Mark London
