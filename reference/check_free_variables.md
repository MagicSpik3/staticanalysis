# Check for Free Variables (Implicit Dependencies)

Finds variables used in a function that are neither Arguments nor Local
Variables. These are "Free Variables" that rely on Global State (or
packages).

## Usage

``` r
check_free_variables(pdata, file)
```

## Arguments

- pdata:

  The AST dataframe.

- file:

  The filename.
