# Run Pre-Flight File Check

Validates a specific list of paths against the Usage Contract. Checks
that INPUTS exist and OUTPUT parents exist.

## Usage

``` r
run_preflight_check(paths_list, contract)
```

## Arguments

- paths_list:

  List. The actual list of paths (e.g. from data_paths()).

- contract:

  Dataframe. Output from infer_path_contract().

## Value

Logical TRUE if passed, FALSE if failed.

## Author

Mark London
