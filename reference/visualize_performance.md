# Visualize Performance Heatmap

Generates a call graph where nodes are colored by their execution time.
Red = Slow, Green = Fast, Grey = Unused.

## Usage

``` r
visualize_performance(target_func, dynamic_data, dir_path = ".")
```

## Arguments

- target_func:

  String. The entry point function to trace (e.g. "nth_prime_bad").

- dynamic_data:

  Dataframe. The output from rdyntrace::trace_results().

- dir_path:

  String. Project root.

## Value

A DiagrammeR graph object.

## Author

Mark London
