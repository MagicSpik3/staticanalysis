# Visualize Incoming Calls (Reverse Dependency)

Generates a graph showing every function that calls the target function.
Can render immediately in RStudio or export a raw .dot file.

## Usage

``` r
visualize_callers(target_func, dir_path = ".", save_dot = NULL)
```

## Arguments

- target_func:

  String. Name of the function to trace.

- dir_path:

  String. Project root.

- save_dot:

  String. Optional path to save the raw DOT file (e.g. "graph.dot").

## Value

A DiagrammeR graph object (invisibly if save_dot is used).

## Author

Mark London
