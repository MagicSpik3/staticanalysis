# Visualize Downstream Calls (Forward Dependency)

Generates a flow chart starting from a top-level function to see
everything it calls, and what they call in turn.

## Usage

``` r
visualize_flow(target_func, dir_path = ".", save_dot = NULL)
```

## Arguments

- target_func:

  String. The entry point function.

- dir_path:

  String. Project root.

- save_dot:

  String. Optional path to save the DOT file.

## Value

A DiagrammeR graph object.
