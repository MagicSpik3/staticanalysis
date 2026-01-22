# Visualize Migration Progress

Renders a network graph of the project functions. Red nodes =
Misplaced/Legacy functions (Technical Debt). Green nodes =
Refactored/Clean functions. Arrows = Dependency calls.

## Usage

``` r
visualize_progress(inventory, return_dot = FALSE)
```

## Arguments

- inventory:

  The dataframe from audit_inventory()

- return_dot:

  Logical. If TRUE, returns the DOT code string instead of rendering the
  graph.
