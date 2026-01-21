# Inspect Recipe for Missing Inputs

Static analysis of the recipe to determine which variables must be
provided by the user (Inputs) and which are calculated internally
(Outputs).

## Usage

``` r
inspect_recipe(recipe)
```

## Arguments

- recipe:

  A 'rule_recipe' object created by compile_rules().

## Value

A list containing 'inputs_needed' and 'outputs_created'.

## Author

Mark London
