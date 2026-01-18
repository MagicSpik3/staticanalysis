#' Inspect Recipe for Missing Inputs
#'
#' Static analysis of the recipe to determine which variables must be provided
#' by the user (Inputs) and which are calculated internally (Outputs).
#'
#' @author Mark London
#' @param recipe A 'rule_recipe' object created by compile_rules().
#' @return A list containing 'inputs_needed' and 'outputs_created'.
#' @export
inspect_recipe <- function(recipe) {
  if (!inherits(recipe, "rule_recipe")) stop("Input must be a compiled recipe.")

  known_vars <- character() # What we have defined so far
  missing_inputs <- character() # What we needed but didn't have
  outputs <- character() # What this recipe produces

  for (step in recipe) {
    # 1. Ask the AST: "What variables are inside this logic?"
    # all.vars() is the native C-level function that finds symbols
    # It ignores numbers (100) and function names (+, mean, if)
    vars_needed <- all.vars(step$expression)

    # 2. Check if we have them
    # We look for variables that are NOT in our 'known' list
    new_needs <- setdiff(vars_needed, known_vars)

    # Accumulate missing inputs
    missing_inputs <- unique(c(missing_inputs, new_needs))

    # 3. Register the Output
    # After this step runs, this variable exists for future steps
    target <- step$target
    known_vars <- unique(c(known_vars, target))
    outputs <- unique(c(outputs, target))
  }

  return(list(
    inputs_needed = sort(missing_inputs),
    outputs_created = sort(outputs)
  ))
}
