#' Run Recipe
#'
#' Executes a compiled 'rule_recipe' object in a safe environment.
#'
#' @author Mark London
#' @param recipe A 'rule_recipe' object created by compile_rules().
#' @return A list containing the calculated results.
#' @export
run_recipe <- function(recipe) {
  if (!inherits(recipe, "rule_recipe")) stop("Input must be a compiled recipe.")

  # CHANGE HERE: Use baseenv() instead of emptyenv() so we get *, +, -, etc.
  env_state <- new.env(parent = baseenv())
  results_list <- list()

  for (step in recipe) {
    tryCatch(
      {
        # Execute the pre-compiled expression inside the sandbox
        val <- eval(step$expression, envir = env_state)

        # Update State
        assign(step$target, val, envir = env_state)
        results_list[[step$target]] <- val
      },
      error = function(e) {
        stop(sprintf(
          "❌ RUNTIME ERROR in Step %d (%s): %s",
          step$step_id, step$target, e$message
        ))
      }
    )
  }

  return(results_list)
}
