#' Compile Rules
#'
#' Parses a configuration file into a 'Recipe' state machine.
#' Performs validation (typos, security) but DOES NOT execute.
#'
#' @author Mark London
#' @param file_path String. Path to the CSV/Excel file.
#' @param allowed_vars Character Vector. The whitelist of allowed outputs.
#' @return A 'recipe' data frame containing parsed expressions and dependencies.
#' @export
compile_rules <- function(file_path, allowed_vars = c("r", "tax", "net")) {

  if (!file.exists(file_path)) stop("File not found: ", file_path)
  rules <- utils::read.csv(file_path, stringsAsFactors = FALSE)

  # 1. VALIDATION
  invalid_vars <- setdiff(rules$Target, allowed_vars)
  if (length(invalid_vars) > 0) {
    stop(sprintf("❌ COMPILER ERROR: Variable '%s' is not allowed.", invalid_vars[1]))
  }

  # 2. COMPILATION (Building the Recipe)
  # We store the *Expression* (code to be run), not the result.
  recipe <- list()

  for (i in seq_len(nrow(rules))) {
    target <- rules$Target[i]
    logic  <- rules$Rule[i]

    # Security Check
    if (grepl("system|rm|list.files", logic)) {
      stop("❌ SECURITY ALERT: Forbidden function detected.")
    }

    # Parse to AST (catches syntax errors immediately)
    expr <- tryCatch(parse(text = logic), error = function(e) {
      stop(sprintf("❌ SYNTAX ERROR in '%s': %s", target, e$message))
    })

    # Store as a structured step
    recipe[[i]] <- list(
      step_id = i,
      target = target,
      expression = expr,
      raw_text = logic
    )
  }

  # Return the recipe (State Machine Definition)
  class(recipe) <- c("rule_recipe", "list")
  return(recipe)
}
