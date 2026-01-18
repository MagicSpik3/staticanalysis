#' Compile Rules
#'
#' Parses a configuration file into a 'Recipe' state machine.
#' Performs validation (typos, security) but DOES NOT execute.
#'
#' @author Mark London
#'
#' @param file_path String. Path to the CSV/Excel file.
#' @param allowed_vars Character Vector. The whitelist of allowed outputs.
#'                     If NULL (default), strict checking is disabled (Discovery Mode).
#' @export
compile_rules <- function(file_path, allowed_vars = NULL) {
  if (!file.exists(file_path)) stop("File not found: ", file_path)
  rules <- utils::read.csv(file_path, stringsAsFactors = FALSE)

  # CHANGE 1: Only validate if a whitelist is actually provided
  if (!is.null(allowed_vars)) {
    invalid_vars <- setdiff(rules$Target, allowed_vars)
    if (length(invalid_vars) > 0) {
      stop(sprintf("❌ COMPILER ERROR: Variable '%s' is not allowed.", invalid_vars[1]))
    }
  }

  recipe <- list()

  for (i in seq_len(nrow(rules))) {
    target <- rules$Target[i]
    logic <- rules$Rule[i]

    # Security Check (Always Keep This!)
    if (grepl("system|rm|list.files", logic)) {
      stop("❌ SECURITY ALERT: Forbidden function detected.")
    }

    expr <- tryCatch(parse(text = logic), error = function(e) {
      stop(sprintf("❌ SYNTAX ERROR in '%s': %s", target, e$message))
    })

    recipe[[i]] <- list(
      step_id = i,
      target = target,
      expression = expr,
      raw_text = logic
    )
  }

  class(recipe) <- c("rule_recipe", "list")
  return(recipe)
}
