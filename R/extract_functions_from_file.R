#' Helper: Extract Function ASTs from a file
#' @noRd
extract_functions_from_file <- function(fpath) {
  # Parse safely; returns a list of expressions
  exprs <- tryCatch(
    parse(fpath, keep.source = FALSE),
    error = function(e) NULL
  )

  funcs <- list()

  for (e in exprs) {
    # Look for assignment: name <- function(...) or name = function(...)
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=")) {
      lhs <- e[[2]] # The name
      rhs <- e[[3]] # The value

      # Check if the value is a function definition
      if (is.call(rhs) && as.character(rhs[[1]]) == "function") {
        # Convert lhs symbol to string
        fn_name <- as.character(lhs)
        funcs[[fn_name]] <- rhs
      }
    }
  }
  return(funcs)
}
