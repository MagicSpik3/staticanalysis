#' Helper: Recursive AST Walker
#' @noRd
walk_ast_for_pkgs <- function(expr) {
  pkgs <- character()

  # SAFETY: Handle lists (blocks of code)
  if (is.list(expr) || is.expression(expr)) {
    # Convert to list to safely iterate without evaluating missing symbols
    safe_list <- as.list(expr)
    for (e in safe_list) {
      if (missing(e)) next # Skip empty holes
      pkgs <- c(pkgs, walk_ast_for_pkgs(e))
    }
    return(pkgs)
  }

  # SAFETY: Handle function calls
  if (is.call(expr)) {
    # Check for package operators (pkg::fun)
    if (is.symbol(expr[[1]]) && as.character(expr[[1]]) %in% c("::", ":::")) {
      pkgs <- c(pkgs, as.character(expr[[2]]))
    }

    # Check for library(pkg)
    if (is.symbol(expr[[1]]) && as.character(expr[[1]]) %in% c("library", "require", "p_load")) {
      if (length(expr) >= 2) {
        arg <- expr[[2]]
        # Only grab if it's a literal symbol or string
        if (is.character(arg) || is.symbol(arg)) {
          pkgs <- c(pkgs, as.character(arg))
        }
      }
    }

    # Recurse children safely
    safe_children <- as.list(expr)
    for (child in safe_children) {
      # The "Banana Rule": If argument is missing (e.g. func(a,,b)), SKIP IT.
      # We check against the empty symbol.
      if (missing(child)) next

      pkgs <- c(pkgs, walk_ast_for_pkgs(child))
    }
  }

  return(pkgs)
}
