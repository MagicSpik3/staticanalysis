#' Helper: Recursive walker to find function names
#' @noRd
extract_calls <- function(expr) {
  calls <- character()

  if (is.call(expr)) {
    # Check for pkg::func or just func
    fn <- expr[[1]]

    if (is.symbol(fn)) {
      # Standard call: mutate(...)
      calls <- c(calls, as.character(fn))
    } else if (is.call(fn) && as.character(fn[[1]]) %in% c("::", ":::")) {
      # Namespaced call: tidytable::mutate(...)
      # fn[[2]] is pkg, fn[[3]] is func
      pkg <- as.character(fn[[2]])
      func <- as.character(fn[[3]])
      calls <- c(calls, paste0(pkg, "::", func))
    }

    # Recurse arguments
    for (i in 2:length(expr)) {
      calls <- c(calls, extract_calls(expr[[i]]))
    }
  } else if (is.recursive(expr)) {
    for (i in seq_along(expr)) {
      calls <- c(calls, extract_calls(expr[[i]]))
    }
  }

  return(calls)
}
