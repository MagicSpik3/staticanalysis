#' Helper to extract function body
#' @noRd
find_func_in_exprs <- function(exprs, target_name) {
  for (e in exprs) {
    # CRITICAL FIX: Ensure e[[1]] is a SYMBOL before checking its name
    # This prevents crashing on namespaced calls like 'pkg::func()'
    if (is.call(e) && is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=")) {
      # Safe name extraction
      if (is.symbol(e[[2]]) && as.character(e[[2]]) == target_name) {
        return(e[[3]])
      }
    }
  }
  return(NULL)
}
