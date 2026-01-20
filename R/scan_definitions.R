#' Scan Definitions
#'
#' Parses R files to find all defined functions and global variables.
#'
#' @param files Character vector of file paths to scan.
#' @param root_dir String. The root directory (used to calculate relative paths).
#' @return A tibble of definitions (name, type, file).
#' @export
scan_definitions <- function(files, root_dir) {
  inventory <- list()
  files <- sort(files)

  for (f in files) {
    # Parse the code into an AST
    exprs <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
    if (is.null(exprs)) next

    for (e in exprs) {
      # FIX: Ensure e is a call AND e[[1]] is a symbol before checking if it's assignment
      # This prevents crashing on namespaced calls like 'pkg::func()' where e[[1]] is not a simple symbol
      if (is.call(e) && is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=")) {
        lhs <- e[[2]]
        rhs <- e[[3]]

        # Determine Name
        obj_name <- as.character(lhs)

        # Determine Type
        obj_type <- "variable"

        # Check if RHS is a function definition
        if (is.call(rhs)) {
          head_sym <- rhs[[1]]
          if (is.symbol(head_sym) && as.character(head_sym) == "function") {
            obj_type <- "function"
          }
        }

        inventory[[length(inventory) + 1]] <- data.frame(
          name = obj_name,
          type = obj_type,
          file = as.character(fs::path_rel(f, start = root_dir)),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(inventory) == 0) {
    return(NULL)
  }
  return(do.call(rbind, inventory))
}
