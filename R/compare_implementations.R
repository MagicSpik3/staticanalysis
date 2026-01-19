#' Compare Implementations
#'
#' Verifies that functions defined in a single legacy file (Monolith) are identical
#' in logic to those split into a directory of files (Refactored).
#' Uses AST comparison to ignore comments and whitespace differences.
#'
#' @param monolith_path String. Path to the legacy monolithic R script.
#' @param refactored_dir String. Path to the directory containing refactored R files.
#' @return A list of comparison results for each function found.
#' @export
compare_implementations <- function(monolith_path, refactored_dir) {
  if (!file.exists(monolith_path)) stop("Monolith file not found.")
  if (!fs::dir_exists(refactored_dir)) stop("Refactored directory not found.")

  # 1. Extract functions from the Monolith
  # We parse the code but do NOT evaluate it (Safety First)
  mono_funcs <- extract_functions_from_file(monolith_path)

  # 2. Extract functions from the Refactored Directory
  ref_files <- fs::dir_ls(refactored_dir, glob = "*.R")
  ref_funcs <- list()
  for (f in ref_files) {
    ref_funcs <- c(ref_funcs, extract_functions_from_file(f))
  }

  # 3. Compare Intersection
  common_names <- intersect(names(mono_funcs), names(ref_funcs))
  results <- list()

  for (fn_name in common_names) {
    f_old <- mono_funcs[[fn_name]]
    f_new <- ref_funcs[[fn_name]]

    # We compare the DEPARSED structure.
    # This standardizes formatting, ensuring logic is identical.
    # We use all.equal on the language objects.
    is_match <- isTRUE(all.equal(f_old, f_new))

    diff_output <- NULL
    if (!is_match) {
      # Use diffobj to generate a visual diff of the logic
      diff_output <- diffobj::diffPrint(f_old, f_new)
    }

    results[[fn_name]] <- list(
      match = is_match,
      diff  = diff_output
    )
  }

  return(results)
}

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
