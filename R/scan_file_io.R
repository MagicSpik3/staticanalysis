#' Scan File I/O Requirements (Refactored)
#'
#' Scans the codebase for file reading operations using pure functional patterns.
#'
#' @param dir_path String. Path to project root.
#' @return A dataframe of detected file operations.
#' @author Mark London
#' @export
scan_file_io <- function(dir_path = ".") {
  files <- list_r_files(dir_path, "source")

  read_funcs <- c(
    "read.csv", "read.table", "readRDS", "load", "source",
    "read_csv", "read_excel", "fread", "scan",
    "read_sav", "read_sas" # <--- ADD THESE
  )

  # 1. Define the recursive scanner (Returns a list of rows)
  find_io_in_expr <- function(expr, current_file) {
    found_ops <- list()

    if (is.call(expr)) {
      fn_name <- "unknown"

      # Identify Function Name
      if (is.symbol(expr[[1]])) {
        fn_name <- as.character(expr[[1]])
      } else if (is.call(expr[[1]]) && as.character(expr[[1]][[1]]) %in% c("::", ":::")) {
        fn_name <- as.character(expr[[1]][[3]])
      }

      # Check if it's a Read Function
      if (fn_name %in% read_funcs && length(expr) >= 2) {
        file_arg <- expr[[2]]

        # Build the row
        new_row <- data.frame(
          file = current_file, line = NA, func = fn_name,
          type = "unknown", path_pattern = NA, root_var = NA,
          stringsAsFactors = FALSE
        )

        if (is.character(file_arg)) {
          new_row$type <- "literal"
          new_row$path_pattern <- file_arg
          found_ops[[length(found_ops) + 1]] <- new_row
        } else if (is.call(file_arg) && as.character(file_arg[[1]]) == "file.path") {
          if (length(file_arg) >= 3 && is.symbol(file_arg[[2]]) && is.character(file_arg[[3]])) {
            new_row$type <- "constructed"
            new_row$path_pattern <- as.character(file_arg[[3]])
            new_row$root_var <- as.character(file_arg[[2]])
            found_ops[[length(found_ops) + 1]] <- new_row
          }
        } else if (is.symbol(file_arg)) {
          new_row$type <- "variable"
          new_row$root_var <- as.character(file_arg)
          found_ops[[length(found_ops) + 1]] <- new_row
        }
      }

      # Recursion: Collect results from children
      for (i in 2:length(expr)) {
        found_ops <- c(found_ops, find_io_in_expr(expr[[i]], current_file))
      }
    }
    return(found_ops)
  }

  # 2. Process Files using lapply (No global state!)
  all_ops <- list()

  for (f in files) {
    exprs <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
    if (!is.null(exprs)) {
      for (e in exprs) {
        all_ops <- c(all_ops, find_io_in_expr(e, f))
      }
    }
  }

  if (length(all_ops) == 0) {
    return(NULL)
  }
  return(do.call(rbind, all_ops))
}
