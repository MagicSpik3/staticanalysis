#' Scan I/O Logic (Reads, Writes, and Path Definitions)
#'
#' Parses code to find data ingestion, data output, and path construction.
#'
#' @param dir_path String. Path to project root.
#' @return A dataframe of I/O events.
#' @author Mark London
#' @export
scan_io_logic <- function(dir_path = ".") {
  files <- list_r_files(dir_path, "source")

  # 1. Configuration: Signatures
  # Format: Function Name = Argument Index for the File Path
  read_sigs <- c(
    "read.csv" = 1, "read.table" = 1, "readRDS" = 1, "load" = 1,
    "read_sav" = 1, "read_sas" = 1, "read_excel" = 1, "read_xlsx" = 1, "fread" = 1,
    "read_spss" = 1, "openxlsx::read.xlsx" = 1, "haven::read_sav" = 1
  )

  write_sigs <- c(
    "write.csv" = 2, "write.table" = 2, "saveRDS" = 2,
    "write_sav" = 2, "write_xlsx" = 2, "fwrite" = 2, "ggsave" = 1,
    "pdf" = 1, "png" = 1, "sink" = 1, "openxlsx::write.xlsx" = 2,
    "haven::write_sav" = 2
  )

  results <- list()

  # 2. Recursive AST Walker
  walk_expr <- function(expr, file, line_offset = 0) {
    if (is.call(expr)) {
      fn_name <- "unknown"

      # Extract function name (handle pkg::fn)
      if (is.symbol(expr[[1]])) {
        fn_name <- as.character(expr[[1]])
      } else if (is.call(expr[[1]]) && as.character(expr[[1]][[1]]) %in% c("::", ":::")) {
        fn_name <- paste0(as.character(expr[[1]][[2]]), "::", as.character(expr[[1]][[3]]))
      }

      # Clean namespace for matching (haven::read_sav -> read_sav)
      clean_fn <- gsub("^.*::", "", fn_name)

      # --- CHECK 1: Is it a READ? ---
      if (clean_fn %in% names(read_sigs)) {
        arg_idx <- read_sigs[[clean_fn]]
        # Check if argument exists by position or name
        # (Simplified: Grab positional for now, improving this requires matching.call)
        if (length(expr) > arg_idx) {
          arg_expr <- expr[[arg_idx + 1]]
          results[[length(results) + 1]] <<- data.frame(
            file = file, type = "READ", func = fn_name,
            expression = deparse1(arg_expr),
            stringsAsFactors = FALSE
          )
        }
      }

      # --- CHECK 2: Is it a WRITE? ---
      else if (clean_fn %in% names(write_sigs)) {
        arg_idx <- write_sigs[[clean_fn]]
        if (length(expr) > arg_idx) {
          arg_expr <- expr[[arg_idx + 1]]
          results[[length(results) + 1]] <<- data.frame(
            file = file, type = "WRITE", func = fn_name,
            expression = deparse1(arg_expr),
            stringsAsFactors = FALSE
          )
        }
      }

      # --- CHECK 3: Is it a PATH DEFINITION? ---
      # Looking for: variable <- file.path(...)
      else if (clean_fn %in% c("<-", "=")) {
        lhs <- expr[[2]]
        rhs <- expr[[3]]

        # Check if RHS is file.path() or paste() involving paths
        if (is.call(rhs)) {
          rhs_fn <- as.character(rhs[[1]])
          # Handle namespace
          rhs_fn <- gsub("^.*::", "", rhs_fn)

          if (rhs_fn %in% c("file.path")) {
            results[[length(results) + 1]] <<- data.frame(
              file = file, type = "PATH_DEF", func = "file.path",
              expression = deparse1(lhs), # The variable being defined
              stringsAsFactors = FALSE
            )
          }
        }
      }

      # Recurse
      for (child in as.list(expr)) walk_expr(child, file)
    }
  }

  # 3. Execution
  for (f in files) {
    exprs <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
    if (!is.null(exprs)) {
      for (e in exprs) walk_expr(e, f)
    }
  }

  if (length(results) == 0) return(NULL)
  return(do.call(rbind, results))
}

#' Helper to safely deparse
#' @noRd
deparse1 <- function(x) {
  paste(deparse(x), collapse = "")
}



# ... Inside the WRITE check ...
else if (clean_fn %in% names(write_sigs)) {
  arg_idx <- write_sigs[[clean_fn]]
  if (length(expr) > arg_idx) {
    arg_expr <- expr[[arg_idx + 1]]

    # Analyze the Path Argument
    path_type <- "LITERAL"
    if (is.symbol(arg_expr)) path_type <- "VARIABLE"
    if (is.call(arg_expr)) {
      # If it calls paste, sprintf, or file.path, it is DYNAMIC
      callee <- as.character(arg_expr[[1]])
      if (callee %in% c("paste", "paste0", "sprintf", "file.path", "glue")) {
        path_type <- "DYNAMIC/DERIVED"
      }
    }

    results[[length(results) + 1]] <<- data.frame(
      file = file, type = "WRITE", func = fn_name,
      path_mode = path_type, # NEW COLUMN
      expression = deparse1(arg_expr),
      stringsAsFactors = FALSE
    )
  }
}
