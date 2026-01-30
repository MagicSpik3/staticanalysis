#' Scan Project I/O (Unified)
#'
#' The authoritative scanner for all File I/O operations.
#' Extracts literal paths, constructed paths (file.path), and config variables (paths$x).
#'
#' @param dir_path String. Project root.
#' @return Dataframe with columns: file, line, func, type (READ/WRITE),
#'         arg_text (raw code), arg_type (literal/symbol/call).
#' @export
scan_project_io <- function(dir_path = ".") {
  files <- list_r_files(dir_path, "source")

  # Definition: Function -> Role mapping
  io_map <- list(
    # READS
    "read.csv" = "READ", "read.table" = "READ", "readRDS" = "READ",
    "load" = "READ", "read_csv" = "READ", "read_excel" = "READ",
    "read_feather" = "READ", "read_sav" = "READ", "read_sas" = "READ",
    "source" = "READ",
    # WRITES
    "write.csv" = "WRITE", "write.table" = "WRITE", "saveRDS" = "WRITE",
    "save" = "WRITE", "write_csv" = "WRITE", "write_xlsx" = "WRITE",
    "ggsave" = "WRITE", "pdf" = "WRITE", "png" = "WRITE",
    "openxlsx::write.xlsx" = "WRITE", "write.xlsx" = "WRITE"
  )

  results <- list()

  find_io <- function(expr, current_file) {
    ops <- list()
    if (is.call(expr)) {
      # 1. Resolve Function Name
      fn_name <- "unknown"
      if (is.symbol(expr[[1]])) {
        fn_name <- as.character(expr[[1]])
      } else if (is.call(expr[[1]]) && as.character(expr[[1]][[1]]) %in% c("::", ":::")) {
        fn_name <- paste0(as.character(expr[[1]][[2]]), "::", as.character(expr[[1]][[3]]))
      }

      # Clean namespace for lookup (e.g. haven::read_sav -> read_sav)
      simple_name <- sub("^.*::", "", fn_name)

      # 2. Check match
      if (simple_name %in% names(io_map)) {
        role <- io_map[[simple_name]]

        # 3. Extract Target Argument
        # Logic: Writes usually take (obj, file), Reads take (file)
        idx <- 2 # Default: 1st argument
        if (role == "WRITE" && !simple_name %in% c("pdf", "png", "ggsave")) {
          if (length(expr) >= 3) idx <- 3 # 2nd argument
        }
        # Override if named arg 'file' exists
        if ("file" %in% names(expr)) target_arg <- expr[["file"]]
        else if (length(expr) >= idx) target_arg <- expr[[idx]]
        else target_arg <- NULL

        if (!is.null(target_arg)) {
          # 4. Analyze Argument Type
          arg_type <- "complex"
          if (is.character(target_arg)) arg_type <- "literal"
          else if (is.symbol(target_arg)) arg_type <- "symbol"
          else if (is.call(target_arg)) arg_type <- "call"

          # Capture RAW text for Regex matching (paths$x)
          # paste(deparse) ensures it's a single string
          raw_text <- paste(deparse(target_arg), collapse = "")

          # Capture CLEAN value for literals
          lit_val <- NA
          if (arg_type == "literal") lit_val <- as.character(target_arg)

          ops[[length(ops) + 1]] <- data.frame(
            file = current_file,
            line = NA, # Can add pdata logic later
            func = simple_name,
            type = role,
            arg_text = raw_text,
            arg_type = arg_type,
            literal_value = lit_val,
            stringsAsFactors = FALSE
          )
        }
      }

      # Recursion
      for (i in 2:length(expr)) {
        ops <- c(ops, find_io(expr[[i]], current_file))
      }
    }
    return(ops)
  }

  for (f in files) {
    exprs <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
    if (!is.null(exprs)) {
      for (e in exprs) {
        # Note: We do NOT recurse blindly on the result list to avoid stack issues,
        # just append the discovered ops.
        results <- c(results, find_io(e, f))
      }
    }
  }

  if (length(results) == 0) return(NULL)
  return(do.call(rbind, results))
}
