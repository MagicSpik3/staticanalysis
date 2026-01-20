#' Audit Functions Used in Rules
#'
#' Scans a directory of rule CSVs, parses the R code in them, and counts
#' which functions are actually being called.
#'
#' @param dir_path String. Path to directory containing CSV rules.
#' @return A frequency table of function calls (e.g., "mutate": 500, "if_else": 20).
#' @export
audit_formula_functions <- function(dir_path) {
  # 1. Read all CSVs
  files <- fs::dir_ls(dir_path, glob = "*.csv", recurse = TRUE)
  all_funcs <- character()

  for (f in files) {
    df <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df) || !"Rule" %in% names(df)) next

    # 2. Parse every rule
    for (rule in df$Rule) {
      # Skip empty rules or comments
      if (trimws(rule) == "" || grepl("^#", rule)) next

      # Parse AST
      expr <- tryCatch(parse(text = rule), error = function(e) NULL)
      if (is.null(expr)) next

      # Walk AST to find function calls
      funcs <- extract_calls(expr)
      all_funcs <- c(all_funcs, funcs)
    }
  }

  # 3. Tally
  if (length(all_funcs) == 0) {
    return(NULL)
  }

  counts <- as.data.frame(table(all_funcs), stringsAsFactors = FALSE)
  colnames(counts) <- c("function_name", "count")
  return(counts[order(-counts$count), ])
}

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
