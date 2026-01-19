#' Audit Project Inventory
#'
#' Scans a project to index all defined functions and global variables.
#' Checks if these objects are referenced in the 'tests' directory.
#'
#' @param dir_path String. Path to the package root (containing R/ and tests/).
#' @return A tibble listing objects, their locations, and test coverage status.
#' @export
audit_inventory <- function(dir_path) {

  if (!fs::dir_exists(dir_path)) stop("Directory not found.")

  # 1. INDEX DEFINITIONS (The "What exists?" phase)
  r_files <- fs::dir_ls(dir_path, recurse = TRUE, glob = "*.R")

  # Filter out tests from the definition scan (we don't care about helper funcs inside tests)
  # We assume standard structure where source is in R/ or package root
  source_files <- r_files[!grepl("/tests/|/spec/|/vignettes/", r_files)]

  inventory <- list()

  for (f in source_files) {
    # Parse the code into an AST
    exprs <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
    if (is.null(exprs)) next

    for (e in exprs) {
      # Look for assignments: name <- value
      if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=")) {
        lhs <- e[[2]]
        rhs <- e[[3]]

        # Determine Name
        obj_name <- as.character(lhs)

        # Determine Type
        obj_type <- "variable"
        if (is.call(rhs) && as.character(rhs[[1]]) == "function") {
          obj_type <- "function"
        }

        inventory[[length(inventory) + 1]] <- data.frame(
          name = obj_name,
          type = obj_type,
          file = as.character(fs::path_rel(f, start = dir_path)),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(inventory) == 0) return(NULL)
  inv_df <- do.call(rbind, inventory)

  # 2. CHECK COVERAGE (The "Is it used?" phase)
  test_files <- fs::dir_ls(dir_path, recurse = TRUE, glob = "*.R")
  test_files <- test_files[grepl("/tests/|/spec/", test_files)]

  # Read all test code as one giant blob of text for fast grepping
  # (Regex is safer/faster here than AST because tests often use non-standard eval)
  if (length(test_files) > 0) {
    all_test_code <- unlist(lapply(test_files, readLines, warn = FALSE))

    inv_df$called_in_test <- vapply(inv_df$name, function(nm) {
      # Look for the name followed by boundary or '('
      # We escape special characters in the function name just in case
      pattern <- paste0("\\b", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", nm), "\\b")
      any(grepl(pattern, all_test_code))
    }, logical(1))
  } else {
    inv_df$called_in_test <- FALSE
  }

  # Sort for readability
  inv_df <- inv_df[order(inv_df$type, inv_df$name), ]

  return(inv_df)
}
