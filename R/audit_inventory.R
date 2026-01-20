#' Audit Project Inventory
#'
#' Scans a project to index all defined functions and global variables.
#' Checks if these objects are referenced in the 'tests' directory.
#' Handles both .R and .r extensions.
#'
#' @param dir_path String. Path to the package root (containing R/ and tests/).
#' @return A tibble listing objects, their locations, and test coverage status.
#' @export
audit_inventory <- function(dir_path) {

  if (!fs::dir_exists(dir_path)) stop("Directory not found.")

  # FIX 1: Use Regex to capture both .R and .r (Linux is case-sensitive)
  r_files <- fs::dir_ls(dir_path, recurse = TRUE, regexp = "\\.[rR]$")

  # Filter out tests/tinytest/spec/vignettes from the definition scan
  # We only want to find WHERE things are defined, not where they are tested
  source_files <- r_files[!grepl("/tinytest/|/tests/|/spec/|/vignettes/", r_files)]

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
        # FIX 2: Strict check to prevent "length > 1" errors
        # We ensure rhs[[1]] is a generic SYMBOL before converting to char
        obj_type <- "variable"

        is_func_def <- FALSE
        if (is.call(rhs)) {
          # Check if the call head is the symbol 'function'
          head_sym <- rhs[[1]]
          if (is.symbol(head_sym) && as.character(head_sym) == "function") {
            is_func_def <- TRUE
          }
        }

        if (is_func_def) {
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
  # FIX 1 Redux: Scan all test files regardless of case
  all_files <- fs::dir_ls(dir_path, recurse = TRUE, regexp = "\\.[rR]$")
  test_files <- all_files[grepl("/tinytest/|/tests/|/spec/|/inst/tinytest/", all_files)]

  if (length(test_files) > 0) {
    # Read all test code as one giant blob of text for fast grepping
    all_test_code <- unlist(lapply(test_files, readLines, warn = FALSE))

    inv_df$called_in_test <- vapply(inv_df$name, function(nm) {
      # Look for the name followed by boundary or '('
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
