#' Validate Data Patch Log (The Gatekeeper)
#'
#' Scans a list of R commands (from Excel) and validates them against the
#' expected dataframe schema.
#'
#' @param patch_strings Character vector. The raw code strings from Excel.
#' @param schema_names Character vector. The allowed column names (names(df)).
#' @param allow_new_cols Logical. If TRUE, warns instead of fails on new columns.
#' @return A dataframe report of valid/invalid commands.
#' @author Mark London
#' @export
validate_patch_log <- function(patch_strings, schema_names, allow_new_cols = FALSE) {

  results <- list()

  # Helper to clean column names from AST
  extract_target <- function(expr) {
    # We expect: df$col <- val  OR  df[['col']] <- val
    # AST: <- ( LHS, RHS )
    if (!is.call(expr)) return(NULL)

    op <- as.character(expr[[1]])

    # Check for Assignment
    if (!op %in% c("<-", "=")) return(list(type = "SIDE_EFFECT", target = NA))

    lhs <- expr[[2]]

    # Drill down to find the column
    # Pattern 1: df$col
    if (is.call(lhs) && as.character(lhs[[1]]) == "$") {
      return(list(type = "COLUMN_MOD", target = as.character(lhs[[3]])))
    }

    # Pattern 2: df[['col']] or df[row, 'col']
    if (is.call(lhs) && as.character(lhs[[1]]) == "[") {
      # This is harder as it might be df[cond, "col"]
      # Heuristic: look for string constants in the LHS args
      args <- as.list(lhs)
      strs <- unlist(lapply(args, function(x) if(is.character(x)) x else NULL))
      if (length(strs) > 0) {
        return(list(type = "COLUMN_MOD", target = strs[1]))
      }
    }

    # Pattern 3: names(df)[...] <- "new_name" (The Rename Case)
    if (is.call(lhs) && as.character(lhs[[1]]) == "[") {
      sub_call <- lhs[[2]] # names(df)
      if (is.call(sub_call) && as.character(sub_call[[1]]) == "names") {
        return(list(type = "SCHEMA_RENAME", target = "names()"))
      }
    }

    return(list(type = "UNKNOWN", target = NA))
  }

  for (i in seq_along(patch_strings)) {
    code <- patch_strings[i]
    status <- "PASS"
    msg <- ""

    # 1. Parse
    expr <- tryCatch(parse(text = code), error = function(e) NULL)

    if (is.null(expr)) {
      status <- "FAIL"
      msg <- "Syntax Error: Could not parse code."
    } else {
      # 2. Analyze
      info <- extract_target(expr[[1]])

      if (info$type == "COLUMN_MOD") {
        # Check if column exists
        if (!info$target %in% schema_names) {
          if (allow_new_cols) {
            status <- "WARN"
            msg <- paste0("Creates NEW column: '", info$target, "'")
          } else {
            status <- "FAIL"
            msg <- paste0("Schema Violation: Column '", info$target, "' does not exist.")
          }
        }
      } else if (info$type == "SCHEMA_RENAME") {
        status <- "WARN"
        msg <- "Detected schema renaming. Please verify 'randcol' logic manually."
      } else if (info$type == "SIDE_EFFECT") {
        status <- "FAIL"
        msg <- "Security Risk: Code is not a direct column assignment."
      }
    }

    results[[length(results) + 1]] <- data.frame(
      row_id = i,
      code = substr(code, 1, 50), # Truncate for display
      status = status,
      message = msg,
      stringsAsFactors = FALSE
    )
  }

  return(do.call(rbind, results))
}
