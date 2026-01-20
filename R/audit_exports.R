#' Audit Function Exports
#'
#' Checks if functions are exported via Roxygen tags and if they appear in the NAMESPACE.
#'
#' @param inventory Dataframe. The output of audit_inventory().
#' @param dir_path String. Project root.
#' @return The inventory dataframe with new columns: 'has_export_tag', 'in_namespace', 'status'.
#' @export
audit_exports <- function(inventory, dir_path = ".") {

  # 1. Parse NAMESPACE file
  ns_path <- file.path(dir_path, "NAMESPACE")
  ns_exports <- character(0)

  if (fs::file_exists(ns_path)) {
    # Simple regex parse of export(func1, func2)
    ns_text <- readLines(ns_path, warn = FALSE)
    # Extract content inside export(...)
    matches <- regmatches(ns_text, regexec("export\\(([^)]+)\\)", ns_text))

    # Process matches
    for (m in matches) {
      if (length(m) > 1) {
        # Split by comma and clean whitespace
        funcs <- strsplit(m[[2]], ",")[[1]]
        ns_exports <- c(ns_exports, trimws(funcs))
      }
    }
  }

  # 2. Check Source Code for Roxygen Tags
  # We iterate only functions
  inventory$has_export_tag <- FALSE
  inventory$detached_tag   <- FALSE
  inventory$in_namespace   <- inventory$name %in% ns_exports

  funcs <- inventory[inventory$type == "function", ]

  if (nrow(funcs) == 0) return(inventory)

  for (i in seq_len(nrow(funcs))) {

    row_idx <- which(inventory$name == funcs$name[i] & inventory$file == funcs$file[i])
    f_path <- file.path(dir_path, funcs$file[i])

    if (!fs::file_exists(f_path)) next

    # Use our robust parser helper
    pdata <- utils::getParseData(parse(f_path, keep.source = TRUE))
    loc <- find_func_lines(pdata, funcs$name[i])

    if (!is.null(loc)) {
      # Read lines to check comments above the start line
      lines <- readLines(f_path, warn = FALSE)
      start_line <- loc$start

      # Scan backwards from the function definition
      curr <- start_line - 1
      found_tag <- FALSE
      found_gap <- FALSE

      while (curr > 0) {
        line <- lines[curr]

        # If it's a Roxygen comment
        if (grepl("^\\s*#'", line)) {
          if (grepl("@export", line)) {
            found_tag <- TRUE
          }
          curr <- curr - 1
        }
        # If it's a blank line, we hit a gap.
        # If we ALREADY found the tag, it's fine (tag is above the gap?).
        # Actually, roxygen must be CONTIGUOUS.
        # If we hit a blank line BEFORE finding the tag, the block is broken.
        else if (trimws(line) == "") {
          # If we see a blank line, stop scanning.
          # But check if we see an export tag further up?
          # A blank line breaks the documentation block.
          # So if we haven't found the tag yet, we check one more line up to see if it's a "Detached" error.
          if (!found_tag) {
            # Peek one line up
            if (curr > 1 && grepl("@export", lines[curr - 1])) {
              found_gap <- TRUE
              found_tag <- TRUE # It exists, but it's detached
            }
          }
          break
        }
        # If it's code or standard comment
        else {
          break
        }
      }

      inventory$has_export_tag[row_idx] <- found_tag
      inventory$detached_tag[row_idx]   <- found_gap
    }
  }

  # 3. Determine Status
  inventory$export_status <- ifelse(
    inventory$detached_tag, "DETACHED_TAG",
    ifelse(
      inventory$has_export_tag & !inventory$in_namespace, "MISSING_IN_NS",
      ifelse(
        !inventory$has_export_tag & inventory$in_namespace, "EXTRA_IN_NS",
        ifelse(inventory$has_export_tag, "EXPORTED", "INTERNAL")
      )
    )
  )

  return(inventory)
}
