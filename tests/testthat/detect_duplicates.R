#' Detect Code Logic Duplicates (Clones)
#'
#' Scans the project for functions that are structurally identical or similar,
#' ignoring variable names, spacing, and comments.
#'
#' @param dir_path String. Path to the project root.
#' @param ignore_constants Logical. If TRUE, treats 'x * 12' and 'x * 52' as identical logic.
#' @return A tibble grouping functions by their logic signature.
#' @export
detect_duplicates <- function(dir_path, ignore_constants = TRUE) {

  # 1. Find all R files
  files <- fs::dir_ls(dir_path, recurse = TRUE, glob = "*.R")

  # Store fingerprints
  fingerprints <- list()

  for (f in files) {
    # Parse the file to get token data
    pdata <- tryCatch(
      utils::getParseData(parse(f, keep.source = TRUE)),
      error = function(e) NULL
    )
    if (is.null(pdata)) next

    # Identify function boundaries
    # We look for "SYMBOL <- FUNCTION" or "SYMBOL = FUNCTION" patterns
    # This is a heuristic; robust AST walking is heavier but this is fast.
    assign_rows <- which(pdata$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN"))

    for (i in assign_rows) {
      # Check if RHS is 'FUNCTION'
      # The token immediately after assignment (skipping comments/whitespace)
      next_idx <- min(which(pdata$id > pdata$id[i] & !pdata$token %in% c("COMMENT", "STR_CONST"))) # rough scan

      # Safer: Look at the parent structure in parse data (id/parent_id)
      # Let's simplify: Extract the source text of the function and re-parse it alone.

      # Get the variable name (LHS of assignment)
      # It's usually the meaningful token before the assignment
      lhs_idx <- max(which(pdata$id < pdata$id[i] & pdata$token == "SYMBOL"))
      func_name <- pdata$text[lhs_idx]

      # Heuristic: Grab the function body block
      # We assume the assignment structure is standard.
      # Let's use a simpler approach: 'extract_functions_from_file' helper we wrote earlier?
      # Yes, let's reuse that concept but return Source Text this time.
    }
  }

  # REVISED STRATEGY: Reuse our robust AST extractor, then fingerprint the AST.
  all_funcs <- audit_inventory(dir_path) # We use your existing tool!
  if (is.null(all_funcs)) return(NULL)

  all_funcs <- all_funcs[all_funcs$type == "function", ]

  results <- list()

  for (row_idx in seq_len(nrow(all_funcs))) {
    rec <- all_funcs[row_idx, ]
    full_path <- file.path(dir_path, rec$file)

    # Parse and extract specific function body
    exprs <- parse(full_path, keep.source = TRUE)
    fn_body <- find_func_in_exprs(exprs, rec$name)

    if (!is.null(fn_body)) {
      # GENERATE FINGERPRINT
      # 1. Get tokens
      pd <- utils::getParseData(parse(text = deparse(fn_body)))

      # 2. Filter noise
      pd <- pd[!pd$token %in% c("COMMENT", "whitespace", "expr", "line"), ]

      # 3. Anonymize
      # Replace variable names with GENERIC_VAR
      pd$text[pd$token %in% c("SYMBOL", "SYMBOL_FORMALS")] <- "VAR"

      # Optional: Ignore constants (treat * 12 same as * 100)
      if (ignore_constants) {
        pd$text[pd$token %in% c("NUM_CONST", "STR_CONST")] <- "CONST"
      }

      # 4. Collapse to signature string
      sig <- paste(pd$text, collapse = " ")

      results[[length(results) + 1]] <- data.frame(
        name = rec$name,
        file = rec$file,
        signature = sig,
        stringsAsFactors = FALSE
      )
    }
  }

  res_df <- do.call(rbind, results)

  # Group by signature to find duplicates
  dupes <- res_df |>
    dplyr::group_by(.data$signature) |>
    dplyr::mutate(group_id = dplyr::cur_group_id(), group_size = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$group_size > 1) |>
    dplyr::arrange(.data$group_id, .data$name) |>
    dplyr::select("group_id", "name", "file", "signature")

  return(dupes)
}

#' Helper to extract the function object from expression list
#' @noRd
find_func_in_exprs <- function(exprs, target_name) {
  for (e in exprs) {
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=")) {
      if (as.character(e[[2]]) == target_name) {
        return(e[[3]]) # The function body
      }
    }
  }
  return(NULL)
}
