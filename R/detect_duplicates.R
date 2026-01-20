#' Detect Code Logic Duplicates (Clones)
#'
#' Scans the project for functions that are structurally identical.
#' Uses 'terminal' token analysis to distinguish operators like + vs *.
#'
#' @param dir_path String. Path to the project root.
#' @param ignore_constants Logical. If TRUE, treats 'x * 12' and 'x * 52' as identical.
#' @return A tibble grouping functions by their logic signature.
#' @export
detect_duplicates <- function(dir_path, ignore_constants = TRUE) {
  # 1. Use the new Scanner
  # PROFESSOR FIX: Sort the files to ensure deterministic order
  files <- sort(list_r_files(dir_path, "source"))
  defs <- scan_definitions(files, root_dir = dir_path)

  if (is.null(defs)) {
    return(NULL)
  }

  funcs <- defs[defs$type == "function", ]
  if (nrow(funcs) == 0) {
    return(NULL)
  }

  # Sort the inventory to ensure deterministic processing order
  funcs <- funcs[order(funcs$name), ]

  results <- list()

  # 2. Analyze each function
  for (i in seq_len(nrow(funcs))) {
    rec <- funcs[i, ]
    f_path <- file.path(dir_path, rec$file)

    # Extract Body
    exprs <- tryCatch(parse(f_path, keep.source = TRUE), error = function(e) NULL)
    fn_body <- find_func_in_exprs(exprs, rec$name)

    if (!is.null(fn_body)) {
      # 3. Generate Fingerprint
      clean_code <- deparse(fn_body)
      # FIX: Explicitly force source retention so getParseData works in Batch Mode
      pd <- utils::getParseData(parse(text = clean_code, keep.source = TRUE))

      # CRITICAL LOGIC FIX: Only use TERMINAL tokens (Leaves of the AST)
      # This ensures we capture the exact operators (+, *) and not just the structure
      pd <- pd[pd$terminal == TRUE, ]

      # Filter noise
      pd <- pd[pd$token != "COMMENT", ]

      # Anonymize Variables
      pd$text[pd$token %in% c("SYMBOL", "SYMBOL_FORMALS")] <- "VAR"

      # Anonymize Constants (Optional)
      if (ignore_constants) {
        pd$text[pd$token %in% c("NUM_CONST", "STR_CONST")] <- "CONST"
      }

      # Create Signature
      sig <- paste(pd$text, collapse = " ")

      results[[length(results) + 1]] <- data.frame(
        name = rec$name,
        file = rec$file,
        signature = sig,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(results) == 0) {
    return(NULL)
  }
  res_df <- do.call(rbind, results)

  # 4. Find Duplicates
  dupes <- res_df |>
    dplyr::group_by(.data$signature) |>
    dplyr::mutate(group_id = dplyr::cur_group_id(), group_size = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$group_size > 1) |>
    dplyr::arrange(.data$group_id, .data$name) |>
    dplyr::select("group_id", "name", "file", "signature")

  return(dupes)
}

#' Helper to extract function body
#' @noRd
find_func_in_exprs <- function(exprs, target_name) {
  for (e in exprs) {
    # CRITICAL FIX: Ensure e[[1]] is a SYMBOL before checking its name
    # This prevents crashing on namespaced calls like 'pkg::func()'
    if (is.call(e) && is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=")) {
      # Safe name extraction
      if (is.symbol(e[[2]]) && as.character(e[[2]]) == target_name) {
        return(e[[3]])
      }
    }
  }
  return(NULL)
}
