#' Check for Unsafe Sequencing (The 1:length trap)
#'
#' Detects `1:length(x)` or `1:nrow(x)`. If x is empty, this generates `1:0`
#' (counts down), creating two iterations instead of zero.
#'
#' @noRd
check_unsafe_sequencing <- function(pdata, file) {
  # 1. Find the colon operator ':'
  colons <- pdata[pdata$token == "':'", ]
  if (nrow(colons) == 0) return(NULL)

  smells <- list()

  for (i in seq_len(nrow(colons))) {
    # Get the parent expression (e.g. 1:length(x))
    parent_id <- colons$parent[i]
    siblings <- pdata[pdata$parent == parent_id, ]

    # We need:
    # LHS = "1" (NUM_CONST)
    # RHS = function call to "length" or "nrow"

    # Check LHS
    # Note: siblings are ordered by ID usually. The colon is in the middle.
    lhs <- siblings[siblings$id < colons$id[i], ]
    rhs <- siblings[siblings$id > colons$id[i], ]

    if (nrow(lhs) == 0 || nrow(rhs) == 0) next

    # Is LHS "1"?
    is_one <- any(lhs$text == "1" & lhs$token == "NUM_CONST")

    # Is RHS a bad function call?
    # We look for SYMBOL_FUNCTION_CALL matching length/nrow
    bad_funcs <- c("length", "nrow", "NROW")
    has_bad_call <- any(rhs$text %in% bad_funcs & rhs$token == "SYMBOL_FUNCTION_CALL")

    if (is_one && has_bad_call) {
      smells[[length(smells) + 1]] <- data.frame(
        file = file,
        line = colons$line1[i],
        id = "UNSAFE_SEQUENCE",
        severity = "HIGH",
        category = "ROBUSTNESS",
        message = "Unsafe sequence 1:length(). Use seq_len() or seq_along() to handle empty inputs.",
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(smells) == 0) return(NULL)
  return(do.call(rbind, smells))
}
