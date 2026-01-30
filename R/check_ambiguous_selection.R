#' Check for Ambiguous Selection (Drop Dimension)
#'
#' Detects usage of df[i, j] which can silently return a vector instead of a
#' dataframe. Enforces usage of drop = FALSE.
#'
#' @noRd
check_ambiguous_selection <- function(pdata, file) {
  # 1. Find the '[' operator
  brackets <- pdata[pdata$token == "'['", ]
  if (nrow(brackets) == 0) return(NULL)

  smells <- list()

  for (i in seq_len(nrow(brackets))) {
    # Define the scope of this subsetting call (lines)
    # We use line numbers to capture all tokens (including nested ones)
    start_line <- brackets$line1[i]
    end_line <- brackets$line2[i]

    # Get all terminal tokens (leaves) in this range
    # Filtering for terminal == TRUE removes the abstract 'expr' wrappers
    # allowing us to see the sequence: "drop" -> "=" -> "FALSE" clearly.
    tokens <- pdata[pdata$line1 >= start_line &
                      pdata$line2 <= end_line &
                      pdata$terminal == TRUE, ]

    # 1. Is it 2D? (Look for a comma)
    # Note: df[1] (1D) is usually safe (list selection).
    # df[1, ] (2D) is the danger zone.
    if (!any(tokens$token == "','")) next

    # 2. Check for SAFE drop (drop = FALSE)
    is_safe <- FALSE

    # Find position of 'drop' argument
    # It usually appears as SYMBOL_SUB (named arg) or SYMBOL
    drop_indices <- which(tokens$text == "drop" & tokens$token %in% c("SYMBOL_SUB", "SYMBOL"))

    for (idx in drop_indices) {
      # Safety Check: ensure we don't go out of bounds
      if ((idx + 2) <= nrow(tokens)) {
        # Check sequence: drop -> = -> FALSE
        # tokens[idx]   is "drop"
        # tokens[idx+1] should be "=" (EQ_ASSIGN)
        # tokens[idx+2] should be "FALSE" or "F" (LOGICAL/SYMBOL)

        next_token <- tokens[idx + 1, ]
        val_token  <- tokens[idx + 2, ]

        if (next_token$text == "=") {
          if (val_token$text %in% c("FALSE", "F")) {
            is_safe <- TRUE
          }
        }
      }
    }

    if (!is_safe) {
      smells[[length(smells) + 1]] <- data.frame(
        file = file,
        line = brackets$line1[i],
        id = "AMBIGUOUS_SELECTION",
        severity = "HIGH",
        category = "ROBUSTNESS",
        message = "Dataframe subsetting [i, j] drops dimensions by default. Use [i, j, drop = FALSE].",
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(smells) == 0) return(NULL)
  return(do.call(rbind, smells))
}
