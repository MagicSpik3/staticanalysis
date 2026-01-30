#' Check for Growing Vectors (The Memory Hog)
#'
#' Detects objects growing inside loops (e.g. x <- c(x, new)).
#' This causes quadratic memory copying and is a major performance killer.
#'
#' @noRd
check_growing_vectors <- function(pdata, file) {
  # 1. Identify Loops (FOR or WHILE keywords)
  loop_tokens <- pdata[pdata$token %in% c("FOR", "WHILE"), ]
  if (nrow(loop_tokens) == 0) return(NULL)

  smells <- list()

  for (i in seq_len(nrow(loop_tokens))) {

    # --- CRITICAL FIX START ---
    # The 'FOR' token is just the word "for".
    # Its PARENT is the expression that contains the entire loop (condition + body).
    loop_parent_id <- loop_tokens$parent[i]

    # Get the scope of the PARENT expression
    loop_scope_row <- pdata[pdata$id == loop_parent_id, ]

    # Handle edge case where parent isn't found (shouldn't happen in valid parse data)
    if (nrow(loop_scope_row) == 0) next

    start_line <- loop_scope_row$line1
    end_line   <- loop_scope_row$line2
    # --- CRITICAL FIX END ---

    # Get all tokens strictly inside this loop structure
    loop_body <- pdata[pdata$line1 >= start_line & pdata$line2 <= end_line, ]

    # 2. Find Assignments: x <- ...
    assigns <- loop_body[loop_body$token == "LEFT_ASSIGN", ]

    if (nrow(assigns) > 0) {
      for (j in seq_len(nrow(assigns))) {

        assign_id <- assigns$id[j]

        # Identify LHS Variable
        # Look for SYMBOL tokens that appear before the assignment in the AST
        # (Filtering by ID < assign_id handles the sequence order)
        lhs_tokens <- loop_body[loop_body$id < assign_id & loop_body$token == "SYMBOL", ]

        if (nrow(lhs_tokens) == 0) next

        # Pick the closest symbol to the assignment (The immediate LHS)
        lhs_sym <- lhs_tokens[which.max(lhs_tokens$id), ]
        var_name <- lhs_sym$text

        # Identify RHS Usage
        # Look for tokens on the same line (or logically after assignment)
        # We check the entire loop body that follows this assignment ID to capture multi-line statements
        # But for robustness, let's look at tokens covering the same line(s) as the assignment expression.
        # Simplified: Look at the same line1 as the assignment.
        rhs_tokens <- loop_body[loop_body$line1 == assigns$line1[j] & loop_body$id > assign_id, ]

        # Check for 'c' or 'append'
        # Note: In some parsers 'c' is SYMBOL_FUNCTION_CALL, in others just SYMBOL. Check both.
        func_calls <- rhs_tokens$text[rhs_tokens$token %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL")]
        uses_c <- any(c("c", "append", "cbind", "rbind") %in% func_calls)

        # Check for Recursion (The variable appearing on RHS)
        vars_on_rhs <- rhs_tokens$text[rhs_tokens$token == "SYMBOL"]
        uses_var <- var_name %in% vars_on_rhs

        if (uses_c && uses_var) {
          smells[[length(smells) + 1]] <- data.frame(
            file = file, line = assigns$line1[j],
            id = "GROWING_VECTOR",
            severity = "CRITICAL",
            category = "PERFORMANCE",
            message = paste0("Growing vector '", var_name, "' inside loop. Pre-allocate memory!"),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (length(smells) == 0) return(NULL)
  return(do.call(rbind, smells))
}
