#' Check for Growing Vectors (The Memory Hog)
#'
#' Detects objects growing inside loops (e.g. x <- c(x, new)).
#' This causes quadratic memory copying and is a major performance killer.
#'
#' @noRd
check_growing_vectors <- function(pdata, file) {
  # 1. Identify Loops
  loops <- pdata[pdata$token %in% c("FOR", "WHILE"), ]
  if (nrow(loops) == 0) return(NULL)

  smells <- list()

  for (i in seq_len(nrow(loops))) {
    loop_id <- loops$parent[i]

    # Get all tokens inside this loop body
    # (Naive: all tokens with parent == loop_id or descendants)
    # Since our AST is flat, we use line numbers or recursive ID mapping.
    # A robust way in flat parse data:
    # The loop body is usually the last child of the loop expression.

    # Let's verify assignment logic inside the specific lines of the loop
    # (Simplified for the demo: just look at lines covered by the loop)
    start_line <- loops$line1[i]
    end_line <- loops$line2[i]

    loop_body <- pdata[pdata$line1 >= start_line & pdata$line2 <= end_line, ]

    # 2. Find Assignments: x <- ...
    assigns <- loop_body[loop_body$token == "LEFT_ASSIGN", ]

    if (nrow(assigns) > 0) {
      for (j in seq_len(nrow(assigns))) {
        # LHS is the symbol before assignment
        assign_id <- assigns$id[j]
        lhs_tokens <- loop_body[loop_body$id < assign_id, ]
        if (nrow(lhs_tokens) == 0) next

        lhs_sym <- lhs_tokens[which.max(lhs_tokens$id), ]
        if (lhs_sym$token != "SYMBOL") next
        var_name <- lhs_sym$text

        # RHS is everything after
        # Check if RHS contains: c(var_name, ...) or c(..., var_name)
        # OR append(var_name, ...)

        # We look for SYMBOL_FUNCTION_CALL "c" or "append"
        # AND usage of 'var_name' inside it

        # Get RHS tokens for this line/expression
        rhs_tokens <- loop_body[loop_body$line1 == assigns$line1[j] & loop_body$id > assign_id, ]

        uses_c <- "c" %in% rhs_tokens$text[rhs_tokens$token == "SYMBOL_FUNCTION_CALL"]
        uses_var <- var_name %in% rhs_tokens$text[rhs_tokens$token == "SYMBOL"]

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
