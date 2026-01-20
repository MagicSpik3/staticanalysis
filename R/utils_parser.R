#' Helper: Find function definition lines in parse data
#' Uses strict geometry (Line/Col) to identify LHS vs RHS, ignoring AST ID ordering.
#' @noRd
find_func_lines <- function(pdata, func_name) {
  # 1. Find all Assignment Operators
  assign_ops <- pdata[pdata$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN"), ]

  if (nrow(assign_ops) == 0) {
    return(NULL)
  }

  for (i in seq_len(nrow(assign_ops))) {
    op_row <- assign_ops[i, ]
    parent_id <- op_row$parent

    # 2. Get all siblings (The components of this assignment expression)
    siblings <- pdata[pdata$parent == parent_id, ]

    # 3. Geometric Split: LHS vs RHS
    # LHS: Ends strictly before the operator starts
    # We compare using (Line, Col) tuples

    # Logic: A sibling is LHS if it ends before the operator starts
    is_lhs <- (siblings$line2 < op_row$line1) |
      (siblings$line2 == op_row$line1 & siblings$col2 < op_row$col1)

    # RHS: Starts strictly after the operator ends
    is_rhs <- (siblings$line1 > op_row$line2) |
      (siblings$line1 == op_row$line2 & siblings$col1 > op_row$col2)

    lhs_tokens <- siblings[is_lhs, ]
    rhs_tokens <- siblings[is_rhs, ]

    if (nrow(lhs_tokens) == 0 || nrow(rhs_tokens) == 0) next

    # 4. Check LHS for the Symbol
    # Find the bounds of the LHS block
    lhs_start_line <- min(lhs_tokens$line1)
    lhs_end_line <- max(lhs_tokens$line2)

    # Scan GLOBAL pdata for the symbol within LHS bounds
    found_lhs <- any(
      pdata$token == "SYMBOL" &
        pdata$text == func_name &
        pdata$line1 >= lhs_start_line &
        pdata$line2 <= lhs_end_line
    )

    if (!found_lhs) next

    # 5. Check RHS for the Function Keyword
    # Find the bounds of the RHS block
    rhs_start_line <- min(rhs_tokens$line1)
    rhs_end_line <- max(rhs_tokens$line2)

    # Scan GLOBAL pdata for 'FUNCTION' within RHS bounds
    found_rhs <- any(
      pdata$token == "FUNCTION" &
        pdata$line1 >= rhs_start_line &
        pdata$line2 <= rhs_end_line
    )

    if (found_rhs) {
      # FOUND IT! Return the full range of the parent block
      # (This includes the LHS, the Operator, and the RHS)
      block <- pdata[pdata$id == parent_id | pdata$parent == parent_id, ]
      return(list(start = min(block$line1), end = max(block$line2)))
    }
  }
  return(NULL)
}
