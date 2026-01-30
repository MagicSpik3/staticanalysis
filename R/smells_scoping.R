#' Check for Free Variables (Implicit Dependencies)
#'
#' Finds variables used in a function that are neither Arguments nor Local Variables.
#' These are "Free Variables" that rely on Global State (or packages).
#'
#' @param pdata The AST dataframe.
#' @param file The filename.
#' @export
check_free_variables <- function(pdata, file) {

  # Helper to analyze one function body
  analyze_func <- function(func_id, func_name) {
    # 1. Get Arguments
    # Args are siblings of the function body in the AST structure, usually before the body.
    # Simplified approach: Look for SYMBOL_FORMALS in the definition
    args <- unique(pdata$text[pdata$parent == func_id & pdata$token == "SYMBOL_FORMALS"])

    # 2. Get Assigned Locals (LHS of <- or =)
    # Find all assignments inside this function's scope
    # (This requires a recursive scope walker, simplified here for 'flat' detection)

    # Get the Body ID
    # The body is usually the last child of the function definition
    kids <- pdata[pdata$parent == func_id, ]
    body_id <- max(kids$id)

    # Find all symbols used in the body
    body_tokens <- pdata[pdata$line1 >= min(kids$line1) & pdata$line2 <= max(kids$line2), ]

    # Locals: Symbols that appear on the LHS of <-
    assigns <- body_tokens[body_tokens$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN"), ]
    locals <- c()

    if (nrow(assigns) > 0) {
      for (i in seq_len(nrow(assigns))) {
        # The LHS is the token immediately before the assignment in terms of ID
        # (Naive check - robust parser would walk the tree)
        lhs_id <- assigns$id[i] - 1
        # Find token with this ID (or close to it)
        lhs_tok <- body_tokens[body_tokens$id < assigns$id[i], ]
        if (nrow(lhs_tok) > 0) {
          # Take the closest one
          cand <- lhs_tok[which.max(lhs_tok$id), ]
          if (cand$token == "SYMBOL") locals <- c(locals, cand$text)
        }
      }
    }

    # 3. Find Usages (Symbols that are NOT calls)
    usages <- body_tokens[body_tokens$token == "SYMBOL", ]

    # Filter out function calls (SYMBOL followed by '(')
    # (This is tricky in flat AST, usually SYMBOL_FUNCTION_CALL handles known functions,
    # but 'df' is just SYMBOL).

    smells <- list()

    for (i in seq_len(nrow(usages))) {
      sym <- usages$text[i]

      # Allow-list: Standard base variables or known packages
      if (sym %in% c("T", "F", "pi", "letters", "month.name")) next

      # The Core Logic:
      # If it's NOT an Arg, and NOT a Local, it's Free (Global).
      if (!sym %in% args && !sym %in% locals) {

        # Heuristic: Ignore if it looks like a function call (we assume functions are available)
        # Check next token
        next_tok <- pdata[pdata$id > usages$id[i], ]
        if (nrow(next_tok) > 0 && next_tok$token[1] == "'('") next

        smells[[length(smells) + 1]] <- data.frame(
          file = file, line = usages$line1[i],
          id = "IMPLICIT_DEPENDENCY",
          severity = "CRITICAL",
          category = "SCOPING",
          message = paste0("Function '", func_name, "' uses '", sym, "' but does not define it. (Implicit Global)"),
          stringsAsFactors = FALSE
        )
      }
    }
    return(do.call(rbind, smells))
  }

  # Orchestration: Find all function definitions in the file
  defs <- pdata[pdata$token == "FUNCTION", ]
  all_smells <- list()

  if (nrow(defs) > 0) {
    for (j in seq_len(nrow(defs))) {
      # Find the name of this function (LHS of assignment)
      # ... (omitted for brevity, requires parent lookup)
      # For now, we analyze the block anonymously or assume standard structure

      # We just analyze the scope of the FUNCTION token
      res <- analyze_func(defs$parent[j], "valid_func")
      if (!is.null(res)) all_smells <- c(all_smells, list(res))
    }
  }

  if (length(all_smells) == 0) return(NULL)
  return(do.call(rbind, all_smells))
}
