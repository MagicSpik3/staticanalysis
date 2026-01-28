#' Check for Dynamic Sourcing (Excel Injection Risk)
#'
#' Flags uses of source() where the file is a variable, not a string literal.
#' This usually indicates code is being executed from data or untrusted inputs.
#'
#' @noRd
check_dynamic_sourcing <- function(pdata, file) {
  # Find calls to source()
  calls <- pdata[pdata$token == "SYMBOL_FUNCTION_CALL" & pdata$text == "source", ]

  if (nrow(calls) == 0) return(NULL)

  smells <- list()

  for (i in seq_len(nrow(calls))) {
    call_id <- calls$parent[i]
    siblings <- pdata[pdata$parent == call_id, ]

    # Find the opening parenthesis
    open_paren <- siblings[siblings$token == "'('", ]
    if (nrow(open_paren) == 0) next

    # The first argument is the first token after '('
    args <- siblings[siblings$id > open_paren$id[1], ]
    if (nrow(args) == 0) next

    first_arg <- args[which.min(args$id), ]

    # THE CHECK: Is the first argument a String Constant?
    # Safe: source("file.R") -> STR_CONST
    # Unsafe: source(x)      -> SYMBOL

    if (first_arg$token != "STR_CONST") {
      smells[[length(smells) + 1]] <- data.frame(
        file = file, line = calls$line1[i],
        id = "DYNAMIC_SOURCING",
        severity = "CRITICAL",
        category = "SECURITY",
        message = "source() called on a variable. This permits arbitrary code execution from input data (Excel Injection).",
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(smells) == 0) return(NULL)
  return(do.call(rbind, smells))
}
