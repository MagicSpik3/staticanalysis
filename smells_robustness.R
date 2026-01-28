#' Check for Ambiguous Tidy Selection
#' Flags usages of external vectors in select/across without all_of()
#' @noRd
check_ambiguous_selection <- function(pdata, file) {
  # Targets: across() and select()
  targets <- c("across", "select")
  calls <- pdata[pdata$token == "SYMBOL_FUNCTION_CALL" & pdata$text %in% targets, ]

  if (nrow(calls) == 0) return(NULL)

  smells <- list()

  for (i in seq_len(nrow(calls))) {
    call_id <- calls$parent[i]

    # Get siblings (the arguments inside the call)
    siblings <- pdata[pdata$parent == call_id, ]

    # We want the first argument.
    # Structure: [FUNC] [ ( ] [ARG1] ...
    # Find the opening parenthesis
    open_paren <- siblings[siblings$token == "'('", ]
    if (nrow(open_paren) == 0) next

    # The first token of Arg1 is the one with the lowest ID *after* the paren
    paren_id <- open_paren$id[1]
    args_tokens <- siblings[siblings$id > paren_id, ]

    if (nrow(args_tokens) == 0) next

    # Get the very first token of the first argument
    first_arg_token <- args_tokens[which.min(args_tokens$id), ]

    # THE CHECK:
    # If it is a raw SYMBOL (variable name), it is ambiguous.
    # If it were 'all_of(...)', the token would be SYMBOL_FUNCTION_CALL.
    # If it were 'c(...)', the token would be SYMBOL_FUNCTION_CALL.
    # If it were 'starts_with(...)', the token would be SYMBOL_FUNCTION_CALL.

    if (first_arg_token$token == "SYMBOL") {
      # Exclusion: 'everything' is technically a function but sometimes parses as SYMBOL depending on context/version
      # but usually SYMBOL_FUNCTION_CALL if followed by ().
      # Let's flag it.

      smells[[length(smells) + 1]] <- data.frame(
        file = file, line = first_arg_token$line1,
        id = "AMBIGUOUS_SELECTION",
        severity = "MEDIUM",
        category = "ROBUSTNESS",
        message = paste0("Ambiguous selection '", first_arg_token$text, "'. Wrap in all_of() if it's a vector, or ensure it's a column."),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(smells) == 0) return(NULL)
  return(do.call(rbind, smells))
}

#' Check for Unsafe T/F Usage
#' @noRd
check_unsafe_boolean <- function(pdata, file) {
  t_f_symbols <- pdata[pdata$token == "SYMBOL" & pdata$text %in% c("T", "F"), ]
  if (nrow(t_f_symbols) > 0) {
    return(data.frame(
      file = file, line = t_f_symbols$line1,
      id = "UNSAFE_BOOLEAN",
      severity = "LOW",
      category = "ROBUSTNESS",
      message = "Use TRUE/FALSE instead of T/F.",
      stringsAsFactors = FALSE
    ))
  }
  return(NULL)
}

#' Check for Sapply Usage
#' @noRd
check_sapply_usage <- function(pdata, file) {
  calls <- pdata[pdata$token == "SYMBOL_FUNCTION_CALL" & pdata$text == "sapply", ]
  if (nrow(calls) > 0) {
    return(data.frame(
      file = file, line = calls$line1,
      id = "SAPPLY_USAGE",
      severity = "MEDIUM",
      category = "ROBUSTNESS",
      message = "sapply() is not type-safe. Use vapply() for robust code.",
      stringsAsFactors = FALSE
    ))
  }
  return(NULL)
}

#' Check for Unsafe Sequencing (1:length)
#' @noRd
check_unsafe_sequencing <- function(pdata, file) {
  colons <- pdata[pdata$token == "':'", ]
  if (nrow(colons) == 0) return(NULL)

  smells <- list()
  src_lines <- readLines(file, warn = FALSE)

  for (i in seq_len(nrow(colons))) {
    row <- colons[i, ]
    if (length(src_lines) >= row$line1) {
      code_line <- src_lines[row$line1]
      if (grepl("1\\s*:\\s*(length|nrow|ncol)\\(", code_line)) {
        smells[[length(smells) + 1]] <- data.frame(
          file = file, line = row$line1,
          id = "UNSAFE_SEQUENCE",
          severity = "MEDIUM",
          category = "ROBUSTNESS",
          message = "Use seq_along() or seq_len(). 1:N fails on empty inputs.",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(smells) == 0) return(NULL)
  return(do.call(rbind, smells))
}
