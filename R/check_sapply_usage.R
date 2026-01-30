#' Check for unsafe sapply usage
#'
#' Detects `sapply()`, which is not type-safe (it might return a list or a vector).
#' Recommends `vapply()` for strict typing.
#'
#' @noRd
check_sapply_usage <- function(pdata, file) {
  # Find all function calls
  calls <- pdata[pdata$token == "SYMBOL_FUNCTION_CALL", ]
  if (nrow(calls) == 0) return(NULL)

  # Filter for 'sapply'
  sapply_calls <- calls[calls$text == "sapply", ]

  if (nrow(sapply_calls) == 0) return(NULL)

  smells <- list()

  for (i in seq_len(nrow(sapply_calls))) {
    smells[[length(smells) + 1]] <- data.frame(
      file = file,
      line = sapply_calls$line1[i],
      id = "SAPPLY_USAGE", # Matches your test expectation
      severity = "MEDIUM",
      category = "EFFICIENCY",
      message = "sapply() is unstable (return type varies). Use vapply() for type safety.",
      stringsAsFactors = FALSE
    )
  }

  return(do.call(rbind, smells))
}
