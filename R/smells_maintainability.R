#' Check for Library Calls inside Functions
#' @noRd
check_library_injection <- function(funcs, dir_path) {
  smells <- list()

  for (i in seq_len(nrow(funcs))) {
    f_path <- file.path(dir_path, funcs$file[i])
    pdata <- get_file_ast(f_path)
    loc <- find_func_lines(pdata, funcs$name[i])

    if (!is.null(loc)) {
      # Scan ONLY inside function body
      body <- pdata[pdata$line1 >= loc$start & pdata$line2 <= loc$end, ]
      libs <- body[body$token == "SYMBOL_FUNCTION_CALL" & body$text %in% c("library", "require"), ]

      if (nrow(libs) > 0) {
        smells[[length(smells) + 1]] <- data.frame(
          file = funcs$file[i], line = libs$line1[1],
          id = "LIBRARY_INJECTION",
          severity = "HIGH",
          category = "ARCHITECTURE",
          message = paste("Function", funcs$name[i], "calls library(). Packages should be loaded at top-level or via DESCRIPTION."),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(smells) == 0) return(NULL)
  return(do.call(rbind, smells))
}

#' Check for Sapply Usage (Suggest vapply)
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
