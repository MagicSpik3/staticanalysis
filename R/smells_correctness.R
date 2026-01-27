#' Check for Environment Pollution (attach/detach)
#' @noRd
check_environment_pollution <- function(pdata, file) {
  # Look for calls to attach() or detach()
  # Pattern: SYMBOL_FUNCTION_CALL matching names
  targets <- c("attach", "detach")
  calls <- pdata[pdata$token == "SYMBOL_FUNCTION_CALL" & pdata$text %in% targets, ]

  if (nrow(calls) > 0) {
    return(data.frame(
      file = file, line = calls$line1,
      id = "ENV_POLLUTION",
      severity = "CRITICAL",
      category = "CORRECTNESS",
      message = paste("Avoid", calls$text, "- it breaks static analysis and creates hidden state."),
      stringsAsFactors = FALSE
    ))
  }
  return(NULL)
}

#' Check for Reproducibility Killers (setwd)
#' @noRd
check_reproducibility <- function(pdata, file) {
  calls <- pdata[pdata$token == "SYMBOL_FUNCTION_CALL" & pdata$text == "setwd", ]

  if (nrow(calls) > 0) {
    return(data.frame(
      file = file, line = calls$line1,
      id = "SETWD_USAGE",
      severity = "CRITICAL",
      category = "REPRODUCIBILITY",
      message = "Never use setwd() in scripts/packages. Use relative paths or the 'here' package.",
      stringsAsFactors = FALSE
    ))
  }
  return(NULL)
}

#' Check for Dynamic Execution (eval, parse, assign)
#' @noRd
check_dynamic_execution <- function(pdata, file) {
  targets <- c("eval", "parse", "assign", "get")
  calls <- pdata[pdata$token == "SYMBOL_FUNCTION_CALL" & pdata$text %in% targets, ]

  if (nrow(calls) > 0) {
    return(data.frame(
      file = file, line = calls$line1,
      id = "DYNAMIC_EXECUTION",
      severity = "HIGH",
      category = "MAINTAINABILITY",
      message = paste("Avoid", calls$text, "- it hides dependencies and makes code hard to debug."),
      stringsAsFactors = FALSE
    ))
  }
  return(NULL)
}

#' Check for Overwriting Base Functions
#' @noRd
check_base_overwrite <- function(funcs) {
  # List of protected names
  protected <- c("mean", "sum", "min", "max", "c", "length", "list", "return", "data")

  bad_funcs <- funcs[funcs$name %in% protected, ]

  if (nrow(bad_funcs) > 0) {
    smells <- list()
    for (i in seq_len(nrow(bad_funcs))) {
      smells[[i]] <- data.frame(
        file = bad_funcs$file[i], line = 1, # Inventory doesn't track line, usually top of file
        id = "BASE_OVERWRITE",
        severity = "CRITICAL",
        category = "CORRECTNESS",
        message = paste("Function overwrites base R function:", bad_funcs$name[i]),
        stringsAsFactors = FALSE
      )
    }
    return(do.call(rbind, smells))
  }
  return(NULL)
}
