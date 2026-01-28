#' Audit Roxygen Documentation Mismatches
#'
#' Checks if the function name in the `@examples` block matches the actual function.
#'
#' @param dir_path String. Project root.
#' @return Dataframe of mismatches.
#' @author Mark London
#' @export
audit_documentation <- function(dir_path = ".") {
  inv <- audit_inventory(dir_path)
  mismatches <- list()

  funcs <- inv[inv$type == "function", ]

  for (i in seq_len(nrow(funcs))) {
    f_path <- file.path(dir_path, funcs$file[i])
    fn_name <- funcs$name[i]

    lines <- readLines(f_path, warn = FALSE)

    # Simple state machine to find @examples block
    in_example <- FALSE
    example_code <- ""

    for (line in lines) {
      if (grepl("@examples", line)) {
        in_example <- TRUE
        next
      }
      # Stop at next tag or NULL
      if (in_example && grepl("^#'\\s*@", line)) {
        break
      }

      if (in_example) {
        example_code <- paste(example_code, line)
      }
    }

    # Analysis: Does the example code call the function?
    if (in_example && example_code != "") {
      # Check if function name appears in the example
      if (!grepl(fn_name, example_code, fixed = TRUE)) {
        mismatches[[length(mismatches) + 1]] <- data.frame(
          function_name = fn_name,
          file = funcs$file[i],
          issue = "Example block does not use the function name"
        )
      }
    }
  }

  if (length(mismatches) == 0) {
    return(NULL)
  }
  return(do.call(rbind, mismatches))
}
