#' Extract Function DNA
#' @description Parses R files and returns a cleaned inventory of function bodies.
get_function_inventory <- function(target_dir) {
  files <- list.files(target_dir, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)

  funs <- lapply(files, function(f) {
    exprs <- try(parse(f, keep.source = TRUE), silent = TRUE)
    if (inherits(exprs, "try-error")) return(NULL)

    # Extract assignments where the RHS is a function
    results <- list()
    for (e in exprs) {
      if (is.call(e) && (identical(e[[1]], as.symbol("<-")) || identical(e[[1]], as.symbol("=")))) {
        if (is.symbol(e[[2]]) && is.call(e[[3]]) && identical(e[[3]][[1]], as.symbol("function"))) {
          # Deparsing removes comments/formatting, leaving only logic
          clean_body <- paste(deparse(e[[3]], width.cutoff = 500L), collapse = "\n")
          results[[as.character(e[[2]])]] <- list(
            file = basename(f),
            body = clean_body,
            size = nchar(clean_body)
          )
        }
      }
    }
    return(results)
  })

  # Flatten and convert to data.frame
  funs <- unlist(funs, recursive = FALSE)
  df <- data.frame(
    name = names(funs),
    file = vapply(funs, `[[`, "", "file"),
    body = vapply(funs, `[[`, "", "body"),
    size = vapply(funs, `[[`, 0L, "size"),
    stringsAsFactors = FALSE
  )
  return(df)
}
