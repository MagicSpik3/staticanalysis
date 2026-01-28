#' Get AST for File (Memoised)
#'
#' Retrieves the parse data for a file. If the file has already been parsed
#' in this session, returns the cached version. Otherwise, parses it,
#' caches it, and returns it.
#'
#' @param f_path String. Path to the file.
#' @return The parse data (dataframe) with preserved source.
#' @author Mark London
#' @noRd
get_file_ast <- function(f_path) {
  # 1. Check Cache
  # We look inside our internal global environment
  if (exists(f_path, envir = .staticanalysis_globals$ast_cache)) {
    return(get(f_path, envir = .staticanalysis_globals$ast_cache))
  }

  # 2. Parse (Expensive Operation)
  exprs <- tryCatch(
    parse(f_path, keep.source = TRUE),
    error = function(e) NULL
  )

  if (is.null(exprs)) {
    return(NULL)
  }

  # 3. Compute Geometry
  pdata <- utils::getParseData(exprs)

  # 4. Store in Cache
  assign(f_path, pdata, envir = .staticanalysis_globals$ast_cache)

  return(pdata)
}
