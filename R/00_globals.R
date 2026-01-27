#' Internal Global State
#'
#' Stores package-level globals, primarily the AST cache.
#'
#' @author Mark London
#' @noRd
.staticanalysis_globals <- new.env(parent = emptyenv())

# Initialize the cache inside the globals
.staticanalysis_globals$ast_cache <- new.env(parent = emptyenv())
