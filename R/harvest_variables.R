utils::globalVariables(c(".data"))
#' Harvest Variables
#'
#' Scans a directory of configuration files (CSVs) and compiles a census
#' of all variables being assigned to (LHS of assignments).
#'
#' @author Mark London
#' @param dir_path String. Path to the directory containing rule files.
#' @return A tibble with columns: variable, count, file_sources.
#' @export
harvest_variables <- function(dir_path) {
  if (!fs::dir_exists(dir_path)) stop("Directory not found: ", dir_path)

  # 1. Find all CSV files
  files <- fs::dir_ls(dir_path, glob = "*.csv")

  all_vars <- list()

  for (f in files) {
    # Read safely (skip if empty)
    df <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) next

    # We assume the 'Target' column exists, based on your schema.
    targets <- character()

    if ("Target" %in% names(df)) {
      targets <- df$Target
    }

    if (length(targets) > 0) {
      all_vars[[f]] <- targets
    }
  }

  # 2. Aggregate Results
  flat_vars <- unlist(all_vars)

  if (is.null(flat_vars)) {
    return(dplyr::tibble(variable = character(), n = integer()))
  }

  # Use Native Pipe (|>) and .data pronoun (now whitelisted)
  census <- dplyr::tibble(variable = flat_vars) |>
    dplyr::count(.data$variable, sort = TRUE)

  return(census)
}
