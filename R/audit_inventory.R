#' Audit Project Inventory
#'
#' Scans a project to index all defined functions and checks test coverage.
#' Orchestrates scan_definitions() and check_test_coverage().
#'
#' @param dir_path String. Path to the package root.
#' @return A tibble listing objects, their locations, and test coverage status.
#' @export
audit_inventory <- function(dir_path) {
  if (!fs::dir_exists(dir_path)) stop("Directory not found.")

  # 1. Get Files
  source_files <- list_r_files(dir_path, type = "source")
  test_files <- list_r_files(dir_path, type = "test")

  # 2. Scan Definitions
  inv_df <- scan_definitions(source_files, root_dir = dir_path)

  # 3. Check Usage
  final_df <- check_test_coverage(inv_df, test_files)

  if (!is.null(final_df)) {
    final_df <- final_df[order(final_df$type, final_df$name), ]
  }

  return(final_df)
}
