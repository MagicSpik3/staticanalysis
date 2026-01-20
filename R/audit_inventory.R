#' Audit Project Inventory
#'
#' Scans a project to index all defined functions and checks test coverage.
#' Also validates if functions are in correctly named files.
#'
#' @param dir_path String. Path to the package root.
#' @return A tibble listing objects, their locations, coverage, and placement status.
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
    # ---------------------------------------------------------
    # NEW: Check for Misplaced Functions
    # Rule: A function named 'foo' MUST be in 'R/foo.R' or 'foo.R'
    # ---------------------------------------------------------

    # Extract filename without extension (e.g. "R/utils.R" -> "utils")
    clean_filenames <- tools::file_path_sans_ext(basename(final_df$file))

    # Logic: It is misplaced if:
    # 1. It is a function
    # 2. Its name does NOT match the filename
    final_df$misplaced <- (
      final_df$type == "function" & final_df$name != clean_filenames
    )
    # FIX: Use method = "radix" to allow mixed sorting directions
    # Sort: Misplaced (TRUE) first, then Type, then Name
    # Calculate order separately to satisfy linter
    sort_order <- order(
      final_df$misplaced,
      final_df$type,
      final_df$name,
      decreasing = c(TRUE, FALSE, FALSE),
      method = "radix"
    )

    final_df <- final_df[sort_order, ]
  }

  return(final_df)
}
