#' List R Files by Category
#'
#' Scans a directory for R scripts and separates them into 'source' (package code)
#' or 'test' (unit tests, tinytests, specs).
#'
#' @param dir_path String. Path to the project root.
#' @param type String. Either "source" (default) or "test".
#' @return A character vector of file paths.
#' @export
list_r_files <- function(dir_path, type = c("source", "test")) {
  type <- match.arg(type)

  if (!fs::dir_exists(dir_path)) {
    stop("Directory not found: ", dir_path)
  }

  # 1. Find ALL R files (Case insensitive .R / .r)
  all_files <- fs::dir_ls(dir_path, recurse = TRUE, regexp = "\\.[rR]$")
  all_files <- sort(all_files)

  # 2. Normalize paths for regex (Windows safety)
  # We convert strictly to forward slashes for matching
  norm_files <- gsub("\\\\", "/", all_files)

  # 3. Define the "Test" Pattern
  # Captures:
  # - /tests/
  # - /testthat/
  # - /inst/tinytest/ or /tinytest/
  # - /spec/ (often used in Ruby-ish R setups)
  # - /vignettes/ (often contain runnable R code examples)
  test_pattern <- "/tests/|/testthat/|/tinytest/|/spec/|/vignettes/"

  is_test <- grepl(test_pattern, norm_files, ignore.case = TRUE)

  if (type == "test") {
    return(as.character(all_files[is_test]))
  } else {
    return(as.character(all_files[!is_test]))
  }
}
