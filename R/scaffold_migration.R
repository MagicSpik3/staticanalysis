#' Scaffold Migration
#'
#' Extracts a function from a legacy script and places it in its own file.
#'
#' @param func_name String. The name of the function to migrate.
#' @param source_file String. Path to the legacy R script.
#' @param target_dir String. Root of the package.
#' @return Logical TRUE if successful.
#' @export
scaffold_migration <- function(func_name, source_file, target_dir = ".") {
  if (!fs::file_exists(source_file)) stop("Source file not found.")

  pdata <- utils::getParseData(parse(source_file, keep.source = TRUE))

  # CALL THE HELPER
  loc <- find_func_lines(pdata, func_name)

  if (is.null(loc)) stop("Could not locate definition for ", func_name)

  # Extract
  all_lines <- readLines(source_file, warn = FALSE)
  func_lines <- all_lines[loc$start:loc$end]

  # Write Files (R + Test)
  r_dir <- file.path(target_dir, "R")
  fs::dir_create(r_dir)
  new_r_file <- file.path(r_dir, paste0(func_name, ".R"))

  if (!fs::file_exists(new_r_file)) {
    writeLines(func_lines, new_r_file)
    message(sprintf("[OK] Extracted '%s' to %s", func_name, basename(new_r_file)))
  }

  test_dir <- file.path(target_dir, "tests", "testthat")
  fs::dir_create(test_dir)
  new_test_file <- file.path(test_dir, paste0("test-", func_name, ".R"))

  if (!fs::file_exists(new_test_file)) {
    writeLines(c(paste0("test_that('", func_name, " works', { expect_true(FALSE) })")), new_test_file)
    message(sprintf("[OK] Created test: %s", basename(new_test_file)))
  }
  return(TRUE)
}
scaffold_migration <- function(func_name, source_file, target_dir = ".") {
  if (!fs::file_exists(source_file)) stop("Source file not found.")
  if (!fs::dir_exists(target_dir)) stop("Target directory not found.")

  # 1. Parse
  pdata <- utils::getParseData(parse(source_file, keep.source = TRUE))

  # 2. Find Boundaries using Helper
  loc <- find_func_lines(pdata, func_name)

  if (is.null(loc)) {
    stop("Could not locate definition for ", func_name)
  }

  # 3. Extract Lines
  all_lines <- readLines(source_file, warn = FALSE)
  func_lines <- all_lines[loc$start:loc$end]

  # 4. Write R File
  r_dir <- file.path(target_dir, "R")
  fs::dir_create(r_dir)
  new_r_file <- file.path(r_dir, paste0(func_name, ".R"))

  if (fs::file_exists(new_r_file)) {
    message(sprintf("[SKIP] File exists: %s", basename(new_r_file)))
  } else {
    writeLines(func_lines, new_r_file)
    message(sprintf("[OK] Extracted '%s' to %s", func_name, basename(new_r_file)))
  }

  # 5. Write Test File
  test_dir <- file.path(target_dir, "tests", "testthat")
  fs::dir_create(test_dir)
  new_test_file <- file.path(test_dir, paste0("test-", func_name, ".R"))

  if (fs::file_exists(new_test_file)) {
    message(sprintf("[SKIP] Test exists: %s", basename(new_test_file)))
  } else {
    test_content <- c(
      paste0("test_that(\"", func_name, " works as expected\", {"),
      "",
      "  # TODO: Implement this test to turn the light Green",
      "  expect_true(FALSE)",
      "",
      "})"
    )
    writeLines(test_content, new_test_file)
    message(sprintf("[OK] Created Red Light test: %s", basename(new_test_file)))
  }
  return(TRUE)
}
