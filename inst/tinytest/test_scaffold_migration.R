# Test: scaffold_migration extracts function and creates test
  # 1. Setup Mock Environment
  mock_dir <- fs::dir_create(file.path(tempdir(), "migration_test"))
  on.exit(fs::dir_delete(mock_dir))

  legacy_file <- file.path(mock_dir, "legacy.R")

  # Create a legacy script with 2 functions
  writeLines(c(
    "# Helper function",
    "helper <- function(x) { x + 1 }",
    "",
    "# Target function",
    "target_func <- function(y) {",
    "  return(y * 2)",
    "}"
  ), legacy_file)

  # 2. Run Migration
  scaffold_migration("target_func", legacy_file, mock_dir)

  # 3. Verify R File Created
  new_r <- file.path(mock_dir, "R", "target_func.R")
  expect_true(file.exists(new_r))

  # Verify content (Should be just the target function)
  content <- readLines(new_r)
  expect_true(any(grepl("target_func <- function", content)))
  expect_true(any(grepl("return\\(y \\* 2\\)", content)))
  expect_false(any(grepl("helper", content))) # Should NOT include the other function

  # 4. Verify Test File Created
  new_test <- file.path(mock_dir, "tests", "testthat", "test-target_func.R")
  expect_true(file.exists(new_test))

  # Verify it is a Red Light test
  test_content <- readLines(new_test)
  expect_true(any(grepl("expect_true\\(FALSE\\)", test_content)))
