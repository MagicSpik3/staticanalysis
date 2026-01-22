# Test: compare_implementations detects identical logic
  # 1. Setup: Create a 'Legacy Monolith'
  monolith_file <- tempfile(fileext = ".R")
  writeLines(c(
    "hello <- function() { print('world') }",
    "calc  <- function(x) { x * 2 }"
  ), monolith_file)

  # 2. Setup: Create 'Refactored Directory'
  refactored_dir <- fs::dir_create(file.path(tempdir(), "R_refactored"))

  # Write the same functions into separate files
  writeLines("hello <- function() { print('world') }", file.path(refactored_dir, "hello.R"))
  writeLines("calc  <- function(x) { x * 2 }", file.path(refactored_dir, "calc.R"))

  # 3. Run Comparison
  results <- compare_implementations(monolith_file, refactored_dir)

  # 4. Verify Matches
  expect_true(results$hello$match)
  expect_true(results$calc$match)

  # 5. Verify Mismatch Detection
  # Create a file that differs
  writeLines("calc <- function(x) { x * 3 }", file.path(refactored_dir, "calc.R"))
  results_bad <- compare_implementations(monolith_file, refactored_dir)
  expect_false(results_bad$calc$match)
