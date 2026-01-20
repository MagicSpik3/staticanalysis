test_that("audit_inventory identifies functions and checks test coverage", {
  # 1. Setup Mock Project Structure
  proj_dir <- fs::dir_create(tempfile())
  r_dir <- fs::dir_create(file.path(proj_dir, "R"))
  tests_dir <- fs::dir_create(file.path(proj_dir, "tests", "testthat"))

  # 2. Create Source Code
  # File A: Contains a function and a variable
  writeLines(c(
    "my_constant <- 42",
    "calc_sum <- function(a, b) { a + b }"
  ), file.path(r_dir, "math_utils.R"))

  # File B: Contains another function
  writeLines(c(
    "calc_diff <- function(a, b) { a - b }"
  ), file.path(r_dir, "diff_utils.R"))

  # 3. Create Test Code
  # We only test 'calc_sum', we ignore 'calc_diff'
  writeLines(c(
    "test_that('sum works', {",
    "  expect_equal(calc_sum(1, 1), 2)",
    "})"
  ), file.path(tests_dir, "test-math.R"))

  # 4. Run Inventory Audit
  inventory <- audit_inventory(proj_dir)

  # 5. Verify Results

  # Check we found 3 objects
  expect_equal(nrow(inventory), 3)

  # Check 'calc_sum' (Should be Function, Tested)
  sum_row <- inventory[inventory$name == "calc_sum", ]
  expect_equal(sum_row$type, "function")
  expect_true(sum_row$called_in_test)

  # Check 'calc_diff' (Should be Function, Untested)
  diff_row <- inventory[inventory$name == "calc_diff", ]
  expect_equal(diff_row$type, "function")
  expect_false(diff_row$called_in_test)

  # Check 'my_constant' (Should be Variable, Untested)
  var_row <- inventory[inventory$name == "my_constant", ]
  expect_equal(var_row$type, "variable")

  # Check File Locations
  # path_rel usually returns just the filename if it's direct, or R/filename
  # We check if the string ends with the expected filename
  expect_true(grepl("math_utils.R$", sum_row$file))

  # Cleanup
  fs::dir_delete(proj_dir)
})
