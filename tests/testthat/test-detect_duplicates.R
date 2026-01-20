test_that("detect_duplicates identifies logic clones", {
  proj_dir <- fs::dir_create(tempfile())

  # Create a file with 3 functions:
  # 1. annualise_pay:  input * 12
  # 2. annualise_col:  col * 52    (Structurally same if ignore_constants=TRUE)
  # 3. weird_math:     input + 10  (Different structure)

  r_file <- file.path(proj_dir, "utils.R")
  test_setup <- c(
    "annualise_pay <- function(pay) { return(pay * 12) }",
    "annualise_col <- function(col) { return(col * 52) }",
    "weird_math    <- function(x)   { return(x + 10) }"
  )
  print(paste('test_setup', test_setup))
  writeLines(test_setup, r_file)

  # Run Detector
  # We assume audit_inventory works (it's used internally)
  dupes <- detect_duplicates(proj_dir, ignore_constants = TRUE)

  print(dupes)
  print(nrow(dupes))
  # Verify
  expect_equal(nrow(dupes), 2) # pay and col should be in the list
  expect_true("annualise_pay" %in% dupes$name)
  expect_true("annualise_col" %in% dupes$name)
  expect_false("weird_math" %in% dupes$name)

  # They should share the same Group ID
  ids <- unique(dupes$group_id)
  expect_equal(length(ids), 1)

  fs::dir_delete(proj_dir)
})
