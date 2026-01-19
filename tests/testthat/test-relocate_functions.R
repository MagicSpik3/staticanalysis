test_that("relocate_functions extracts and moves code", {

  # 1. Setup Source Environment
  src_dir <- fs::dir_create(file.path(tempdir(), "src_repo"))
  dest_dir <- file.path(tempdir(), "dest_repo") # Will be created by function

  # Create a legacy file with two functions
  writeLines(c(
    "keep_me <- function() { 1 + 1 }",
    "move_me <- function() { return('moved') }"
  ), file.path(src_dir, "legacy.R"))

  # 2. Mock an Inventory (as if audit_inventory() produced it)
  mock_inv <- data.frame(
    name = c("keep_me", "move_me"),
    type = "function",
    file = "legacy.R", # Relative path inside src_dir
    stringsAsFactors = FALSE
  )

  # 3. Define the Move Plan
  # Move 'move_me' to the new destination
  plan <- list()
  plan[[dest_dir]] <- c("move_me")

  # 4. Execute Relocation
  relocate_functions(mock_inv, plan, source_root = src_dir, dry_run = FALSE)

  # 5. Verify
  # Check file exists
  expected_file <- file.path(dest_dir, "move_me.R")
  expect_true(file.exists(expected_file))

  # Check content contains the code
  content <- readLines(expected_file)
  expect_true(any(grepl("return\\('moved'\\)", content)))

  # Check 'keep_me' was NOT moved (file shouldn't exist)
  expect_false(file.exists(file.path(dest_dir, "keep_me.R")))

  # Cleanup
  fs::dir_delete(src_dir)
  fs::dir_delete(dest_dir)
})
