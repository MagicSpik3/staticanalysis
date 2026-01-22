# Test: refactor_misplaced performs surgical extraction of roommate functions
  # 1. Setup Sandbox Environment
  # Create a temp directory acting as a package root
  root <- fs::dir_create(file.path(tempdir(), "refactor_test"))
  r_dir <- fs::dir_create(file.path(root, "R"))

  # Clean up after test
  on.exit(fs::dir_delete(root))

  # 2. Create the "Roommate" Scenario
  # A single file 'owner.R' containing TWO functions
  code_content <- c(
    "#' The Owner Function",
    "#' @description This belongs here.",
    "owner <- function() { return(TRUE) }",
    "",
    "#' The Roommate Function",
    "#' @description This should be moved.",
    "#' @export",
    "roommate <- function() { return(FALSE) }"
  )

  owner_file_path <- file.path(r_dir, "owner.R")
  writeLines(code_content, owner_file_path)

  # Verify setup: File exists and has both
  expect_true(fs::file_exists(owner_file_path))

  # 3. Run the Refactor Tool
  # We point it to our temp root
  # Note: ensure 'refactor_misplaced' is loaded in your R session or package
  staticanalysis::refactor_misplaced(root, dry_run = FALSE)

  # 4. Verify The "Owner" Stayed Put
  expect_true(fs::file_exists(owner_file_path))
  new_owner_content <- readLines(owner_file_path)

  # It should still contain 'owner'
  expect_true(any(grepl("owner <- function", new_owner_content)))
  # It should NOT contain 'roommate' anymore
  expect_false(any(grepl("roommate <- function", new_owner_content)))

  # 5. Verify The "Roommate" Moved Out
  new_roommate_path <- file.path(r_dir, "roommate.R")
  expect_true(fs::file_exists(new_roommate_path))

  new_roommate_content <- readLines(new_roommate_path)
  # It should contain the function code
  expect_true(any(grepl("roommate <- function", new_roommate_content)))
  # It should contain the Roxygen documentation (Critical check)
  expect_true(any(grepl("#' The Roommate Function", new_roommate_content)))
