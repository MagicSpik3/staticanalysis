test_that("migrate_namespace swaps package calls", {
  # 1. Setup Mock Project
  proj_dir <- fs::dir_create(file.path(tempdir(), "migration_test"))

  # Create a file using the old package
  code_file <- file.path(proj_dir, "script.R")
  writeLines(c(
    "library(tidytable)",
    "df <- tidytable::mutate(df, a = 1)"
  ), code_file)

  # 2. Execute Migration (tidytable -> dplyr)
  migrate_namespace(proj_dir, from_pkg = "tidytable", to_pkg = "dplyr", dry_run = FALSE)

  # 3. Verify
  new_code <- readLines(code_file)

  # Check library swap
  expect_true(any(grepl("library\\(dplyr\\)", new_code)))
  expect_false(any(grepl("library\\(tidytable\\)", new_code)))

  # Check function swap
  expect_true(any(grepl("dplyr::mutate", new_code)))

  # Cleanup
  fs::dir_delete(proj_dir)
})
