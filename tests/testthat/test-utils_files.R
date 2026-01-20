test_that("list_r_files correctly separates source from tests", {
  # 1. Setup Mock Repo
  proj_dir <- fs::dir_create(tempfile())

  # Create directory structure
  fs::dir_create(file.path(proj_dir, "R"))
  fs::dir_create(file.path(proj_dir, "tests", "testthat"))
  fs::dir_create(file.path(proj_dir, "inst", "tinytest"))

  # 2. Create Files
  # -- Source Files --
  fs::file_create(file.path(proj_dir, "R", "app.R"))
  fs::file_create(file.path(proj_dir, "R", "legacy.r")) # Lowercase extension

  # -- Test Files --
  fs::file_create(file.path(proj_dir, "tests", "testthat", "test-app.R"))
  fs::file_create(file.path(proj_dir, "inst", "tinytest", "test_math.r"))

  # 3. Run: Get Source
  sources <- list_r_files(proj_dir, "source")
  expect_length(sources, 2)
  expect_true(any(grepl("app.R", sources)))
  expect_true(any(grepl("legacy.r", sources)))
  expect_false(any(grepl("test", sources))) # Should not have tests

  # 4. Run: Get Tests
  tests <- list_r_files(proj_dir, "test")
  expect_length(tests, 2)
  expect_true(any(grepl("test-app.R", tests)))
  expect_true(any(grepl("test_math.r", tests)))

  # Cleanup
  fs::dir_delete(proj_dir)
})
