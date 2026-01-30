test_that("validate_file_io approves existing literal files", {
  # 1. Setup: Create a real file
  temp_dir <- withr::local_tempdir()
  real_file <- file.path(temp_dir, "data.csv")
  file.create(real_file)

  # 2. Mock Report
  report <- data.frame(
    type = "literal",
    path_pattern = real_file,
    root_var = NA,
    file = "script.R",
    func = "read.csv",
    stringsAsFactors = FALSE
  )

  # 3. Assert (Should pass)
  # Suppress messages to keep test output clean, or check them if desired
  expect_true(suppressMessages(validate_file_io(report)))
})

test_that("validate_file_io catches missing literal files", {
  # 1. Setup: Define a path that definitely doesn't exist
  temp_dir <- withr::local_tempdir()
  ghost_file <- file.path(temp_dir, "ghost.csv")

  report <- data.frame(
    type = "literal",
    path_pattern = ghost_file,
    root_var = NA,
    file = "script.R",
    func = "read.csv",
    stringsAsFactors = FALSE
  )

  # 2. Assert (Should fail)
  # We also check that it prints the alert (stderr)
  options(cli.num_colors = 0)
  expect_message(
    res <- validate_file_io(report),
    "MISSING"
  )
  expect_false(res)
})

test_that("validate_file_io resolves constructed paths using context", {
  # 1. Setup: Create 'project/inputs/data.csv'
  temp_dir <- withr::local_tempdir()
  dir.create(file.path(temp_dir, "inputs"))
  real_file <- file.path(temp_dir, "inputs", "data.csv")
  file.create(real_file)

  # 2. Mock Report: source(file.path(proj_root, "inputs", "data.csv"))
  # The scanner would extract: root_var="proj_root", path_pattern="inputs/data.csv"
  # (Assuming your scanner logic combines the non-root parts, or we simplify for this test)

  # Let's assume the scanner gave us:
  # root_var = "proj_root"
  # path_pattern = "inputs/data.csv"

  report <- data.frame(
    type = "constructed",
    path_pattern = file.path("inputs", "data.csv"),
    root_var = "proj_root",
    file = "script.R",
    func = "read.csv",
    stringsAsFactors = FALSE
  )

  # 3. Context Map
  ctx <- list(proj_root = temp_dir)

  # 4. Assert
  expect_true(suppressMessages(validate_file_io(report, context_vars = ctx)))

  # 5. Counter-test: If we give the wrong context, it fails
  ctx_bad <- list(proj_root = "/tmp/nowhere")
  expect_false(suppressMessages(validate_file_io(report, context_vars = ctx_bad)))
})

test_that("validate_file_io skips rows when context variable is missing", {
  # If the code uses `file.path(unknown_var, "file.csv")` and we don't provide
  # 'unknown_var', the function should SKIP it (return TRUE), not crash or fail.

  report <- data.frame(
    type = "constructed",
    path_pattern = "file.csv",
    root_var = "mystery_var",
    file = "script.R",
    func = "read.csv",
    stringsAsFactors = FALSE
  )

  # Pass empty context
  expect_true(suppressMessages(validate_file_io(report, list())))
})

test_that("validate_file_io handles NULL/Empty input gracefully", {
  expect_true(validate_file_io(NULL))
  expect_true(suppressMessages(validate_file_io(data.frame())))
})
