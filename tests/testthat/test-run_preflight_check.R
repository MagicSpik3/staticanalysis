test_that("run_preflight_check passes when all files and directories exist", {
  # 1. Setup: Real environment
  temp_dir <- withr::local_tempdir()

  # Create Input
  input_path <- file.path(temp_dir, "raw_data.csv")
  file.create(input_path)

  # Create Output Directory
  output_dir <- file.path(temp_dir, "outputs")
  dir.create(output_dir)
  output_path <- file.path(output_dir, "final.rds")

  # 2. Config
  paths <- list(
    raw = input_path,
    processed = output_path
  )

  contract <- data.frame(
    key = c("raw", "processed"),
    role = c("INPUT", "OUTPUT"),
    evidence = c("read.csv", "saveRDS"),
    stringsAsFactors = FALSE
  )

  # 3. Assert
  # We suppress messages because the CLI output is verbose
  expect_true(suppressMessages(run_preflight_check(paths, contract)))
})

test_that("run_preflight_check fails on missing input file", {
  temp_dir <- withr::local_tempdir()

  # Path points to nothing
  paths <- list(raw = file.path(temp_dir, "missing.csv"))

  contract <- data.frame(
    key = "raw", role = "INPUT", evidence = "read", stringsAsFactors = FALSE
  )

  # We expect "MISSING INPUT" in the message log, and return FALSE
  options(cli.num_colors = 0)
  expect_message(
    res <- run_preflight_check(paths, contract),
    "MISSING INPUT"
  )
  expect_false(res)
})

test_that("run_preflight_check fails on missing output directory", {
  temp_dir <- withr::local_tempdir()

  # Directory "ghost_dir" does not exist, so we can't write there
  paths <- list(res = file.path(temp_dir, "ghost_dir", "out.csv"))

  contract <- data.frame(
    key = "res", role = "OUTPUT", evidence = "write", stringsAsFactors = FALSE
  )

  options(cli.num_colors = 0)
  expect_message(
    res <- run_preflight_check(paths, contract),
    "MISSING DIR"
  )
  expect_false(res)
})

test_that("run_preflight_check fails on placeholders in output paths", {
  temp_dir <- withr::local_tempdir()

  # User forgot to update the config template
  paths <- list(res = file.path(temp_dir, "*specify_v1*", "out.csv"))

  contract <- data.frame(
    key = "res", role = "OUTPUT", evidence = "write", stringsAsFactors = FALSE
  )

  options(cli.num_colors = 0)
  expect_message(
    res <- run_preflight_check(paths, contract),
    "CONFIG ERROR" # Placeholder found
  )
  expect_false(res)
})

test_that("run_preflight_check warns but passes if key is missing from paths", {
  # If the code uses "raw", but the paths list doesn't have "raw" defined.
  # Current logic is to WARN but not FAIL (errors not incremented).
  contract <- data.frame(key = "raw", role = "INPUT", evidence = "read", stringsAsFactors = FALSE)
  paths <- list() # Empty

  options(cli.num_colors = 0)
  expect_message(
    res <- run_preflight_check(paths, contract),
    "missing from paths list"
  )
  expect_true(res)
})

test_that("run_preflight_check handles NULL contract gracefully", {
  expect_true(suppressMessages(run_preflight_check(list(), NULL)))
})
