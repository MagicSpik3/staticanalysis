# Test: infer_path_contract detects INPUT roles from read functions
  # 1. Setup: Create code that reads from the config object
  temp_dir <- withr::local_tempdir()

  code <- '
    # Standard dollar sign access
    data <- read.csv(paths$raw_data)

    # Nested read
    obj <- readRDS(file = paths$parameters)
  '
  writeLines(code, file.path(temp_dir, "inputs.R"))

  # 2. Run
  contract <- infer_path_contract(temp_dir)

  # 3. Assert
  expect_false(is.null(contract))

  # Check raw_data mapping
  raw <- contract[contract$key == "raw_data", ]
  expect_equal(nrow(raw), 1)
  expect_equal(raw$role, "INPUT")
  expect_match(raw$evidence, "read.csv")

  # Check parameters mapping
  param <- contract[contract$key == "parameters", ]
  expect_equal(nrow(param), 1)
  expect_equal(param$role, "INPUT")

# Test: infer_path_contract detects OUTPUT roles from write functions
  temp_dir <- withr::local_tempdir()

  # Code that writes to the config object
  code <- '
    write.csv(df, paths$final_results)
    saveRDS(model, file = paths$model_output)
  '
  writeLines(code, file.path(temp_dir, "outputs.R"))

  contract <- infer_path_contract(temp_dir)

  # Check final_results mapping
  res <- contract[contract$key == "final_results", ]
  expect_equal(nrow(res), 1)
  expect_equal(res$role, "OUTPUT")

  # Check model_output mapping
  mod <- contract[contract$key == "model_output", ]
  expect_equal(nrow(mod), 1)
  expect_equal(mod$role, "OUTPUT")

# Test: infer_path_contract handles bracket notation paths[['key']]
  # Some developers prefer brackets for safety or dynamic keys
  temp_dir <- withr::local_tempdir()

  code <- '
    x <- read_feather(paths[["arrow_file"]])
    write.table(x, paths[["csv_export"]])
  '
  writeLines(code, file.path(temp_dir, "brackets.R"))

  contract <- infer_path_contract(temp_dir)

  # Check Input
  arrow <- contract[contract$key == "arrow_file", ]
  expect_equal(nrow(arrow), 1)
  expect_equal(arrow$role, "INPUT")

  # Check Output
  csv <- contract[contract$key == "csv_export", ]
  expect_equal(nrow(csv), 1)
  expect_equal(csv$role, "OUTPUT")

# Test: infer_path_contract respects custom config variable names
  temp_dir <- withr::local_tempdir()

  # Scenario: User names their config "conf" instead of "paths"
  code <- '
    read.csv(conf$my_input)
    # This should be IGNORED because it uses "paths"
    write.csv(x, paths$fake_out)
  '
  writeLines(code, file.path(temp_dir, "custom.R"))

  # Pass the custom name
  contract <- infer_path_contract(temp_dir, config_var = "conf")

  # Should find 'my_input'
  expect_true("my_input" %in% contract$key)

  # Should NOT find 'fake_out'
  expect_false("fake_out" %in% contract$key)

# Test: infer_path_contract returns unique rows for duplicate usage
  temp_dir <- withr::local_tempdir()

  # Same key used multiple times
  code <- '
    d1 <- read.csv(paths$same_key)
    d2 <- read.csv(paths$same_key)
  '
  writeLines(code, file.path(temp_dir, "dupe.R"))

  contract <- infer_path_contract(temp_dir)

  # Should be deduped to 1 row
  target <- contract[contract$key == "same_key", ]
  expect_equal(nrow(target), 1)
