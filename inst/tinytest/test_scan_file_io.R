# Test: scan_file_io detects literal, variable, and constructed reads
  # 1. Setup
  temp_dir <- withr::local_tempdir()

  code <- '
    # 1. Literal
    df <- read.csv("data/input.csv")

    # 2. Variable
    path <- "data/other.rds"
    obj <- readRDS(path)

    # 3. Constructed (file.path)
    source(file.path(config_dir, "utils.R"))

    # 4. Namespaced
    haven::read_sav("survey.sav")

    # 5. Nested (Recursion Check)
    if (TRUE) {
      load("nested.RData")
    }
  '
  writeLines(code, file.path(temp_dir, "analysis.R"))

  # 2. Run Scanner
  res <- scan_file_io(temp_dir)

  # 3. Assertions
  expect_false(is.null(res))

  # Bug Fix Check: Ensure read_sav is in your scanner's allowed list!
  expect_equal(nrow(res), 5)

  # Check Literal
  # Use match or which to avoid NA rows
  lit <- res[which(res$path_pattern == "data/input.csv"), ]
  expect_equal(nrow(lit), 1)
  expect_equal(lit$type, "literal")
  expect_equal(lit$func, "read.csv")

  # Check Variable
  var_row <- res[which(res$root_var == "path"), ]
  expect_equal(nrow(var_row), 1)
  expect_equal(var_row$type, "variable")
  expect_equal(var_row$func, "readRDS")

  # Check Constructed
  constr <- res[which(res$type == "constructed"), ]
  expect_equal(nrow(constr), 1)
  expect_equal(constr$root_var, "config_dir")
  expect_equal(constr$path_pattern, "utils.R") # Expect clean string, no quotes
  expect_equal(constr$func, "source")

  # Check Namespaced
  # Note: Your scanner extracts the function name (read_sav), not the package
  ns_row <- res[which(res$func == "read_sav"), ]
  expect_equal(nrow(ns_row), 1)
  expect_equal(ns_row$path_pattern, "survey.sav")

  # Check Recursion
  nest <- res[which(res$func == "load"), ]
  expect_equal(nrow(nest), 1)
  expect_equal(nest$path_pattern, "nested.RData")

# Test: scan_file_io returns NULL for code with no IO
  temp_dir <- withr::local_tempdir()
  writeLines("x <- 1 + 1", file.path(temp_dir, "math.R"))
  expect_null(scan_file_io(temp_dir))
