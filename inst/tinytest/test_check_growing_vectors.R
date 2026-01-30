# Test: staticanalysis:::check_growing_vectors detects basic quadratic growth pattern
  # Scenario: The classic beginner mistake
  code <- "
    res <- integer()
    for (i in 1:100) {
      # This triggers the smell
      res <- c(res, i)
    }
  "

  # Helper to mimic the tool's pipeline
  pdata <- utils::getParseData(parse(text = code, keep.source = TRUE))
  smells <- staticanalysis:::check_growing_vectors(pdata, "bad_loop.R")

  expect_false(is.null(smells))
  expect_equal(nrow(smells), 1)
  expect_equal(smells$id, "GROWING_VECTOR")
  expect_match(smells$message, "res") # Should mention the specific variable
  expect_equal(smells$severity, "CRITICAL")

# Test: staticanalysis:::check_growing_vectors detects growth in WHILE loops
  # Scenario: While loop usage
  code <- "
    while (condition) {
      log <- c(log, 'entry')
    }
  "

  pdata <- utils::getParseData(parse(text = code, keep.source = TRUE))
  smells <- staticanalysis:::check_growing_vectors(pdata, "while_loop.R")

  expect_false(is.null(smells))
  expect_equal(smells$id, "GROWING_VECTOR")

# Test: staticanalysis:::check_growing_vectors ignores safe pre-allocation
  # Scenario: Correct usage (x[i] <- val)
  # This has an assignment inside a loop, but no c() call on itself
  code <- "
    res <- numeric(100)
    for (i in 1:100) {
      res[i] <- i * 2
    }
  "

  pdata <- utils::getParseData(parse(text = code, keep.source = TRUE))
  smells <- staticanalysis:::check_growing_vectors(pdata, "safe_loop.R")

  expect_null(smells)

# Test: staticanalysis:::check_growing_vectors ignores assignment to DIFFERENT variable
  # Scenario: y <- c(x, new)
  # This creates a new object 'y', it doesn't grow 'y' recursively.
  # (It might be inefficient, but it's not the quadratic 'Grow' pattern on LHS)
  code <- "
    for (i in 1:10) {
      new_ver <- c(old_ver, i)
    }
  "

  pdata <- utils::getParseData(parse(text = code, keep.source = TRUE))
  smells <- staticanalysis:::check_growing_vectors(pdata, "diff_var.R")

  expect_null(smells)

# Test: staticanalysis:::check_growing_vectors handles assignments outside loops correctly
  # Scenario: c() usage outside loop should be ignored
  code <- "
    x <- c(x, 1)
    for (i in 1:10) {
      print(i)
    }
  "

  pdata <- utils::getParseData(parse(text = code, keep.source = TRUE))
  smells <- staticanalysis:::check_growing_vectors(pdata, "outside.R")

  expect_null(smells)

# Test: staticanalysis:::check_growing_vectors detects growing with complex expressions
  # Scenario: x <- c(x, f(i) + 1)
  # Ensure the detector isn't confused by complex RHS arguments
  code <- "
    for (i in 1:10) {
      data <- c(data, complex_calc(i, 2))
    }
  "

  pdata <- utils::getParseData(parse(text = code, keep.source = TRUE))
  smells <- staticanalysis:::check_growing_vectors(pdata, "complex.R")

  expect_false(is.null(smells))
  expect_equal(smells$id, "GROWING_VECTOR")
  expect_match(smells$message, "data")
