test_that("get_file_ast parses valid R files", {
  # 1. Setup a valid R file
  temp_file <- withr::local_tempfile(fileext = ".R")
  writeLines("x <- 100 + 1", temp_file)

  # 2. Run
  # Use ::: to access the internal function
  pdata <- staticanalysis:::get_file_ast(temp_file)

  # 3. Verify AST structure
  expect_false(is.null(pdata))
  expect_s3_class(pdata, "data.frame")

  # It should contain the tokens we wrote
  tokens <- pdata$text
  expect_true("x" %in% tokens)
  expect_true("100" %in% tokens)
  expect_true("+" %in% tokens)
})

test_that("get_file_ast caches results (Memoization Check)", {
  # 1. Setup
  temp_file <- withr::local_tempfile(fileext = ".R")
  writeLines("y <- 2", temp_file)

  # Access the internal global environment
  # We need to ensure the cache starts empty for this file
  globals <- staticanalysis:::.staticanalysis_globals
  rm(list = ls(envir = globals$ast_cache), envir = globals$ast_cache)

  # 2. First Call (Should Parse and Cache)
  ast_1 <- staticanalysis:::get_file_ast(temp_file)
  expect_false(is.null(ast_1))

  # Check if it landed in the cache
  expect_true(exists(temp_file, envir = globals$ast_cache))

  # 3. The "Poison Pill" Test
  # We modify the cached version manually.
  # If the function is truly reading from cache, it will return this modified version.
  fake_ast <- ast_1
  fake_ast$POISON_TAG <- "I_WAS_CACHED"
  assign(temp_file, fake_ast, envir = globals$ast_cache)

  # 4. Second Call (Should Retrieve from Cache)
  ast_2 <- staticanalysis:::get_file_ast(temp_file)

  # 5. Assert we got the poisoned version (proving file wasn't re-parsed)
  expect_true("POISON_TAG" %in% names(ast_2))
  expect_equal(ast_2$POISON_TAG[1], "I_WAS_CACHED")
})

test_that("get_file_ast handles syntax errors gracefully", {
  temp_file <- withr::local_tempfile(fileext = ".R")
  # Write invalid code (missing closing brace)
  writeLines("if (TRUE) { print('oops')", temp_file)

  # Should return NULL, not crash
  pdata <- staticanalysis:::get_file_ast(temp_file)
  expect_null(pdata)
})
