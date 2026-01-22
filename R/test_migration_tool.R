# tools/migrate_tests.R
if (!requireNamespace("fs", quietly = TRUE)) install.packages("fs")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")

migrate_to_tinytest <- function(old_dir = "tests/testthat", new_dir = "inst/tinytest") {

  if (!fs::dir_exists(old_dir)) stop("Old test directory not found!")
  if (!fs::dir_exists(new_dir)) fs::dir_create(new_dir)

  files <- fs::dir_ls(old_dir, glob = "*.R")

  for (f in files) {
    content <- readLines(f)
    new_content <- content

    # --- SYNTAX CONVERSIONS ---

    # 1. Remove library(testthat)
    new_content <- new_content[!grepl("library\\(testthat\\)", new_content)]

    # 2. Convert context("name") to a simple comment
    new_content <- gsub('context\\("(.*?)"\\)', '# Context: \\1', new_content)

    # 3. Flatten 'test_that' blocks
    # Transforms: test_that("my description", {
    # Into:       # Test: my description
    new_content <- gsub('test_that\\("(.*?)"\\s*,\\s*\\{', '# Test: \\1', new_content)

    # Remove the closing brackets "}" of the test_that blocks
    # (This is a rough heuristic - it assumes the closing } is on its own line)
    new_content <- new_content[!grepl("^\\s*\\}\\)\\s*$", new_content)]

    # 4. Convert specific expectations

    # expect_is(x, "class") -> expect_inherits(x, "class")
    new_content <- gsub("expect_is\\(", "expect_inherits(", new_content)

    # expect_length(x, n) -> expect_equal(length(x), n)
    # This is hard to regex perfectly, so we warn the user
    if (any(grepl("expect_length", new_content))) {
      warning(paste("File", basename(f), "uses expect_length. Change to expect_equal(length(x), n) manually."))
    }

    # expect_match(x, "pattern") -> expect_true(grepl("pattern", x))
    # Tinytest handles grep internally for some things, but explicit is better.
    # We will leave expect_match as it often works, or flag it.

    # expect_error(code, regexp) -> expect_error(code, pattern=regexp)
    # Tinytest uses 'pattern' arg, testthat uses 'regexp'.
    # Usually positional args work fine, so we leave as is.

    # 5. Save file with new naming convention
    # testthat uses "test-name.R"
    # tinytest prefers "test_name.R"
    new_name <- gsub("test-", "test_", basename(f))
    new_path <- file.path(new_dir, new_name)

    writeLines(new_content, new_path)
    message("Converted: ", basename(f), " -> ", new_name)
  }

  message("\nMigration Complete! Check inst/tinytest/ for your new files.")
  message("Don't forget to delete 'tests/testthat' once you verify the new tests pass.")
}

# Run the migration
migrate_to_tinytest()
