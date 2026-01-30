# Test: map_internal_calls detects function dependencies
  # 1. Create a "Mini-Project" in a temp directory
  temp_dir <- withr::local_tempdir()
  # Create the R folder structure
  r_dir <- file.path(temp_dir, "R")
  dir.create(r_dir)

  # --- File 1: utils.R (Defines 'helper') ---
  writeLines("
    helper <- function() {
      print('I am helping')
    }
  ", file.path(r_dir, "utils.R"))

  # --- File 2: main.R (Defines 'process') ---
  # 'process' calls:
  # - helper()  -> INTERNAL (Should be detected)
  # - mean()    -> EXTERNAL (Should be ignored)
  # - process() -> RECURSIVE (Should be ignored by setdiff)
  writeLines("
    process <- function(data) {
      x <- helper()
      y <- mean(data)
      if (x) process(data)
    }
  ", file.path(r_dir, "main.R"))

  # 2. Mock the Inventory
  # This mimics what audit_inventory() would return for these files
  inventory <- data.frame(
    name = c("helper", "process"),
    type = "function",
    # Important: Inventory usually stores paths relative to project root ("R/...")
    file = c("R/utils.R", "R/main.R"),
    stringsAsFactors = FALSE
  )

  # 3. Run the Mapper
  # We pass temp_dir as the root so it can resolve "R/utils.R" correctly
  edges <- map_internal_calls(inventory, dir_path = temp_dir)

  # 4. Assertions

  # Expect exactly 1 edge: process -> helper
  expect_equal(nrow(edges), 1)
  expect_equal(edges$from, "process")
  expect_equal(edges$to, "helper")

# Test: map_internal_calls handles empty or missing files gracefully
  temp_dir <- withr::local_tempdir()

  # Inventory points to a file that doesn't exist
  inventory <- data.frame(
    name = "ghost_func",
    type = "function",
    file = "R/ghost.R",
    stringsAsFactors = FALSE
  )

  # Should not crash, just return empty edges
  edges <- map_internal_calls(inventory, dir_path = temp_dir)
  expect_equal(nrow(edges), 0)
  expect_named(edges, c("from", "to"))

# Test: map_internal_calls ignores non-function items
  temp_dir <- withr::local_tempdir()

  # Inventory contains a "dataset" or "variable"
  inventory <- data.frame(
    name = "my_data",
    type = "data", # Not a function
    file = "data/d.R",
    stringsAsFactors = FALSE
  )

  edges <- map_internal_calls(inventory, dir_path = temp_dir)
  expect_equal(nrow(edges), 0)
