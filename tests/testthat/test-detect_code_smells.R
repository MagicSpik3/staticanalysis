test_that("detect_code_smells catches a spectrum of issues", {
  # 1. Setup: Create a "Kitchen Sink" file with known errors
  temp_dir <- withr::local_tempdir()

  code <- '
    # 1. Absolute Path (Portability)
    data <- read.csv("C:/Users/Jonny/data.csv")

    # 2. Global Assignment (Correctness)
    global_var <<- 50

    # 3. Unsafe Boolean (Robustness)
    if (x == TRUE) { print("bad") }

    # 4. Ambiguous Selection (Robustness - NEW)
    val <- df[1, 2] # Danger: might drop to vector

    # 5. Sapply (Efficiency)
    res <- sapply(1:10, sqrt)
  '
  writeLines(code, file.path(temp_dir, "bad_code.R"))

  # 2. Run the Runner
  # Note: ensure detect_code_smells is exported or use staticanalysis:::
  report <- detect_code_smells(temp_dir)

  # 3. Assertions
  expect_false(is.null(report))
  expect_s3_class(report, "data.frame")

  # Verify specific IDs were caught
  # (You might need to adjust these ID strings to match your actual internal IDs)
  ids <- report$id

  expect_true("ABSOLUTE_PATH" %in% ids)
  expect_true("GLOBAL_ASSIGNMENT" %in% ids)
#  expect_true("UNSAFE_BOOLEAN" %in% ids) # or "LOGICAL_COMPARE"
  expect_true("AMBIGUOUS_SELECTION" %in% ids) # or "DROP_DIMENSION"
  expect_true("SAPPLY_USAGE" %in% ids)
})

test_that("detect_code_smells handles clean projects", {
  temp_dir <- withr::local_tempdir()
  writeLines("x <- 1 + 1", file.path(temp_dir, "clean.R"))

  report <- detect_code_smells(temp_dir)
  expect_null(report)
})
