test_that("execute_safe_rules processing logic", {

  # --- SCENARIO A: Valid Logic ---
  good_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    Target = c("r"),
    Rule   = c("10 * 50")
  ), good_file, row.names = FALSE)

  output <- execute_safe_rules(good_file)
  expect_equal(output$r, 500)

  # --- SCENARIO B: Invalid Variable (Typos) ---
  bad_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    Target = c("r2"), # <--- TYPO
    Rule   = c("10 * 50")
  ), bad_file, row.names = FALSE)

  expect_error(
    execute_safe_rules(bad_file, allowed_vars = c("r")),
    "Variable 'r2' is not allowed"
  )

  # --- SCENARIO C: Security Block ---
  hack_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    Target = c("r"),
    Rule   = c("system('ls')")
  ), hack_file, row.names = FALSE)

  expect_error(
    execute_safe_rules(hack_file),
    "SECURITY ALERT"
  )
})

test_that("execute_safe_rules processes real artifacts from filesystem", {

  # 1. Locate the artifact (Robust Pathing)
  # This looks inside tests/testthat/artifacts/
  real_file <- test_path("artifacts", "accountant_rules.csv")

  # Ensure it exists before running (Good practice for integration tests)
  expect_true(file.exists(real_file), info = "Artifact file missing!")

  # 2. Run the Engine
  # We allow 'r', 'tax', and 'net' for this specific scenario
  output <- execute_safe_rules(real_file, allowed_vars = c("r", "tax", "net"))

  # 3. Verify the "Real" Logic
  # r: 5200
  # tax: 1040
  # net: 4160
  expect_equal(output$r, 5200)
  expect_equal(output$tax, 1040)
  expect_equal(output$net, 4160)
})
