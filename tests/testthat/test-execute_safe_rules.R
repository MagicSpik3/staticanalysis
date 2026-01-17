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
