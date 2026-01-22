# Test: Inspector identifies external inputs vs internal variables
  # 1. Setup: Create a recipe
  # Logic:
  #   tax = r * 0.2    (Needs 'r', Creates 'tax')
  #   net = r - tax    (Needs 'r', 'tax'. 'tax' is internal, 'r' is external)

  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    Target = c("tax", "net"),
    Rule   = c("r * 0.2", "r - tax")
  ), f, row.names = FALSE)

  recipe <- compile_rules(f)

  # 2. Run Inspector
  report <- inspect_recipe(recipe)

  # 3. Verify
  # 'r' must be input
  expect_true("r" %in% report$inputs_needed)

  # 'tax' should NOT be an input (it was created in step 1)
  expect_false("tax" %in% report$inputs_needed)

  # Both should be outputs
  expect_equal(report$outputs_created, c("net", "tax"))
