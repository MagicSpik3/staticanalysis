# test-pipeline.R
# Test: End-to-End Pipeline: Compile -> Run
  # 1. Locate the artifact (Now correctly inside testthat/artifacts)
  real_file <- testthat::test_path("artifacts/accountant_rules.csv")
  expect_true(file.exists(real_file))

  # 2. COMPILE (Should succeed)
  recipe <- compile_rules(real_file)

  # Check the Recipe Object
  expect_s3_class(recipe, "rule_recipe")
  expect_equal(length(recipe), 3) # We expect 3 steps (r, tax, net)
  expect_equal(recipe[[1]]$target, "r")

  # 3. RUN (Should succeed)
  results <- run_recipe(recipe)

  # Verify Math
  expect_equal(results$r, 5200)
  expect_equal(results$tax, 1040)
  expect_equal(results$net, 4160)

# Test: Compiler catches security threats
  # Mock a bad file
  bad_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(Target = "r", Rule = "system('ls')"), bad_file)

  # Compiler should throw error BEFORE we even try to run it
  expect_error(compile_rules(bad_file), "SECURITY ALERT")
