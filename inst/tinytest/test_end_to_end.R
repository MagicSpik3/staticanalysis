# Test: End-to-End: Complex Payroll Model (Level 1 to 10)
  # 1. Setup Artifact
  # We write it dynamically to ensure the test is self-contained,
  # or you can read the static file you created above.
  csv_content <- data.frame(
    Target = c(
      "base_salary",
      "years_service",
      "bonus_rate",
      "bonus",
      "total_gross",
      "is_high_earner",
      "tax_rate",
      "tax_paid",
      "net_pay",
      "audit_token",
      "stat_check"
    ),
    Rule = c(
      "50000",
      "5",
      "ifelse(years_service > 3, 0.15, 0.05)", # Logic
      "base_salary * bonus_rate", # Dependency
      "base_salary + bonus", # Math
      "total_gross > 45000", # Boolean
      "ifelse(is_high_earner, 0.4, 0.2)", # Derived Logic
      "total_gross * tax_rate",
      "total_gross - tax_paid",
      "paste('EMP', years_service, round(net_pay), sep='_')", # String
      "stats::median(c(base_salary, total_gross, net_pay))" # External Pkg
    )
  )

  e2e_file <- tempfile(fileext = ".csv")
  write.csv(csv_content, e2e_file, row.names = FALSE)


  # 1. COMPILE (Discovery Mode: No allowed_vars)
  # This now SUCCEEDS without error!
  recipe <- compile_rules(e2e_file, allowed_vars = NULL)
  expect_s3_class(recipe, "rule_recipe")

  # 2. AUDIT (The new "Spirit" check)
  # We ensure there are no unintended typos in our complex model
  typos <- audit_recipe_typos(recipe)
  expect_equal(nrow(typos), 0) # Expect clean code

  # 3. RUN
  results <- run_recipe(recipe)
  expect_equal(results$net_pay, 34500)

  # 3. INSPECT
  # Verify we detected the implicit outputs correctly
  inspection <- inspect_recipe(recipe)
  expect_true("net_pay" %in% inspection$outputs_created)
  expect_true("stat_check" %in% inspection$outputs_created)
