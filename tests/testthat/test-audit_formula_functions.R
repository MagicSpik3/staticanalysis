test_that("audit_formula_functions counts function usage correctly", {
  # 1. Setup ISOLATED Directory
  # We create a unique sub-folder so we don't scan debris from other tests
  test_dir <- fs::dir_create(file.path(tempdir(), "audit_test_env"))
  on.exit(fs::dir_delete(test_dir)) # Auto-cleanup

  csv_file <- file.path(test_dir, "rules.csv")

  # Create a rule set with mixed usage:
  # - sum() appears twice (once nested)
  # - tidytable::mutate() appears once (namespaced)
  # - ifelse() appears once
  rules_df <- data.frame(
    Target = c("var1", "var2", "var3"),
    Rule = c(
      "sum(a, b)",
      "tidytable::mutate(x = 1)",
      "ifelse(x > 10, sum(c), 0)"
    )
  )
  write.csv(rules_df, csv_file, row.names = FALSE)

  # 2. Run Audit on the ISOLATED directory
  report <- audit_formula_functions(test_dir)

  # 3. Verify
  # sum: used twice
  sum_row <- report[report$function_name == "sum", ]
  expect_equal(sum_row$count, 2)

  # tidytable::mutate: used once
  tt_row <- report[report$function_name == "tidytable::mutate", ]
  expect_equal(tt_row$count, 1)

  # ifelse: used once
  if_row <- report[report$function_name == "ifelse", ]
  expect_equal(if_row$count, 1)
})
