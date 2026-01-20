test_that("visualize_rules consumes a Recipe object", {
  # 1. Setup: Get a compiled recipe
  real_file <- testthat::test_path("artifacts/accountant_rules.csv")
  recipe <- compile_rules(real_file)

  # 2. Run Visualizer
  graph <- visualize_rules(recipe)

  # 3. Verify
  expect_s3_class(graph, "htmlwidget")

  # Check that 'r' flows into 'tax'
  expect_true(grepl("r -> tax", graph$x$diagram))
})
