test_that("visualize_rules generates a graph from artifacts", {

  # 1. Locate the artifact
  real_file <- test_path("artifacts", "accountant_rules.csv")

  # 2. Run the Visualizer
  graph <- visualize_rules(real_file)

  # 3. Verify it returned a DiagrammeR object
  expect_s3_class(graph, "grViz")
  expect_s3_class(graph, "htmlwidget")

  # 4. (Optional) Check the HTML source contains our nodes
  # logic: r depends on nothing (in file context), tax depends on r
  expect_true(grepl("r -> tax", graph$x$diagram))
  expect_true(grepl("r -> net", graph$x$diagram))
})
