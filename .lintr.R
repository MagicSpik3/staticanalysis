linters <- lintr::linters_with_defaults(
  line_length_linter = lintr::line_length_linter(120),
  return_linter = NULL,
  object_name_linter = NULL
)

exclusions <- list(
  "tests/testthat",
  "R/RcppExports.R"
)
