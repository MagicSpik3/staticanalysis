test_that("Scanner detects undeclared dependencies (tidytable)", {
  # 1. Setup Mock Project
  proj_dir <- fs::dir_create(tempfile())

  # Create a DESCRIPTION that only declares 'dplyr'
  desc_file <- file.path(proj_dir, "DESCRIPTION")
  cat("Package: TestProj\nImports: dplyr\n", file = desc_file)

  # Create an R file that uses 'tidytable' (Undeclared)
  r_file <- file.path(proj_dir, "script.R")
  writeLines(c(
    "#' @importFrom tidytable mutate",
    "my_func <- function(x) {",
    "  # This is a comment about library(fake)",
    "  tidytable::filter(x, a > 1)",
    "}"
  ), r_file)

  # 2. Run Scanner
  report <- scan_dependencies(proj_dir)

  # 3. Verify Ghosts
  expect_true("tidytable" %in% report$undeclared_ghosts)

  # 4. Verify Usage Stats (The "Banana Check")
  # tidytable appears twice (once in roxygen, once in code)
  tt_stats <- report$usage_stats[report$usage_stats$package == "tidytable", ]
  expect_gt(tt_stats$count, 0)

  # Cleanup
  fs::dir_delete(proj_dir)
})
