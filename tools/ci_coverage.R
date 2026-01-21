# tools/ci_coverage.R
if (!requireNamespace("covr", quietly = TRUE)) install.packages("covr")

# 1. Run the coverage with your specific settings
cov <- covr::package_coverage(
  path = ".",
  type = "test",
  combine_types = TRUE,   # <--- This must be TRUE for the HTML report to work
  relative_path = TRUE,
  quiet = TRUE,
  clean = TRUE,
  line_exclusions = NULL,
  function_exclusions = NULL,
  pre_clean = TRUE
)

# 2. Print the text result (for the GitLab console/badge regex)
print(cov)

# 3. Save the HTML report (for the CI/CD artifact)
covr::report(cov, file = "coverage-report.html")
