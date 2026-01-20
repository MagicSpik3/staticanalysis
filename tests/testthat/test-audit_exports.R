test_that("audit_exports correctly identifies all 5 export statuses", {

  # 1. Setup: Create a temporary dummy package
  root <- fs::dir_create(file.path(tempdir(), "export_test_pkg"))
  r_dir <- fs::dir_create(file.path(root, "R"))

  # Create minimal DESCRIPTION so it looks like a package
  writeLines(c("Package: dummy", "Version: 0.1"), file.path(root, "DESCRIPTION"))

  # ---------------------------------------------------------
  # 2. Create the "House of Horrors" (The R Files)
  # ---------------------------------------------------------

  # A. EXPORTED (Perfect)
  writeLines(c(
    "#' @export",
    "good_export <- function() {}"
  ), file.path(r_dir, "good.R"))

  # B. INTERNAL (Your example - documented but not exported)
  writeLines(c(
    "#' @return Nothing",
    "not_exported <- function() {}"
  ), file.path(r_dir, "internal.R"))

  # C. MISSING_IN_NS (Tagged @export, but we won't put it in NAMESPACE)
  writeLines(c(
    "#' @export",
    "forgot_to_document <- function() {}"
  ), file.path(r_dir, "missing.R"))

  # D. DETACHED_TAG (The tricky syntax error)
  writeLines(c(
    "#' @export",
    "",  # <--- The deadly blank line
    "detached_func <- function() {}"
  ), file.path(r_dir, "detached.R"))

  # E. EXTRA_IN_NS (Not tagged, but exists in NAMESPACE)
  writeLines(c(
    "ghost_func <- function() {}"
  ), file.path(r_dir, "extra.R"))

  # ---------------------------------------------------------
  # 3. Create the NAMESPACE (The Truth)
  # We deliberately make it inconsistent to trigger the errors
  # ---------------------------------------------------------
  writeLines(c(
    "export(good_export)", # Correct
    "export(ghost_func)"   # Wrong (Source has no tag)
    # Missing: forgot_to_document
    # Missing: detached_func
    # Missing: not_exported (Correctly)
  ), file.path(root, "NAMESPACE"))

  # ---------------------------------------------------------
  # 4. Run the Audit
  # ---------------------------------------------------------
  # We use your new tool to scan this fake package
  inv <- staticanalysis::audit_inventory(root)

  # Filter to just the relevant columns for this test
  results <- inv[, c("name", "export_status")]

  # ---------------------------------------------------------
  # 5. Verify the 5 Statuses
  # ---------------------------------------------------------

  # 1. EXPORTED
  expect_equal(
    results$export_status[results$name == "good_export"],
    "EXPORTED"
  )

  # 2. INTERNAL
  expect_equal(
    results$export_status[results$name == "not_exported"],
    "INTERNAL"
  )

  # 3. MISSING_IN_NS (Tag exists, but NS entry missing)
  expect_equal(
    results$export_status[results$name == "forgot_to_document"],
    "MISSING_IN_NS"
  )

  # 4. DETACHED_TAG (Tag exists, but parser sees gap)
  expect_equal(
    results$export_status[results$name == "detached_func"],
    "DETACHED_TAG"
  )

  # 5. EXTRA_IN_NS (NS entry exists, but tag missing)
  expect_equal(
    results$export_status[results$name == "ghost_func"],
    "EXTRA_IN_NS"
  )

  # Cleanup
  fs::dir_delete(root)
})
