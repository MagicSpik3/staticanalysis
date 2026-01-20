# runner
# This loads all your functions into memory immediately
usethis::use_package("fs")      # For file system handling
usethis::use_package("dplyr")   # For counting frequencies
usethis::use_package("purrr")   # For iteration
usethis::use_package("diffobj")
usethis::proj_sitrep()
usethis::use_package("rlang")
usethis::use_package("devtools", type = "Suggests")
usethis::use_package("here", type = "Suggests")
usethis::use_package("lintr", type = "Suggests")
usethis::use_package("styler", type = "Suggests")

usethis::use_build_ignore(".lintr")
usethis::use_build_ignore("runner.R")
usethis::use_build_ignore("test.R")

# 1. Promote 'rlang' to a real dependency (Required for .data)
usethis::use_package("rlang")

# 2. Move Dev Tools to "Suggests" (Optional, for development only)
# This stops R from installing them on the production server
usethis::use_package("devtools", type = "Suggests")
usethis::use_package("here", type = "Suggests")
usethis::use_package("lintr", type = "Suggests")
usethis::use_package("styler", type = "Suggests")
usethis::use_package("usethis", type = "Suggests")

# 3. Add 'diffobj' and 'xmlparsedata' (Required for the Audit tool we are about to build)
usethis::use_package("diffobj", type = "Suggests")
usethis::use_package("xmlparsedata", type = "Suggests")
usethis::use_build_ignore(c("refactor_plan.R", "self_test.R", "update_pipeline.bat"))


devtools::load_all()



testthat::test_file("tests/testthat/test-detect_duplicates.R")
testthat::test_file("tests/testthat/test-audit_formula_functions.R")



# 1. Remove the old corrupted installation
remove.packages("staticanalysis")

# 2. Load the source code afresh
devtools::load_all()

# 3. Document (updates NAMESPACE)
devtools::document()

# 4. Check
library(staticanalysis)
devtools::check()
devtools::test()
styler::style_pkg()
lintr::lint_package()
library(staticanalysis)
here::dr_here()
#devtools::check()
ls("package:staticanalysis")
list.files("tests/testthat", recursive = TRUE)

report <- scan_dependencies(".")
print(report$undeclared_ghosts)


# show the tre
fs::dir_tree(".", recurse = TRUE)

#devtools::load_all()
#sapply(ls("package:staticanalysis"), get)


#g <- visualize_rules("tests/testthat/artifacts/accountant_rules.csv")
#print(g)


package_overview <- function() {
  cat("Package root:", usethis::proj_get(), "\n\n")

  cat("R functions:\n")
  print(list.files("R", pattern = "\\.R$", full.names = TRUE))
  cat("\n")

  cat("Tests:\n")
  print(list.files("tests/testthat", pattern = "\\.R$", full.names = TRUE))
  cat("\n")

  devtools::load_all(quiet = TRUE)
  cat("Exported functions:\n")
  print(getNamespaceExports("staticanalysis"))
}


package_overview()

#devtools::load_all() # Load new functions
# ==============================================================================
# STATIC ANALYSIS RUNNER
# ==============================================================================
# Usage:
#   Rscript runner.R                   (Runs on current directory)
#   Rscript runner.R /path/to/repo     (Runs on specific repo)
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
target_dir <- if (length(args) > 0) args[1] else getwd()

message(sprintf("[START] Starting Analysis on: %s", target_dir))

# Check if the package is installed; if not, try to load local source
if (!requireNamespace("staticanalysis", quietly = TRUE)) {
  message("[WARN] Package 'staticanalysis' not installed globally. Loading from source...")
  # Assumes runner.R is inside the staticanalysis repo
  devtools::load_all(".")
} else {
  library(staticanalysis)
}

# ------------------------------------------------------------------------------
# 1. Project Inventory & Test Coverage
# ------------------------------------------------------------------------------
message("\n📊 GENERATING INVENTORY...")
tryCatch({
  inventory <- staticanalysis::audit_inventory(target_dir)

  if (!is.null(inventory)) {
    print(inventory)

    # Calculate coverage metric
    funcs_only <- inventory[inventory$type == "function", ]
    coverage <- mean(funcs_only$called_in_test) * 100
    message(sprintf("\nTest Coverage (Reference Check): %.1f%% of functions mentioned in tests.", coverage))
  } else {
    message("No functions or variables found.")
  }
}, error = function(e) message("Skipped Inventory: ", e$message))

# ------------------------------------------------------------------------------
# 2. Dependency Scan
# ------------------------------------------------------------------------------
message("\n📦 SCANNING DEPENDENCIES...")
tryCatch({
  deps <- staticanalysis::scan_dependencies(target_dir)

  if (length(deps$undeclared_ghosts) > 0) {
    message("[ERROR] GHOST DEPENDENCIES FOUND (Used but not in DESCRIPTION):")
    print(deps$undeclared_ghosts)
  } else {
    message("[OK] No ghost dependencies found.")
  }

  if (length(deps$usage_stats) > 0) {
    message("\nTop Used Packages:")
    print(head(deps$usage_stats, 5))
  }
}, error = function(e) message("Skipped Dependencies: ", e$message))

# ------------------------------------------------------------------------------
# 3. Rule Compilation (If CSVs exist)
# ------------------------------------------------------------------------------
csv_files <- list.files(target_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
if (length(csv_files) > 0) {
  message(sprintf("\n📜 Found %d rule files (CSVs). Attempting compile...", length(csv_files)))
  # Just trying the first one as a smoke test
  tryCatch({
    recipe <- staticanalysis::compile_rules(csv_files[1], allowed_vars = NULL)
    message("[OK] Successfully compiled first rule file.")
  }, error = function(e) message("[WARN] Could not compile rules: ", e$message))
}

message("\n[OK] Analysis Complete.")


# # In your terminal at work:
#Rscript runner.R /home/work/repos/BigProject/SubPackageA
#Rscript runner.R /home/work/repos/BigProject/SubPackageB

