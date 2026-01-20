# 1. Load the latest version of your tools
devtools::load_all()
library(staticanalysis)

# 2. Define target as the current project root
target <- "."

# --- TEST 1: INVENTORY ---
message("[SCAN] Running Inventory on Self...")
inv <- audit_inventory(target)
# You should see your new modular functions here (scan_definitions, etc.)
print(inv[inv$type == "function", c("name", "file")])

# --- TEST 2: DEPENDENCIES ---
message("\n📦 Running Dependency Scan on Self...")
deps <- scan_dependencies(target)
# Expectation: 'testthat', 'fs', 'utils' should be used.
# 'tidytable' should be GONE (except in that one mock test).
print(deps$usage_stats)
print(paste("Ghosts:", paste(deps$undeclared_ghosts, collapse=", ")))

# --- TEST 3: DUPLICATES ---
message("\n👯 Checking for Code Duplication...")
dupes <- detect_duplicates(target)
if (!is.null(dupes)) {
  print(dupes)
} else {
  message("No duplicates found (Clean Code!).")
}

