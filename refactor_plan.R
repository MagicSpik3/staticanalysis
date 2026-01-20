# ==============================================================================
# REFACTORING BATTLE SCRIPT
# ==============================================================================
# Usage: Rscript refactor_plan.R /path/to/legacy/repo
# ==============================================================================

library(staticanalysis)
library(fs)

args <- commandArgs(trailingOnly = TRUE)
target_repo <- if (length(args) > 0) args[1] else getwd()

message(sprintf("[START] Starting Refactor Analysis on: %s", target_repo))

# ------------------------------------------------------------------------------
# STEP 1: INVENTORY & DUPLICATE DETECTION (The "Cleanup")
# ------------------------------------------------------------------------------
message("\n[SCAN] 1. Scanning for Code Duplicates (Logic Clones)...")

# We look for functions that are identical (ignoring variable names/constants)
dupes <- detect_duplicates(target_repo, ignore_constants = TRUE)

if (!is.null(dupes) && nrow(dupes) > 0) {
  message(sprintf("[WARN]  Found %d duplicate function signatures!", length(unique(dupes$group_id))))
  print(head(dupes, 10))

  # Action: Save this report so you can manually delete the clones later
  write.csv(dupes, file.path(target_repo, "refactor_report_duplicates.csv"))
  message("📄 Report saved: refactor_report_duplicates.csv")
} else {
  message("[OK] No logical duplicates found.")
}

# ------------------------------------------------------------------------------
# STEP 2: DEPENDENCY CHECK (The "Safety Check")
# ------------------------------------------------------------------------------
message("\n📦 2. Checking Dependencies...")
deps <- scan_dependencies(target_repo)

# Check specifically for the 'tidytable' vs 'dplyr' split
tt_count <- deps$usage_stats[deps$usage_stats$package == "tidytable", "count"]
dp_count <- deps$usage_stats[deps$usage_stats$package == "dplyr", "count"]

message(sprintf("📊 Usage Stats: tidytable (%s) vs dplyr (%s)",
                ifelse(length(tt_count), tt_count, 0),
                ifelse(length(dp_count), dp_count, 0)))

# ------------------------------------------------------------------------------
# STEP 3: THE SPLIT (V1 / V2 / V3)
# ------------------------------------------------------------------------------
message("\n[SPLIT]  3. Preparing to Split Monolith...")

# Get the full inventory of functions
inv <- audit_inventory(target_repo)
write.csv(inv, file.path(target_repo, "refactor_inventory.csv"))

message("📄 Full inventory saved: refactor_inventory.csv")
message("ℹ️  INSTRUCTIONS:")
message("   1. Open 'refactor_inventory.csv'")
message("   2. Add a column 'Target_Repo' (V1, V2, V3, or 'Delete')")
message("   3. Run the 'relocate_functions()' tool using that map.")

message("\n[OK] Analysis Complete. You are ready to split.")


# Rscript refactor_plan.R /path/to/legacy/project
