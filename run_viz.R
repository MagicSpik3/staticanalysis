# --- SETUP -------------------------------------------------------------------
# 1. Point this to your LEGACY project folder (The Monolith)
#    Use "." if you are running this INSIDE the legacy repo.
#    Use "../my_legacy_project" if you are running it from outside.
target_dir <- "../legacy_project_folder_name"

# --- EXECUTION ---------------------------------------------------------------

# 2. Load your new tool
#    If you haven't installed it as a library yet, use load_all()
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".")
} else {
  library(staticanalysis)
}

message(sprintf("🚀 Starting Audit on: %s", fs::path_abs(target_dir)))

# 3. Audit the Inventory
#    (This scans files, coverage, and placement)
inv <- staticanalysis::audit_inventory(target_dir)

message(sprintf("✅ Audit Complete. Found %d functions.", sum(inv$type == "function")))

# 4. Generate the Diagram (Text Mode)
#    return_dot = TRUE prevents it from trying to open a window
dot_text <- staticanalysis::visualize_progress(inv, return_dot = TRUE)

# --- OUTPUT ------------------------------------------------------------------

message("\n📋 Copy the DOT code below this line:\n")
cat("---------------------------------------------------\n")
cat(dot_text)
cat("\n---------------------------------------------------\n")
