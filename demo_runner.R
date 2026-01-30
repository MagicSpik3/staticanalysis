# ==============================================================================
# STATIC ANALYSIS TOOLKIT - DEMO RUNNER
# Target: Legacy Codebase / 'mypkg'
# ==============================================================================

# 1. Setup ---------------------------------------------------------------------
# Run this once to load your toolkit
devtools::load_all(".")
# library(staticanalysis)

# Set the target (Change this to point to your 'bad' package)
target_pkg <- "../mypkg"
# target_pkg <- "D:/git/was-methods/was.methods/R" # For the real legacy demo

cli::cli_h1(paste("Targeting:", basename(target_pkg)))

# ==============================================================================
# PILLAR 1: CODE CORRECTNESS & PERFORMANCE (The "Engineer")
# ==============================================================================
# Unlike linter (style), this finds Logic Errors, Security Risks, and Slowness.

cli::cli_h2("1. Running Deep Code Audit...")

# Detects:
# - Growing Vectors (Performance)
# - Self-Shadowing (Logic)
# - Global Assignment (state)
# - Excel Injection (Security)
smells <- detect_code_smells(target_pkg)

# Print the colorful CLI report
print_smells(smells)

# ==============================================================================
# PILLAR 2: ARCHITECTURE & DEPENDENCIES (The "Map")
# ==============================================================================
# Visualizing the "Spaghetti" to see cascading effects of changes.

cli::cli_h2("2. Visualizing Dependency Graph...")

# 2a. Trace a specific function (The "Impact Analysis")
# "If I change 'is_prime_bad', what breaks?"
# (Replace 'is_prime_bad' with a relevant function from the target)
target_func <- "is_prime_bad"
visualize_callers(target_func, dir_path = target_pkg)

# 2b. Export the Graph for documentation
# visualize_callers(target_func, dir_path = target_pkg, save_dot = "architecture.dot")

# 2c. Audit Library Dependencies (External packages)
cli::cli_h2("Scanning External Dependencies...")
#debugonce(scan_dependencies)
deps <- scan_dependencies(target_pkg)
print(head(deps))

# ==============================================================================
# PILLAR 3: I/O CONTRACTS & SECURITY (The "Gatekeeper")
# ==============================================================================
# Understanding what the code READS and WRITES without running it.

cli::cli_h2("3. Inferring I/O Contracts...")

# 3a. Scan for Hardcoded Paths & Dynamic Sourcing
io_logic <- scan_io_logic(target_pkg)
# Show only interesting IO events (Writes or Path Defs)
if (!is.null(io_logic)) {
  print(io_logic[io_logic$type %in% c("WRITE", "PATH_DEF"), ])
}

# 3b. Generate the "Pre-Flight" Contract
# "What files MUST exist for this pipeline to start?"
contract <- infer_path_contract(target_pkg)
print(contract)

# 3c. (Optional) Run a Mock Pre-Flight Check
# This simulates what happens if we ran the pipeline right now
# paths_list <- list(...) # You would load real paths here
# run_preflight_check(paths_list, contract)

# ==============================================================================
# PILLAR 4: REFACTORING CANDIDATES (The "Janitor")
# ==============================================================================
# Finding Copy-Paste code and misplaced logic.

cli::cli_h2("4. Detecting Duplicated Logic...")

duplicates <- detect_duplicates(target_pkg, threshold = 5) # threshold = lines
if (!is.null(duplicates)) {
  print(duplicates)
} else {
  cli::cli_alert_success("No significant duplication found.")
}

cli::cli_h1("Audit Complete.")
