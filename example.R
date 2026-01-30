library(staticanalysis)
library(rdyntrace)

# 1. The Legacy Loader (The Soup Kitchen)
# Use the messy source() loop the devs use
path_to_funcs <- r"(D:\git\was-methods\was.methods\R)"
r_files <- list.files(path_to_funcs, pattern = "\\.R$", full.names = TRUE)

message("Sourcing legacy files...")
# Use local=FALSE to ensure they hit .GlobalEnv
purrr::walk(r_files, source, local = FALSE)

# 2. Static Check (Works on files)
smells <- detect_code_smells(path_to_funcs)
print_smells(smells)

# 3. Dynamic Instrumentation (Works on Globals)
# Hook the functions we just sourced
instrument_globals()

# 4. Run the Pipeline Trigger
# (Manually call the entry point function)
message("Running Pipeline...")
paths <- data_paths_FOR_MARK("2210", 9)
v1df <- create_master_v1(paths)

# 5. Harvest Results
results <- rdyntrace::trace_results()
print(results)
