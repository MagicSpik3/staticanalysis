# 1. Unload the current broken namespace
detach("package:rdyntrace", unload = TRUE)

# 2. Load your source code as if it were a package
devtools::load_all("../rdyntrace") # Point to your package folder

# 3. NOW run your command
# Note: When using load_all, you often don't need 'rdyntrace::' prefix,
# but it should work if load_all shimmed it correctly.
# rdyntrace::instrument_globals()
devtools::load_all()

library(staticanalysis)
library(rdyntrace)

# 1. The Legacy Loader (The Soup Kitchen)
# Use the messy source() loop the devs use
path_to_funcs <- r"(D:\git\was-methods\was.methods\R)"
# path_to_funcs <- r"(/home/jonny/R_Code/mypkg/R)"

r_files <- list.files(path_to_funcs, pattern = "\\.R$", full.names = TRUE)

message("Sourcing legacy files...")
# Use local=FALSE to ensure they hit .GlobalEnv
purrr::walk(r_files, source, local = FALSE)

# 2. Static Check (Works on files)
smells <- detect_code_smells(path_to_funcs)
print_smells(smells)

# 3. Dynamic Instrumentation (Works on Globals)
# Hook the functions we just sourced
rdyntrace::instrument_globals()

# 4. Run the Pipeline Trigger
# (Manually call the entry point function)
message("Running Pipeline...")
#paths <- data_paths_FOR_MARK("2210", 9)
#v1df <- create_master_v1(paths)

# 5. Harvest Results
results <- rdyntrace::trace_results()
print(results)
