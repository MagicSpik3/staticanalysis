# 1. Load the Excel file
patches <- readxl::read_excel("corrections.xlsx")
patch_code <- patches$R_correction

# 2. Load the "Skeleton" (Metadata only)
# You don't need the real data, just the names!
# read.csv(..., nrows=1) is fast.
schema_names <- names(read.csv(paths$masterv1, nrows = 1))

# 3. Validate
report <- validate_patch_log(patch_code, schema_names)

# 4. Check results
if (any(report$status == "FAIL")) {
  print(report[report$status == "FAIL", ])
  stop("Validation Failed! Please fix the Excel sheet before proceeding.")
} else {
  message("All patches valid. Starting 3-day pipeline...")
}
