# Scan Project Dependencies (Robust AST Method)

Scans a specified project directory to detect which R packages are
actually being used in the code (via
[`library()`](https://rdrr.io/r/base/library.html), `::`, or Roxygen
tags) and compares them against the `DESCRIPTION` file.

This helps identify:

- **Ghosts:** Packages used in the code but missing from `DESCRIPTION`.

- **Unused:** Packages declared in `DESCRIPTION` but never used in code.

- **Bloat:** A frequency table of how often each package is called.

## Usage

``` r
scan_dependencies(dir_path)
```

## Arguments

- dir_path:

  A character string specifying the path to the project root. Must
  contain R files and optionally a DESCRIPTION file.

## Value

A named list containing:

- `usage_stats`: A data frame of package usage counts.

- `undeclared_ghosts`: A character vector of packages used but not
  listed in DESCRIPTION.

- `unused_declarations`: A character vector of packages listed in
  DESCRIPTION but not found in code.

## Examples

``` r
# 1. Create a temporary "Fake Project" for testing
tmp_proj <- tempfile("test_project")
dir.create(tmp_proj)

# 2. Create a dummy DESCRIPTION file
desc_content <- c(
  "Package: TestProj",
  "Imports: dplyr, fs"
)
writeLines(desc_content, file.path(tmp_proj, "DESCRIPTION"))

# 3. Create a dummy R script
script_content <- c(
  "library(dplyr)",
  "x <- tidyr::pivot_longer(mtcars)"
)
writeLines(script_content, file.path(tmp_proj, "script.R"))

# 4. Run the scanner
# (We wrap in if/try in case internal dependencies aren't loaded in this example context)
if (requireNamespace("fs", quietly = TRUE)) {
  try({
    result <- scan_dependencies(tmp_proj)
    print(result$undeclared_ghosts)
  })
}
#> [1] "tidyr"

# 5. Cleanup
unlink(tmp_proj, recursive = TRUE)
```
