# staticanalysis: Legacy R Modernization Toolkit

**Author:** Mark London  
**License:** MIT

# staticanalysis

**staticanalysis** provides a suite of tools for auditing R project
health, detecting logic duplication, and automating the refactoring of
legacy “monolith” scripts into modern, testable packages.

Unlike standard linters (which check style), `staticanalysis` checks
**structure** and **logic**. It uses robust Abstract Syntax Tree (AST)
parsing to ensure deterministic results, even when file timestamps or
local environments vary.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yourusername/staticanalysis")
```

## Core Workflow

The package is designed to support the **“Strangler Fig”** migration
pattern: identifying legacy code, locking it down with tests, and
extracting it piece by piece.

### 1. Audit Project Health (`audit_inventory`)

Get a high-level view of your project’s structure. This tool flags:

- **Test Coverage:** Which functions lack corresponding unit tests.
- **Misplaced Functions:** Functions defined in files that do not match
  their name (violating the “One Function, One File” rule).

``` r
library(staticanalysis)

# Run a full inventory scan
inv <- audit_inventory(".")

# View functions that need to be moved to their own files
print(inv[inv$misplaced == TRUE, ])
```

### 2. Detect Logic Duplicates (`detect_duplicates`)

Identify “Copy-Paste” programming. This function uses AST token analysis
to find structural clones. It is strictly typed, meaning it
distinguishes between `x + 1` and `x * 1`, but ignores variable names
(`foo` vs `bar`) and constants (optional).

``` r
# Find functions with identical logic signatures
dupes <- detect_duplicates(".")

if (!is.null(dupes)) {
  print(dupes)
}
```

### 3. Automated Refactoring (`refactor_misplaced`)

Automatically clean up “Roommate Functions” (multiple functions living
in a single file). This tool performs a surgical extraction:

1.  Identifies misplaced functions.
2.  Extracts their code and Roxygen documentation (bottom-up to preserve
    line numbers).
3.  Moves them to `R/<function_name>.R`.
4.  Updates the original file.

``` r
# Dry run to see what will happen
refactor_misplaced(dry_run = TRUE)

# Execute the surgery
refactor_misplaced(dry_run = FALSE)
```

### 4. Legacy Migration (`scaffold_migration`)

The primary tool for breaking up monolith scripts. It extracts a
specific function from a legacy script and generates a corresponding
**“Red Light”** test file.

``` r
# 1. Extract 'process_data' from 'old_script.R'
# 2. Create 'R/process_data.R'
# 3. Create 'tests/testthat/test-process_data.R' (Failing by default)
scaffold_migration("process_data", source_file = "legacy/script.R")
```

### 5. Dependency Scanning (`scan_dependencies`)

Find “Ghost Dependencies” (packages used in code but missing from
`DESCRIPTION`) and unused imports.

``` r
deps <- scan_dependencies(".")
print(deps$undeclared_ghosts)
```

## Visualizing Progress

Use
[`visualize_progress()`](https://magicspik3.github.io/staticanalysis/reference/visualize_progress.md)
to generate a network graph showing the decoupling of your project.
Nodes represent functions, and edges represent dependencies or file
membership.

``` r
# Requires DiagrammeR
visualize_progress(inv)
```

## Technical Note: AST Parsing

This package uses a custom geometry-based AST traversal engine
(`find_func_lines`). This ensures that function boundaries are correctly
identified regardless of nesting depth (e.g., functions wrapped in
`tryCatch` or `local`), preventing the common “parser mismatch” errors
found in regex-based tools.

## License

MIT
