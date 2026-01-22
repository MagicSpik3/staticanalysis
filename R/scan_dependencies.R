#' Scan Project Dependencies (Robust AST Method)
#'
#' @description
#' Scans a specified project directory to detect which R packages are actually
#' being used in the code (via `library()`, `::`, or Roxygen tags) and compares
#' them against the `DESCRIPTION` file.
#'
#' This helps identify:
#' \itemize{
#'   \item \strong{Ghosts:} Packages used in the code but missing from `DESCRIPTION`.
#'   \item \strong{Unused:} Packages declared in `DESCRIPTION` but never used in code.
#'   \item \strong{Bloat:} A frequency table of how often each package is called.
#' }
#'
#' @param dir_path A character string specifying the path to the project root.
#'   Must contain R files and optionally a DESCRIPTION file.
#'
#' @return A named list containing:
#' \itemize{
#'   \item \code{usage_stats}: A data frame of package usage counts.
#'   \item \code{undeclared_ghosts}: A character vector of packages used but not listed in DESCRIPTION.
#'   \item \code{unused_declarations}: A character vector of packages listed in DESCRIPTION but not found in code.
#' }
#'
#' @importFrom fs dir_exists dir_ls
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' # 1. Create a temporary "Fake Project" for testing
#' tmp_proj <- tempfile("test_project")
#' dir.create(tmp_proj)
#'
#' # 2. Create a dummy DESCRIPTION file
#' desc_content <- c(
#'   "Package: TestProj",
#'   "Imports: dplyr, fs"
#' )
#' writeLines(desc_content, file.path(tmp_proj, "DESCRIPTION"))
#'
#' # 3. Create a dummy R script
#' script_content <- c(
#'   "library(dplyr)",
#'   "x <- tidyr::pivot_longer(mtcars)"
#' )
#' writeLines(script_content, file.path(tmp_proj, "script.R"))
#'
#' # 4. Run the scanner
#' # (We wrap in if/try in case internal dependencies aren't loaded in this example context)
#' if (requireNamespace("fs", quietly = TRUE)) {
#'   try({
#'     result <- scan_dependencies(tmp_proj)
#'     print(result$undeclared_ghosts)
#'   })
#' }
#'
#' # 5. Cleanup
#' unlink(tmp_proj, recursive = TRUE)
scan_dependencies <- function(dir_path) {
  if (!fs::dir_exists(dir_path)) stop("Directory not found.")

  # 1. READ DECLARED PACKAGES
  desc_path <- file.path(dir_path, "DESCRIPTION")
  declared_pkgs <- character()
  current_pkg_name <- ""

  if (file.exists(desc_path)) {
    # CORRECTED LINE: read.dcf is in base, not utils
    dcf <- read.dcf(desc_path)

    if ("Package" %in% colnames(dcf)) current_pkg_name <- dcf[1, "Package"]

    for (field in c("Imports", "Depends", "Suggests")) {
      if (field %in% colnames(dcf)) {
        deps <- strsplit(dcf[1, field], ",\n?")[[1]]
        clean_deps <- gsub("\\s*\\(.*\\)", "", deps)
        declared_pkgs <- c(declared_pkgs, trimws(clean_deps))
      }
    }
    declared_pkgs <- setdiff(declared_pkgs, "R")
  }

  # 2. FIND USAGE (AST + Regex)
  r_files <- fs::dir_ls(dir_path, recurse = TRUE, glob = "*.R")
  detected_pkgs <- character()

  for (f in r_files) {
    # Method A: Robust AST Walker
    exprs <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
    if (!is.null(exprs)) {
      # Ensure 'walk_ast_for_pkgs' is available internally
      detected_pkgs <- c(detected_pkgs, walk_ast_for_pkgs(exprs))
    }

    # Method B: Regex (Roxygen only)
    lines <- readLines(f, warn = FALSE)
    rox_matches <- regmatches(lines, regexec("@importFrom\\s+([a-zA-Z0-9\\.]+)", lines))
    detected_pkgs <- c(detected_pkgs, vapply(rox_matches, function(x) {
      if (length(x) > 1) x[2] else NA_character_
    }, character(1)))

    rox_import <- regmatches(lines, regexec("@import\\s+([a-zA-Z0-9\\.]+)", lines))
    detected_pkgs <- c(detected_pkgs, vapply(rox_import, function(x) {
      if (length(x) > 1) x[2] else NA_character_
    }, character(1)))
  }

  # 3. CLEANUP & COUNTING
  clean_detected <- stats::na.omit(detected_pkgs)
  ignored_set <- c(
    "base", "stats", "utils", "methods", "graphics", "grDevices", "datasets", "",
    current_pkg_name
  )
  valid_usage <- clean_detected[!clean_detected %in% ignored_set]

  # Create Frequency Table
  usage_table <- as.data.frame(table(valid_usage), stringsAsFactors = FALSE)
  colnames(usage_table) <- c("package", "count")

  # 4. CLASSIFY
  used_set <- unique(valid_usage)
  ghosts <- setdiff(used_set, declared_pkgs)
  unused <- setdiff(declared_pkgs, used_set)

  return(list(
    usage_stats = usage_table[order(-usage_table$count), ],
    undeclared_ghosts = sort(ghosts),
    unused_declarations = sort(unused)
  ))
}
