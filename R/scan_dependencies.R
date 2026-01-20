#' Scan Project Dependencies (Robust AST Method)
#'
#' Scans all R scripts for package usage.
#' Returns usage counts to help identify "bloated" single-use imports.
#'
#' @param dir_path String. Path to the project root.
#' @return A list containing 'usage_stats', 'ghosts', and 'unused'.
#' @export
scan_dependencies <- function(dir_path) {
  if (!fs::dir_exists(dir_path)) stop("Directory not found.")

  # 1. READ DECLARED PACKAGES
  desc_path <- file.path(dir_path, "DESCRIPTION")
  declared_pkgs <- character()
  current_pkg_name <- ""

  if (file.exists(desc_path)) {
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
      detected_pkgs <- c(detected_pkgs, walk_ast_for_pkgs(exprs))
    }

    # Method B: Regex (Roxygen only)
    lines <- readLines(f, warn = FALSE)
    rox_matches <- regmatches(lines, regexec("@importFrom\\s+([a-zA-Z0-9\\.]+)", lines))
    detected_pkgs <- c(detected_pkgs, vapply(rox_matches, function(x) {
      if (length(x) > 1) {
        x[2]
      } else {
        NA_character_
      }
    }, character(1)))

    rox_import <- regmatches(lines, regexec("@import\\s+([a-zA-Z0-9\\.]+)", lines))
    detected_pkgs <- c(detected_pkgs, vapply(rox_import, function(x) {
      if (length(x) > 1) {
        x[2]
      } else {
        NA_character_
      }
    }, character(1)))
  }

  # 3. CLEANUP & COUNTING
  clean_detected <- stats::na.omit(detected_pkgs)
  ignored_set <- c(
    "base", "stats", "utils", "methods", "graphics", "grDevices", "datasets", "",
    current_pkg_name
  )
  valid_usage <- clean_detected[!clean_detected %in% ignored_set]

  # Create Frequency Table (The "Banana" Report)
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

