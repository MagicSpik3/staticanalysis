#' Migrate Namespace Usage
#'
#' Automatically refactors code to replace one package dependency with another.
#' Useful for migrating from 'tidytable' to 'dplyr' or vice versa.
#'
#' @param dir_path String. Path to the project root.
#' @param from_pkg String. The package to remove (e.g., "tidytable").
#' @param to_pkg String. The package to replace it with (e.g., "dplyr").
#' @param dry_run Logical. If TRUE, only shows what would change without writing files.
#' @export
migrate_namespace <- function(dir_path, from_pkg = "tidytable", to_pkg = "dplyr", dry_run = TRUE) {

  files <- fs::dir_ls(dir_path, recurse = TRUE, glob = "*.R")

  for (f in files) {
    old_code <- readLines(f, warn = FALSE)
    new_code <- old_code
    changed <- FALSE

    # 1. Replace explicit calls: pkg::func -> new::func
    # We use a simple regex here because AST replacement is complex for 670 files,
    # and namespace calls are usually consistent.
    pattern_explicit <- paste0(from_pkg, "::")
    replace_explicit <- paste0(to_pkg, "::")

    if (any(grepl(pattern_explicit, new_code))) {
      new_code <- gsub(pattern_explicit, replace_explicit, new_code)
      changed <- TRUE
    }

    # 2. Replace library calls: library(pkg) -> library(new)
    # We handle both quote styles
    pattern_lib <- paste0("library\\(['\"]?", from_pkg, "['\"]?\\)")
    replace_lib <- paste0("library(", to_pkg, ")")

    if (any(grepl(pattern_lib, new_code))) {
      new_code <- gsub(pattern_lib, replace_lib, new_code)
      changed <- TRUE
    }

    # 3. Report or Write
    if (changed) {
      if (dry_run) {
        message(sprintf("📝 [DRY RUN] Would modify: %s", fs::path_rel(f, start = dir_path)))
      } else {
        writeLines(new_code, f)
        message(sprintf("✅ Refactored: %s", fs::path_rel(f, start = dir_path)))
      }
    }
  }
}
