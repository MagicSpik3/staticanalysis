#' Refactor Misplaced Functions (In Situ)
#'
#' Automatically moves functions into their own files.
#'
#' @param dir_path String. Project root.
#' @param dry_run Logical. If TRUE, reports what would happen.
#' @export
refactor_misplaced <- function(dir_path = ".", dry_run = TRUE) {
  inv <- audit_inventory(dir_path)
  offenders <- inv[inv$misplaced == TRUE, ]

  if (nrow(offenders) == 0) {
    message("No misplaced functions found.")
    return(invisible(NULL))
  }

  files <- unique(offenders$file)

  for (f_rel in files) {
    f_path <- file.path(dir_path, f_rel)
    # Parse ONCE
    pdata <- utils::getParseData(parse(f_path, keep.source = TRUE))

    # Identify moves
    moves <- list()
    for (fn in offenders[offenders$file == f_rel, "name"]) {
      # CALL THE HELPER
      loc <- find_func_lines(pdata, fn)
      if (!is.null(loc)) {
        moves[[fn]] <- list(name = fn, start = loc$start, end = loc$end)
      }
    }

    if (length(moves) == 0) next

    # Sort Bottom-Up
    moves <- moves[order(sapply(moves, function(x) x$start), decreasing = TRUE)]
    lines <- readLines(f_path, warn = FALSE)
    remove_indices <- c()

    for (m in moves) {
      # Capture Roxygen comments above
      curr <- m$start - 1
      comments_start <- m$start
      while (curr > 0) {
        if (grepl("^\\s*#'", lines[curr])) {
          comments_start <- curr
          curr <- curr - 1
        } else {
          break
        }
      }

      extract_lines <- lines[comments_start:m$end]
      new_path <- file.path(dir_path, "R", paste0(m$name, ".R"))

      if (dry_run) {
        message(sprintf("[DRY] Move '%s' to '%s'", m$name, basename(new_path)))
      } else {
        if (!fs::file_exists(new_path)) {
          writeLines(extract_lines, new_path)
          message(sprintf("[OK] Created '%s'", basename(new_path)))
          remove_indices <- c(remove_indices, comments_start:m$end)
        }
      }
    }

    if (!dry_run && length(remove_indices) > 0) {
      lines_remaining <- lines[-remove_indices]
      writeLines(lines_remaining, f_path)
      message(sprintf("[OK] Updated '%s'", f_rel))
    }
  }
}
