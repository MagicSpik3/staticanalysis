#' Validate File I/O Requirements (Pre-Flight Check)
#'
#' Takes the output of scan_file_io() and checks if the files actually exist
#' on disk, using a provided context map for variables.
#'
#' @param io_report Dataframe. Output from scan_file_io().
#' @param context_vars Named List. Mapping of variable names to real paths.
#'        Example: list(target_var = "D:/test_dir", base_dir = "/tmp")
#' @return Logical. TRUE if all clear, FALSE if missing files.
#' @author Mark London
#' @export
validate_file_io <- function(io_report, context_vars = list()) {
  if (is.null(io_report)) return(TRUE)

  missing_files <- character()

  cli::cli_h1("Pre-Flight File Check")

  for (i in seq_len(nrow(io_report))) {
    row <- io_report[i, ]
    full_path <- NA

    # 1. Resolve Path
    if (row$type == "literal") {
      full_path <- row$path_pattern
    } else if (row$type == "constructed") {
      # Check if we have the root variable in our context
      if (!is.na(row$root_var) && row$root_var %in% names(context_vars)) {
        root <- context_vars[[row$root_var]]
        full_path <- file.path(root, row$path_pattern)
      } else {
        # We can't resolve it, so we skip validation (or warn)
        # cli::cli_alert_warning("Skipping check for '{row$root_var}' (Variable not provided)")
        next
      }
    } else {
      next # Skip pure variables
    }

    # 2. Check Existence
    if (!is.na(full_path)) {
      if (!file.exists(full_path)) {
        missing_files <- c(missing_files, full_path)
        cli::cli_alert_danger("MISSING: {.file {full_path}}")
        cli::cli_text("  Captured in: {.code {row$func}} inside {.file {row$file}}")
      } else {
        # Optional: Print success for debugging
        # cli::cli_alert_success("Found: {full_path}")
      }
    }
  }

  if (length(missing_files) > 0) {
    cli::cli_alert_danger("Pre-flight check FAILED. {length(missing_files)} files missing.")
    return(FALSE)
  } else {
    cli::cli_alert_success("All checkable files present. Cleared for takeoff.")
    return(TRUE)
  }
}
