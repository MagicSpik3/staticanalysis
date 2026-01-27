#' Audit Test Integrity
#'
#' Scans test files for "Mocking the System Under Test" anti-patterns.
#' Flags test files that define their own functions instead of importing them.
#'
#' @param dir_path String. Project root.
#' @return A dataframe of suspicious test files.
#' @author Mark London
#' @export
audit_test_integrity <- function(dir_path = ".") {
  test_files <- list_r_files(dir_path, type = "test")
  suspicious <- list()

  for (f in test_files) {
    # Scan for function definitions
    # We reuse your 'scan_definitions' logic but targeted at test files
    defs <- scan_definitions(f, root_dir = dir_path)

    if (!is.null(defs) && nrow(defs) > 0) {
      # Filter: We usually allow tiny helpers, but we flag anything
      # that looks like logic.

      # Heuristic: If a function inside a test file has > 5 lines, it's suspect.
      # (We'd need LOC metrics for that, but for now, just existence is enough)

      for (i in seq_len(nrow(defs))) {
        suspicious[[length(suspicious) + 1]] <- data.frame(
          file = defs$file[i],
          defined_function = defs$name[i],
          reason = "Function defined inside test file (Possible Mock)",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(suspicious) == 0) return(NULL)
  return(do.call(rbind, suspicious))
}
