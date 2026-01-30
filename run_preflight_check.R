#' Run Pre-Flight File Check
#'
#' Validates a specific list of paths against the Usage Contract.
#' Checks that INPUTS exist and OUTPUT parents exist.
#'
#' @param paths_list List. The actual list of paths (e.g. from data_paths()).
#' @param contract Dataframe. Output from infer_path_contract().
#' @return Logical TRUE if passed, FALSE if failed.
#' @author Mark London
#' @export
run_preflight_check <- function(paths_list, contract) {

  if (is.null(contract)) {
    cli::cli_alert_warning("No contract provided. Skipping checks.")
    return(TRUE)
  }

  errors <- 0

  cli::cli_h1("Pre-Flight System Check")

  # 1. Check INPUTS (Must Exist)
  inputs <- contract[contract$role == "INPUT", ]
  if (nrow(inputs) > 0) {
    cli::cli_h2("Checking Inputs (Must Exist)")
    for (i in seq_len(nrow(inputs))) {
      key <- inputs$key[i]

      if (!key %in% names(paths_list)) {
        cli::cli_alert_warning("Key '{key}' found in code but missing from paths list.")
        next
      }

      path <- paths_list[[key]]

      # Handle dynamic/wildcard paths if necessary (Simple check first)
      if (!file.exists(path)) {
        cli::cli_alert_danger("MISSING INPUT: {.file {path}}")
        cli::cli_text("  Used by: {inputs$evidence[i]}")
        errors <- errors + 1
      } else {
        cli::cli_alert_success("Found: {key}")
      }
    }
  }

  # 2. Check OUTPUTS (Parent Must Exist, Path should NOT contain placeholders)
  outputs <- contract[contract$role == "OUTPUT", ]
  if (nrow(outputs) > 0) {
    cli::cli_h2("Checking Outputs (Configuration)")
    for (i in seq_len(nrow(outputs))) {
      key <- outputs$key[i]
      if (!key %in% names(paths_list)) next
      path <- paths_list[[key]]

      # Check 1: Time Bombs
      if (grepl("\\*specify", path)) {
        cli::cli_alert_danger("CONFIG ERROR: Placeholder found in output '{key}'")
        errors <- errors + 1
      }

      # Check 2: Directory Existence
      parent <- dirname(path)
      if (!dir.exists(parent)) {
        cli::cli_alert_danger("MISSING DIR: Parent for output '{key}' does not exist.")
        cli::cli_text("  Path: {.file {parent}}")
        errors <- errors + 1
      } else {
        cli::cli_alert_success("Ready to Write: {key}")
      }
    }
  }

  if (errors > 0) {
    cli::cli_h1("Pre-Flight FAILED")
    cli::cli_alert_danger("{errors} blocking issues detected.")
    return(FALSE)
  } else {
    cli::cli_h1("Pre-Flight PASSED")
    return(TRUE)
  }
}
