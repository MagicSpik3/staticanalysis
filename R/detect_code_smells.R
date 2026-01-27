#' @title Detect Code Smells (Orchestrator)
#'
#' Scans the codebase for dangerous patterns and classifies them by severity.
#'
#' @param dir_path String. Path to the project root.
#' @return A dataframe with columns: file, line, id, severity, category, message.
#' @author Mark London
#' @export
detect_code_smells <- function(dir_path = ".") {
  files <- list_r_files(dir_path, "source")

  # 1. Get Inventory (Needed for Scope Checks)
  inv <- scan_definitions(files, root_dir = dir_path)

  smells <- list()

  # 2. Run File-Level Checks
  for (f in files) {
    pdata <- get_file_ast(f)
    if (is.null(pdata)) next

    # Delegate to specialized modules
    smells <- c(smells, list(
      # Existing Checks (Updated to new schema internally)
      check_absolute_paths(pdata, f),
      check_global_assignment(pdata, f),
      check_unsafe_boolean(pdata, f),
      check_unsafe_sequencing(pdata, f),

      # NEW TIER 1 CHECKS (Correctness)
      check_environment_pollution(pdata, f),
      check_reproducibility(pdata, f),
      check_dynamic_execution(pdata, f),
      check_sapply_usage(pdata, f)
    ))
  }

  # 3. Run Function-Level Checks (Scope-aware)
  if (!is.null(inv)) {
    funcs <- inv[inv$type == "function", ]
    if (nrow(funcs) > 0) {
      smells <- c(smells, list(
        check_self_shadowing(funcs, dir_path), # The Ouroboros
        check_library_injection(funcs, dir_path), # Tier 2: Library inside func
        check_base_overwrite(funcs)               # Tier 1: Overwriting 'mean'
      ))
    }
  }

  # 4. Aggregate & return
  smells <- Filter(Negate(is.null), smells)
  if (length(smells) == 0) return(NULL)

  res <- do.call(rbind, smells)
  return(unique(res))
}
