#' Execute Safe Rules
#'
#' Reads a configuration file, validates variable names against a strict schema,
#' and executes the logic if safe.
#'
#' @author Mark London
#'
#' @param file_path String. Path to the CSV/Excel file.
#' @param allowed_vars Character Vector. The "Whitelist" of allowed outputs.
#' @return A list containing the calculated results.
#' @export
execute_safe_rules <- function(file_path, allowed_vars = c("r")) {

  # 1. Read the "Excel" file (Using CSV for simplicity in tests)
  # Expected columns: 'Target', 'Rule'
  if (!file.exists(file_path)) stop("File not found: ", file_path)
  rules <- utils::read.csv(file_path, stringsAsFactors = FALSE)

  # 2. VALIDATION PHASE (The "Pre-Flight Check")
  # Check 1: Are they assigning to a forbidden variable?
  invalid_vars <- setdiff(rules$Target, allowed_vars)

  if (length(invalid_vars) > 0) {
    stop(sprintf("❌ VALIDATION ERROR: Variable '%s' is not allowed. Did you mean '%s'?",
                 invalid_vars[1], allowed_vars[1]))
  }

  # 3. EXECUTION PHASE (The "Compiler")
  results <- list()

  for (i in seq_len(nrow(rules))) {
    target <- rules$Target[i]
    logic  <- rules$Rule[i]

    # Check 2: Safety Check (prevent hacking)
    if (grepl("system|rm|list.files", logic)) {
      stop("❌ SECURITY ALERT: Forbidden function detected in rule.")
    }

    # Execute safely within a local environment (not Global!)
    # We pass 'results' so rules can refer to previous calculations
    tryCatch({
      val <- eval(parse(text = logic), envir = list2env(results))
      results[[target]] <- val
    }, error = function(e) {
      stop(sprintf("❌ MATH ERROR in '%s': %s", target, e$message))
    })
  }

  return(results)
}
