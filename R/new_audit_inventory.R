#' Constructor: Audit Inventory Object
#'
#' Validates and creates a standardized audit inventory object.
#' Enforces the schema to prevent silent column errors (The "Global Coupling" fix).
#'
#' @param df Dataframe containing the raw inventory scan.
#' @return An object of class 'audit_inventory'.
#' @author Mark London
#' @export
new_audit_inventory <- function(df) {
  # 1. Define Schema
  required_cols <- c("name", "type", "file")

  # 2. Validation
  if (is.null(df)) {
    return(NULL)
  }

  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(
      "Invalid Inventory Schema. Missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # 3. Standardization (Ensure optional columns exist)
  optional_cols <- c("misplaced", "called_in_test", "export_status", "signature")
  for (col in optional_cols) {
    if (!col %in% colnames(df)) df[[col]] <- NA
  }

  # 4. Class Tagging
  structure(df, class = c("audit_inventory", "data.frame"))
}
