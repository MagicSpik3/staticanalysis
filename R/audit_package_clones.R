#' Generate Package Clone Report
#' @export
audit_package_clones <- function(pkg_path, output_file = "clone_report.xlsx") {
  message("Scanning: ", pkg_path)

  # Ensure we point to the R directory
  r_path <- if (dir.exists(file.path(pkg_path, "R"))) file.path(pkg_path, "R") else pkg_path

  inv <- get_function_inventory(r_path)
  dupes <- detect_near_dupes(inv)

  # --- THE FIX: Guard against empty duplicates ---
  if (is.null(dupes) || nrow(dupes) == 0) {
    message("✨ No near-duplicates found!")
    dupes <- data.frame(fn_a=character(), fn_b=character(),
                        file_a=character(), file_b=character(),
                        distance=numeric())
    candidates <- data.frame(Function=character(), Duplicate_Count=integer())
  } else {
    # Existing logic for when duplicates DO exist
    candidates <- as.data.frame(table(c(dupes$fn_a, dupes$fn_b)))
    colnames(candidates) <- c("Function", "Duplicate_Count")
    candidates <- candidates[order(-candidates$Duplicate_Count), ]
  }

  report_list <- list(
    Inventory = inv[, c("name", "file", "size")],
    Duplicate_Pairs = dupes,
    Refactor_Candidates = candidates
  )

  writexl::write_xlsx(report_list, output_file)
  message("✅ Analysis complete: ", output_file)
}
