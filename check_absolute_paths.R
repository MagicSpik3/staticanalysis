#' @title check_absolute_paths
#' @noRd
check_absolute_paths <- function(pdata, file) {
  strings <- pdata[pdata$token == "STR_CONST", ]
  if (nrow(strings) == 0) return(NULL)

  clean_strs <- gsub("^[\"']|[\"']$", "", strings$text)

  # Logic: 4 Backslashes (UNC) or Drive Letter (C:/)
  # Uses startsWith to avoid Regex confusion
  is_unc <- startsWith(clean_strs, "\\\\\\\\") | startsWith(clean_strs, "//")
  is_drive <- nchar(clean_strs) >= 3 & grepl("^[a-zA-Z]:[\\\\/]", clean_strs)

  if (any(is_unc | is_drive)) {
    bad_strs <- strings[is_unc | is_drive, ]
    return(data.frame(
      file = file, line = bad_strs$line1,
      type = "ABSOLUTE_PATH",
      evidence = bad_strs$text,
      message = "Hardcoded absolute path detected. Use config or relative paths.",
      stringsAsFactors = FALSE
    ))
  }
  return(NULL)
}
