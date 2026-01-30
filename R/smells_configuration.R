#' Check for Configuration Placeholders
#'
#' Flags string literals that appear to be manual instructions (e.g., "*specify_here*").
#' These indicate the "Edit-to-Configure" anti-pattern.
#'
#' @noRd
check_placeholders <- function(pdata, file) {
  # Find all string constants
  strings <- pdata[pdata$token == "STR_CONST", ]
  if (nrow(strings) == 0) return(NULL)

  # Remove quotes
  clean_strs <- gsub("^[\"']|[\"']$", "", strings$text)

  # Regex for "Placeholder" patterns:
  # 1. Starts and ends with * (*foo*)
  # 2. Contains "SPECIFY" or "INSERT" or "CHANGE_ME"
  # 3. Contains "<" and ">" (e.g. <date>)

  is_placeholder <- grepl("^\\*.*\\*$", clean_strs) |
    grepl("SPECIFY|INSERT_|CHANGE_ME|TODO", clean_strs, ignore.case = TRUE) |
    grepl("^<.*>$", clean_strs)

  if (any(is_placeholder)) {
    bad <- strings[is_placeholder, ]
    return(data.frame(
      file = file, line = bad$line1,
      id = "CONFIG_PLACEHOLDER",
      severity = "HIGH",
      category = "CONFIGURATION",
      message = paste("Detected manual placeholder:", bad$text, "- Code should not require editing to run. Use arguments or config files."),
      stringsAsFactors = FALSE
    ))
  }
  return(NULL)
}
