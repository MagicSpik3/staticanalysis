# helper_find_usage.R
library(rstudioapi) # Optional, for opening files

find_function_usage <- function(path = ".", func_name) {
  files <- list.files(path, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)

  for (f in files) {
    content <- readLines(f)
    # Basic regex to find function calls "func_name("
    # Note: This is a simple regex. For robust parsing, use `getParseData`.
    matches <- grep(paste0(func_name, "\\("), content)

    if (length(matches) > 0) {
      cat(sprintf("\nFound '%s' in %s on lines: %s", func_name, basename(f), paste(matches, collapse = ", ")))
    }
  }
}

# Usage:
# find_function_usage("R/", "my_date_function")
