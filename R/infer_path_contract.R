#' Infer Path Contract
#' @export
infer_path_contract <- function(dir_path = ".", config_var = "paths") {
  # USE THE NEW UNIFIED SCANNER
  io_events <- scan_project_io(dir_path)

  if (is.null(io_events)) return(NULL)

  contract <- list()

  # Regex for paths$key OR paths[["key"]]
  regex_dollar <- paste0("^", config_var, "\\$([a-zA-Z0-9_]+)$")
  regex_bracket <- paste0("^", config_var, "\\[\\[['\"]([a-zA-Z0-9_]+)['\"]]\\]$")

  for (i in seq_len(nrow(io_events))) {
    expr <- io_events$arg_text[i] # Use arg_text
    role <- ifelse(io_events$type[i] == "READ", "INPUT", "OUTPUT")
    key <- NA

    if (grepl(regex_dollar, expr)) {
      key <- sub(regex_dollar, "\\1", expr)
    } else if (grepl(regex_bracket, expr)) {
      key <- sub(regex_bracket, "\\1", expr)
    }

    if (!is.na(key)) {
      contract[[length(contract) + 1]] <- data.frame(
        key = key,
        role = role,
        evidence = paste(io_events$func[i], "in", basename(io_events$file[i])),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(contract) == 0) return(NULL)

  # Deduping: We might find the same key twice.
  # If a key is BOTH Input and Output, we default to OUTPUT (it's being written).
  df <- do.call(rbind, contract)

  # Smart Aggregation
  df <- unique(df) # Remove exact duplicates

  # Handle "Mixed Role" conflict:
  # If key "X" has INPUT and OUTPUT rows, remove the INPUT row.
  keys <- unique(df$key)
  clean_rows <- list()

  for (k in keys) {
    rows <- df[df$key == k, ]
    if (nrow(rows) > 1 && "OUTPUT" %in% rows$role) {
      # Keep only OUTPUT
      clean_rows[[length(clean_rows) + 1]] <- rows[rows$role == "OUTPUT", ]
    } else {
      clean_rows[[length(clean_rows) + 1]] <- rows
    }
  }

  return(do.call(rbind, clean_rows))
}
