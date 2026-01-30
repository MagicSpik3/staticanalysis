#' Infer Path Contract
#'
#' Scans the code to determine which variables are treated as Inputs (Read)
#' and which are treated as Outputs (Write).
#'
#' @param dir_path String. Project root.
#' @param config_var String. The name of your paths list variable (default "paths").
#' @return A dataframe describing the contract (Input/Output) for each key.
#' @author Mark London
#' @export
infer_path_contract <- function(dir_path = ".", config_var = "paths") {
  # Reuse the io_logic scanner we built previously
  # (Assuming scan_io_logic returns a dataframe with 'type' and 'expression')
  io_events <- scan_io_logic(dir_path)

  if (is.null(io_events)) return(NULL)

  contract <- list()

  # Regex to find usages of the config variable (e.g., "paths$person")
  # Matches: paths$key OR paths[["key"]]
  regex_dollar <- paste0("^", config_var, "\\$([a-zA-Z0-9_]+)$")
  regex_bracket <- paste0("^", config_var, "\\[\\[['\"]([a-zA-Z0-9_]+)['\"]]\\]$")

  for (i in seq_len(nrow(io_events))) {
    expr <- io_events$expression[i]
    type <- io_events$type # READ or WRITE
    key <- NA

    # Check for paths$key
    if (grepl(regex_dollar, expr)) {
      key <- sub(regex_dollar, "\\1", expr)
    }
    # Check for paths[["key"]]
    else if (grepl(regex_bracket, expr)) {
      key <- sub(regex_bracket, "\\1", expr)
    }

    if (!is.na(key)) {
      contract[[length(contract) + 1]] <- data.frame(
        key = key,
        role = ifelse(type == "READ", "INPUT", "OUTPUT"),
        evidence = paste(io_events$func[i], "in", basename(io_events$file[i])),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(contract) == 0) return(NULL)

  # Aggregation: A key might be both (Read then Written), but usually Role is singular.
  # We prioritize OUTPUT (if it's written, we don't strictly require it to exist at start,
  # unless it's read *before* write, which is complex. Let's assume WRITE = OUTPUT).

  df <- do.call(rbind, contract)

  # Deduplicate logic: If a key appears as both, flag it?
  # For simple Pre-Flight, let's just list all requirements.
  return(unique(df))
}
