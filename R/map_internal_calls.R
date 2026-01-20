#' Map Internal Function Calls
#'
#' Scans the body of each function in the inventory to find calls to other project functions.
#'
#' @param inventory The dataframe from audit_inventory()
#' @return A dataframe of edges (from, to)
#' @noRd
map_internal_calls <- function(inventory) {
  # 1. Filter: We only care about functions
  funcs <- inventory[inventory$type == "function", ]
  edges <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)

  if (nrow(funcs) == 0) return(edges)

  # 2. Iterate through every function definition
  for (i in seq_len(nrow(funcs))) {
    caller_name <- funcs$name[i]
    f_path <- funcs$file[i]

    # Skip if file missing
    if (!fs::file_exists(f_path)) next

    # 3. Parse the file
    pdata <- utils::getParseData(parse(f_path, keep.source = TRUE))

    # 4. Find the specific function body
    # (Relies on your existing R/utils_parser.R helper)
    loc <- find_func_lines(pdata, caller_name)
    if (is.null(loc)) next

    # Slice the parse data to just this function's body
    body_data <- pdata[pdata$line1 >= loc$start & pdata$line2 <= loc$end, ]

    # 5. Find all calls inside this body
    # We look for SYMBOL_FUNCTION_CALL tokens
    candidates <- unique(body_data$text[body_data$token == "SYMBOL_FUNCTION_CALL"])

    # Intersect with our known function list to find internal calls
    # (We don't care about calls to 'base::print' or 'dplyr::filter')
    called_funcs <- intersect(candidates, funcs$name)
    called_funcs <- setdiff(called_funcs, caller_name) # Remove recursion

    if (length(called_funcs) > 0) {
      new_edges <- data.frame(
        from = caller_name,
        to = called_funcs,
        stringsAsFactors = FALSE
      )
      edges <- rbind(edges, new_edges)
    }
  }

  return(edges)
}
