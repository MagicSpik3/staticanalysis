#' Map Internal Function Calls
#'
#' Scans the body of each function in the inventory to find calls to other project functions.
#'
#' @param inventory The dataframe from audit_inventory()
#' @param dir_path String. The project root (required to resolve relative paths in inventory).
#' @return A dataframe of edges (from, to)
#' @author Mark London
#' @export
map_internal_calls <- function(inventory, dir_path = ".") {
  # 1. Filter: We only care about functions
  funcs <- inventory[inventory$type == "function", ]
  edges <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)

  if (nrow(funcs) == 0) return(edges)

  # 2. Iterate through every function definition
  for (i in seq_len(nrow(funcs))) {
    caller_name <- funcs$name[i]

    # FIX: Reconstruct the full path using dir_path
    # Inventory says "R/file.R", we need "../mypkg/R/file.R"
    f_path <- file.path(dir_path, funcs$file[i])

    # Skip if file missing (shouldn't happen if dir_path is correct)
    if (!fs::file_exists(f_path)) next

    # 3. Use Cached AST
    pdata <- get_file_ast(f_path)
    if (is.null(pdata)) next

    # 4. Find the specific function body
    loc <- find_func_lines(pdata, caller_name)
    if (is.null(loc)) next

    # Slice the parse data to just this function's body
    body_data <- pdata[pdata$line1 >= loc$start & pdata$line2 <= loc$end, ]

    # 5. Find all calls inside this body
    # We look for SYMBOL_FUNCTION_CALL tokens
    candidates <- unique(body_data$text[body_data$token == "SYMBOL_FUNCTION_CALL"])

    # Intersect with our known function list to find internal calls
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
