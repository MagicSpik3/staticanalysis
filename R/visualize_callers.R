#' Visualize Incoming Calls (Reverse Dependency)
#'
#' Generates a graph showing every function that calls the target function.
#' Can render immediately in RStudio or export a raw .dot file.
#'
#' @param target_func String. Name of the function to trace.
#' @param dir_path String. Project root.
#' @param save_dot String. Optional path to save the raw DOT file (e.g. "graph.dot").
#' @return A DiagrammeR graph object (invisibly if save_dot is used).
#' @author Mark London
#' @export
visualize_callers <- function(target_func, dir_path = ".", save_dot = NULL) {
  # 1. Get Inventory
  inv <- audit_inventory(dir_path)

  # 2. Get Map
  edges <- map_internal_calls(inv, dir_path = dir_path)

  # 3. Filter for calls TO our target
  incoming <- edges[edges$to == target_func, ]

  if (nrow(incoming) == 0) {
    message("Orphan Function: No internal functions call '", target_func, "'")
    return(NULL)
  }

  # 4. Build Graph (DOT Format)
  dot_code <- "digraph {"
  dot_code <- paste0(dot_code, "\n  graph [rankdir=LR, layout=dot, fontname=Helvetica];")
  dot_code <- paste0(dot_code, "\n  node [shape=box, style=filled, fillcolor=lightblue, fontname=Helvetica];")

  # Highlight the Target (Red)
  dot_code <- paste0(dot_code, sprintf('\n  "%s" [fillcolor=salmon, style=filled, shape=doubleoctagon];', target_func))

  # Add Edges
  for (i in seq_len(nrow(incoming))) {
    dot_code <- paste0(dot_code, sprintf('\n  "%s" -> "%s";', incoming$from[i], incoming$to[i]))
  }

  dot_code <- paste0(dot_code, "\n}")

  # 5. Export to File (if requested)
  if (!is.null(save_dot)) {
    writeLines(dot_code, save_dot)
    cli::cli_alert_success("Saved DOT graph to {.file {save_dot}}")
  }

  # 6. Render
  return(DiagrammeR::grViz(dot_code))
}
