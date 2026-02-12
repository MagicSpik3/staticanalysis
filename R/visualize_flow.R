#' Visualize Downstream Calls (Forward Dependency)
#'
#' Generates a flow chart starting from a top-level function to see
#' everything it calls, and what they call in turn.
#'
#' @param target_func String. The entry point function.
#' @param dir_path String. Project root.
#' @param save_dot String. Optional path to save the DOT file.
#' @return A DiagrammeR graph object.
#' @export
visualize_flow <- function(target_func, dir_path = ".", save_dot = NULL) {
  # 1. Get Structural Map
  inv <- audit_inventory(dir_path)
  edges <- map_internal_calls(inv, dir_path = dir_path)

  # 2. Recursive Search (BFS) for Downstream logic
  queue <- c(target_func)
  relevant_edges <- data.frame()
  visited <- c()

  while (length(queue) > 0) {
    curr <- queue[1]
    queue <- queue[-1]

    if (curr %in% visited) next
    visited <- c(visited, curr)

    # Find what THIS function calls
    kids <- edges[edges$from == curr, ]
    if (nrow(kids) > 0) {
      relevant_edges <- rbind(relevant_edges, kids)
      queue <- unique(c(queue, kids$to)) # Add children to queue
    }
  }

  if (nrow(relevant_edges) == 0) {
    cli::cli_alert_info("Function '{target_func}' makes no internal calls (Leaf Function).")
    return(NULL)
  }

  # 3. Build Graph (DOT Format)
  dot_code <- "digraph {"
  dot_code <- paste0(dot_code, "\n  graph [rankdir=TB, layout=dot, fontname=Helvetica];") # TB = Top to Bottom
  dot_code <- paste0(dot_code, "\n  node [shape=box, style=filled, fillcolor=honeydew, fontname=Helvetica];")

  # Highlight the Entry Point (The "Top" function)
  dot_code <- paste0(dot_code, sprintf('\n  "%s" [fillcolor=gold, shape=component];', target_func))

  # Add Edges
  for (i in seq_len(nrow(relevant_edges))) {
    dot_code <- paste0(dot_code, sprintf('\n  "%s" -> "%s";', relevant_edges$from[i], relevant_edges$to[i]))
  }

  dot_code <- paste0(dot_code, "\n}")

  # 4. Save/Render
  if (!is.null(save_dot)) {
    writeLines(dot_code, save_dot)
    cli::cli_alert_success("Saved flow chart to {.file {save_dot}}")
  }

  return(DiagrammeR::grViz(dot_code))
}
