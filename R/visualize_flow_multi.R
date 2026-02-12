#' Visualize Multi-Package Downstream Flow
#'
#' @param target_func String. Entry point.
#' @param project_path String. Path to the main project (e.g., was.controller).
#' @param cross_packages Character vector. Names of your other packages (e.g., c("was.utils", "was.methods")).
#' @export
visualize_flow_multi <- function(target_func, project_path = ".", cross_packages = NULL, save_dot = NULL) {

  # 1. Build a "Known Universe" of your functions
  inv <- audit_inventory(project_path)

  # Add exports from your other packages to the 'internal' list
  # This tells the mapper: "If you see these names, they aren't 'external' noise."
  extra_funs <- unlist(lapply(cross_packages, function(p) {
    if (requireNamespace(p, quietly = TRUE)) getNamespaceExports(p) else NULL
  }))

  # Create a dummy inventory for the external packages so the mapper tracks them
  if (length(extra_funs) > 0) {
    extra_inv <- data.frame(
      name = extra_funs,
      type = "function",
      file = "external_package",
      stringsAsFactors = FALSE
    )
    # Combine with local inventory
    inv <- rbind(inv[, c("name", "type", "file")], extra_inv)
  }

  # 2. Map the calls (The mapper will now flag your cross-package functions)
  edges <- map_internal_calls(inv, dir_path = project_path)

  # 3. BFS Recursive Discovery
  queue <- c(target_func)
  relevant_edges <- data.frame()
  visited <- c()

  while (length(queue) > 0) {
    curr <- queue[1]
    queue <- queue[-1]
    if (curr %in% visited) next
    visited <- c(visited, curr)

    kids <- edges[edges$from == curr, ]
    if (nrow(kids) > 0) {
      relevant_edges <- rbind(relevant_edges, kids)
      queue <- unique(c(queue, kids$to))
    }
  }

  # 4. DOT Generation with Package-Specific Coloring
  dot_code <- "digraph { \n  graph [rankdir=TB, layout=dot, fontname=Helvetica];\n"

  # Get all nodes in the discovered flow
  nodes <- unique(c(relevant_edges$from, relevant_edges$to))

  for (n in nodes) {
    # Determine which package/file the node belongs to for styling
    origin <- inv$file[inv$name == n][1]

    style <- if (n == target_func) {
      'fillcolor=gold, shape=component' # Entry Point
    } else if (origin == "external_package") {
      'fillcolor=lightgray, style=dashed' # Functions from utils/methods
    } else {
      'fillcolor=honeydew, shape=box' # Local controller functions
    }

    dot_code <- paste0(dot_code, sprintf('  "%s" [%s];\n', n, style))
  }

  for (i in seq_len(nrow(relevant_edges))) {
    dot_code <- paste0(dot_code, sprintf('  "%s" -> "%s";\n', relevant_edges$from[i], relevant_edges$to[i]))
  }

  dot_code <- paste0(dot_code, "}")

  if (!is.null(save_dot)) writeLines(dot_code, save_dot)
  return(DiagrammeR::grViz(dot_code))
}
