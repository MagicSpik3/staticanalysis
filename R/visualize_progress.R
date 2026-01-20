#' Visualize Migration Progress
#'
#' Renders a network graph of the project functions.
#' Red nodes   = Misplaced/Legacy functions (Technical Debt).
#' Green nodes = Refactored/Clean functions.
#' Arrows      = Dependency calls.
#'
#' @param inventory The dataframe from audit_inventory()
#' @param return_dot Logical. If TRUE, returns the DOT code string instead of rendering the graph.
#' @export
visualize_progress <- function(inventory, return_dot = FALSE) {
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("Package 'DiagrammeR' is required for visualization.")
  }

  # 1. Filter: Drop variables, keep only functions
  funcs <- inventory[inventory$type == "function", ]

  if (nrow(funcs) == 0) {
    stop("No functions found in inventory to visualize.")
  }

  # 2. Map Internal Dependencies
  edges_df <- map_internal_calls(inventory)

  # 3. Define Visual Styles
  # Green (Honeydew) for Clean, Red (Mistyrose) for Misplaced
  node_fill <- ifelse(funcs$misplaced, "mistyrose", "honeydew")
  node_border <- ifelse(funcs$misplaced, "red", "darkgreen")

  # Create the Node Data Frame
  nodes <- DiagrammeR::create_node_df(
    n = nrow(funcs),
    label = funcs$name,
    type = "function",
    shape = "oval",
    style = "filled",
    color = node_border,
    fillcolor = node_fill,
    penwidth = 2
  )

  # 4. Create the Edge Data Frame
  # We must map names back to DiagrammeR's internal IDs
  id_map <- setNames(nodes$id, nodes$label)

  if (nrow(edges_df) > 0) {
    graph_edges <- DiagrammeR::create_edge_df(
      from = id_map[edges_df$from],
      to   = id_map[edges_df$to],
      rel  = "calls",
      color = "slategray",
      arrowhead = "vee"
    )
  } else {
    graph_edges <- DiagrammeR::create_edge_df(from = integer(0), to = integer(0))
  }

  # 5. Build Graph
  graph <- DiagrammeR::create_graph() |>
    DiagrammeR::add_node_df(nodes) |>
    DiagrammeR::add_edge_df(graph_edges) |>
    DiagrammeR::add_global_graph_attrs(
      attr = "layout",
      value = "dot",
      attr_type = "graph"
    )

  # 6. Output
  if (return_dot) {
    return(DiagrammeR::generate_dot(graph))
  }

  DiagrammeR::render_graph(graph)
}
