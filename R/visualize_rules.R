#' Visualize Rules from Recipe
#'
#' Generates a flowchart from a compiled Recipe object.
#'
#' @author Mark London
#' @param recipe A 'rule_recipe' object created by compile_rules().
#' @return A DiagrammeR graph object.
#' @export
visualize_rules <- function(recipe) {
  if (!inherits(recipe, "rule_recipe")) stop("Input must be a compiled recipe.")

  # 1. Extract Nodes (Targets) from the Recipe Steps
  nodes <- vapply(recipe, function(x) x$target, character(1))

  # 2. Build Edges (Dependencies)
  edges <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)

  for (step in recipe) {
    # We inspect the AST (step$expression) directly from the recipe
    vars_in_logic <- all.names(step$expression)

    # Find which variables in the logic are actually other Nodes
    inputs <- intersect(vars_in_logic, nodes)

    if (length(inputs) > 0) {
      new_edges <- data.frame(from = inputs, to = step$target, stringsAsFactors = FALSE)
      edges <- rbind(edges, new_edges)
    }
  }

  # 3. Render Graph
  if (nrow(edges) == 0) {
    graph_spec <- paste(unique(nodes), collapse = "; ")
  } else {
    graph_spec <- paste(paste(edges$from, "->", edges$to), collapse = " ")
  }

  DiagrammeR::grViz(sprintf("
    digraph logic_flow {
      graph [rankdir = LR]
      node [shape = box, fontname = Helvetica]
      %s
    }
  ", graph_spec))
}
