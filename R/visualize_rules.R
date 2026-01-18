#' Visualize Rules
#'
#' Reads a configuration file and generates a visual flowchart of the logic.
#'
#' @author Mark London
#'
#' @param file_path String. Path to the CSV/Excel file.
#' @return A DiagrammeR graph object.
#' @export
visualize_rules <- function(file_path) {

  if (!file.exists(file_path)) stop("File not found: ", file_path)
  rules <- utils::read.csv(file_path, stringsAsFactors = FALSE)

  # 1. Build the Node List (Every variable is a node)
  nodes <- unique(rules$Target)

  # 2. Build the Edge List (Who depends on whom?)
  edges <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)

  for (i in seq_len(nrow(rules))) {
    target <- rules$Target[i]
    logic  <- rules$Rule[i]

    # Parse the logic to find input variables
    # We cheat slightly by parsing and looking for symbols
    # "r * 0.2" -> symbols: "r"

    # Extract all symbols from the expression
    # (Safe scan, no eval)
    parsed <- tryCatch(parse(text = logic), error = function(e) NULL)

    if (!is.null(parsed)) {
      # Recursively find symbols in the AST
      vars_in_logic <- all.names(parsed)

      # Filter to keep only things that appear in our 'Target' list
      # (Ignore numbers like '0.2' and functions like '*')
      inputs <- intersect(vars_in_logic, nodes)

      if (length(inputs) > 0) {
        new_edges <- data.frame(from = inputs, to = target, stringsAsFactors = FALSE)
        edges <- rbind(edges, new_edges)
      }
    }
  }

  # 3. Render Graph
  # Using DiagrammeR's simple string spec
  if (nrow(edges) == 0) {
    graph_spec <- paste(nodes, collapse = "; ")
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
