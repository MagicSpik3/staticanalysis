#' Visualize Performance Heatmap
#'
#' Generates a call graph where nodes are colored by their execution time.
#' Red = Slow, Green = Fast, Grey = Unused.
#'
#' @param target_func String. The entry point function to trace (e.g. "nth_prime_bad").
#' @param dynamic_data Dataframe. The output from rdyntrace::trace_results().
#' @param dir_path String. Project root.
#' @return A DiagrammeR graph object.
#' @author Mark London
#' @export
visualize_performance <- function(target_func, dynamic_data, dir_path = ".") {
  # 1. Get Structural Map
  inv <- audit_inventory(dir_path)
  edges <- map_internal_calls(inv, dir_path = dir_path)
  
  # 2. Filter for the relevant subgraph (Downstream calls)
  # We want to see what target_func calls (Forward dependency), not who calls it.
  # So we look for 'from' == target_func, and recurse.
  
  # Simple BFS to find all children
  queue <- c(target_func)
  relevant_edges <- data.frame()
  visited <- c()
  
  while(length(queue) > 0) {
    curr <- queue[1]
    queue <- queue[-1]
    
    if (curr %in% visited) next
    visited <- c(visited, curr)
    
    # Find children
    kids <- edges[edges$from == curr, ]
    if (nrow(kids) > 0) {
      relevant_edges <- rbind(relevant_edges, kids)
      queue <- c(queue, kids$to)
    }
  }
  
  if (nrow(relevant_edges) == 0) {
    message("Function '", target_func, "' makes no internal calls.")
    return(NULL)
  }
  
  # 3. Build the Graph
  nodes <- unique(c(relevant_edges$from, relevant_edges$to))
  
  dot_code <- "digraph { \n  graph [rankdir=LR, layout=dot, fontname=Helvetica];\n"
  
  # 4. Generate Nodes with Colors based on Time
  for (n in nodes) {
    # Lookup time
    stats <- dynamic_data[dynamic_data$func == n, ]
    
    if (nrow(stats) == 0 || stats$calls == 0) {
      # Not called / Dead
      color <- "grey90"
      label <- paste0(n, "\\n(Unused)")
    } else {
      time <- stats$time_sec
      calls <- stats$calls
      
      # Color Logic (Log Scale for heat)
      # You can adjust these thresholds
      if (time > 10) {
        color <- "salmon" # HOT
      } else if (time > 1) {
        color <- "gold"   # WARM
      } else {
        color <- "palegreen" # FAST
      }
      
      label <- sprintf("%s\\n%d calls\\n%.3fs", n, calls, time)
    }
    
    dot_code <- paste0(dot_code, sprintf('  "%s" [shape=box, style=filled, fillcolor="%s", label="%s"];\n', n, color, label))
  }
  
  # Add Edges
  for (i in seq_len(nrow(relevant_edges))) {
    dot_code <- paste0(dot_code, sprintf('  "%s" -> "%s";\n', relevant_edges$from[i], relevant_edges$to[i]))
  }
  
  dot_code <- paste0(dot_code, "}")
  
  DiagrammeR::grViz(dot_code)
}