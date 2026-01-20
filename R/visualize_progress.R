#' Visualize Migration Progress
#'
#' Draws a network graph showing the decoupling of the legacy monolith.
#'
#' @param inventory A dataframe from audit_inventory().
#' @export
visualize_progress <- function(inventory) {
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("DiagrammeR is required for visualization.")
  }

  # 1. Classify Status
  inventory$status <- "legacy"

  # "Migrated" means it lives in a file that matches its name (approx)
  # e.g., function 'clean_data' in 'R/clean_data.R'
  is_migrated <- mapply(
    function(n, f) grepl(n, f, fixed = TRUE),
    inventory$name, inventory$file
  )

  inventory$status[is_migrated] <- "migrated_untested"
  inventory$status[is_migrated & inventory$called_in_test] <- "complete"

  # 2. Build Graph Data
  # We group by "File" to show clusters breaking away
  files <- unique(inventory$file)
  nodes <- data.frame(
    id = seq_along(files),
    label = files,
    shape = ifelse(grepl("legacy|script", files), "database", "note"),
    color = "lightblue",
    stringsAsFactors = FALSE
  )

  # Color nodes based on completeness
  DiagrammeR::create_graph() |>
    DiagrammeR::add_node_df(nodes) |>
    DiagrammeR::render_graph()
}
