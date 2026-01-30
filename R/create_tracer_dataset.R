#' Create Tracer Dataset (The Micro-Verse)
#'
#' Reads the full raw data, filters it down to a specific set of IDs,
#' and saves mini-versions to a temporary directory.
#'
#' @param target_ids Vector. The IDs to "hand carry" (e.g., c("HAS18107...", "HAS18108...")).
#' @param paths_list List. Your current data_paths list.
#' @param output_dir String. Where to save the micro-files.
#' @export
create_tracer_dataset <- function(target_ids, paths_list, output_dir = "test_data/TRACER") {

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  cli::cli_h1("Creating Tracer Environment")
  cli::cli_alert_info("Target IDs: {paste(target_ids, collapse=', ')}")

  # Helper to filter and save
  process_file <- function(key, path) {
    if (!file.exists(path)) return()

    # 1. Read (Smart Read based on extension)
    ext <- tools::file_ext(path)
    data <- NULL

    try({
      if (ext == "SAV") data <- haven::read_sav(path)
      else if (ext == "csv") data <- read.csv(path)
      else if (ext == "rds") data <- readRDS(path)
      else if (ext %in% c("xlsx", "xls")) data <- openxlsx::read.xlsx(path)
    })

    if (is.null(data)) {
      cli::cli_alert_warning("Skipping {key} (Could not read)")
      return()
    }

    # 2. Filter (Smart Filter)
    # We look for columns that might hold the ID (pid, hserial, etc.)
    # You might need to adjust 'id_cols' based on your schema
    id_cols <- c("pid", "hserial", "newident", "person_id")
    found_col <- intersect(names(data), id_cols)

    if (length(found_col) > 0) {
      # Filter!
      col <- found_col[1]
      initial_rows <- nrow(data)

      # Use base subsetting to be safe
      data <- data[data[[col]] %in% target_ids, ]

      # Special Case: If filtering resulted in 0 rows, keep header only?
      # Or maybe these IDs don't exist in this specific file.

      cli::cli_alert_success("Filtered {key}: {initial_rows} -> {nrow(data)} rows")

      # 3. Write to Tracer Dir
      # We verify the filename to avoid directory issues
      new_path <- file.path(output_dir, basename(path))

      if (ext == "SAV") haven::write_sav(data, new_path)
      else if (ext == "csv") write.csv(data, new_path, row.names=FALSE)
      else if (ext == "rds") saveRDS(data, new_path)
      else if (ext %in% c("xlsx", "xls")) openxlsx::write.xlsx(data, new_path)

      return(new_path)

    } else {
      cli::cli_alert_warning("Skipping {key} (No ID column found)")
      return(NULL)
    }
  }

  # Loop through all paths in your config
  new_paths <- list()
  for (key in names(paths_list)) {
    path <- paths_list[[key]]
    # Only process files, not directories
    if (!dir.exists(path)) {
      new_p <- process_file(key, path)
      if (!is.null(new_p)) new_paths[[key]] <- new_p
    }
  }

  return(new_paths)
}
