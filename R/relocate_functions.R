#' Relocate Functions to New Repositories
#'
#' Reads the project inventory and copies specific functions into a new directory structure.
#' Useful for splitting a Monolith into Micro-services.
#'
#' @param inventory The dataframe returned by audit_inventory().
#' @param function_map A named list where keys are destination paths and values are character
#' @param source_root String. The root directory to read source files from.  <-- ADD THIS
#' @param dry_run Logical. If TRUE, just logs actions.
#' @export
relocate_functions <- function(inventory, function_map, source_root, dry_run = TRUE) {
  if (is.null(inventory)) stop("Inventory is empty.")

  for (dest_dir in names(function_map)) {
    funcs_to_move <- function_map[[dest_dir]]

    # Ensure destination exists
    if (!dry_run) fs::dir_create(dest_dir)

    message(sprintf("\n[START] Moving %d functions to: %s", length(funcs_to_move), dest_dir))


    for (fn_name in funcs_to_move) {
      # Find where it lives now
      record <- inventory[inventory$name == fn_name & inventory$type == "function", ]

      if (nrow(record) == 0) {
        warning(sprintf("[WARN]  Function '%s' not found in inventory. Skipping.", fn_name))
        next
      }

      # We take the first match if there are duplicates (e.g., defined in multiple files)
      src_file <- file.path(source_root, record$file[1])

      # Extract the function definition safely
      # We parse the file, find the function, and deparse it back to text
      env <- new.env()
      tryCatch(
        {
          sys.source(src_file, envir = env, keep.source = TRUE)
          fn_obj <- get(fn_name, envir = env)

          # Write to new file (One function per file is best practice)
          new_filename <- file.path(dest_dir, paste0(fn_name, ".R"))

          if (dry_run) {
            message(sprintf("  [DRY] Would write '%s' to %s", fn_name, new_filename))
          } else {
            # FIX: Use 'utils::capture.output' to satisfy the namespace checker
            writeLines(utils::capture.output(print(fn_obj)), new_filename)
            message(sprintf("  [OK] Wrote '%s'", new_filename))
          }
        },
        error = function(e) {
          warning(sprintf("  [ERROR] Failed to extract '%s': %s", fn_name, e$message))
        }
      )
    }
  }
}
