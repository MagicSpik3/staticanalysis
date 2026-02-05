#' Detect Non-DRY Logic
#' @param inv Result from get_function_inventory
#' @param threshold Levenshtein distance limit (default 40)
detect_near_dupes <- function(inv, threshold = 40, bucket_width = 100) {
  # Sort by size to enable windowed comparison
  inv <- inv[order(inv$size), ]
  n <- nrow(inv)
  matches <- list()

  for (i in 1:(n - 1)) {
    # Only look forward at functions within the size bucket
    for (j in (i + 1):n) {
      if ((inv$size[j] - inv$size[i]) > bucket_width) break

      # Quick check: Exact name match + similar size = High Alert
      dist <- as.numeric(adist(inv$body[i], inv$body[j]))

      if (dist <= threshold) {
        matches[[length(matches) + 1]] <- data.frame(
          fn_a = inv$name[i], fn_b = inv$name[j],
          file_a = inv$file[i], file_b = inv$file[j],
          distance = dist
        )
      }
    }
  }
  return(do.call(rbind, matches))
}
