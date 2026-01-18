#' Detect Potential Typos
#'
#' Analyzes a variable census to find "near neighbors" — variables that look
#' similar but are likely typos (e.g., "rate" vs "ratw").
#'
#' @author Mark London
#' @param census A tibble returned by harvest_variables().
#' @param max_distance Integer. Maximum number of character edits. Default is 1.
#' @return A tibble of suspicious pairs.
#' @importFrom rlang .data
#' @export
detect_typos <- function(census, max_distance = 1) {
  if (nrow(census) < 2) {
    return(NULL)
  }

  vars <- census$variable
  dist_matrix <- utils::adist(vars)

  dist_matrix[lower.tri(dist_matrix, diag = TRUE)] <- NA
  matches <- which(dist_matrix <= max_distance, arr.ind = TRUE)

  if (nrow(matches) == 0) {
    return(dplyr::tibble(
      var_a = character(),
      var_b = character(),
      distance = integer()
    ))
  }

  results <- dplyr::tibble(
    idx_a = matches[, 1],
    idx_b = matches[, 2],
    distance = dist_matrix[matches]
  ) |>
    dplyr::mutate(
      var_a = vars[.data$idx_a],
      var_b = vars[.data$idx_b],
      count_a = census$n[.data$idx_a],
      count_b = census$n[.data$idx_b]
    ) |>
    # FIX: Use strings in select() to avoid deprecation warning
    dplyr::select("var_a", "count_a", "var_b", "count_b", "distance") |>
    dplyr::arrange(.data$distance, dplyr::desc(.data$count_a))

  return(results)
}
