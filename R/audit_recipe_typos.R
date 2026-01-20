#' Audit Recipe for Typos
#'
#' Scans a compiled recipe for suspicious variable names using fuzzy matching.
#' This is the "Safety Net" when using compile_rules(allowed_vars = NULL).
#'
#' @param recipe A compiled rule_recipe object.
#' @return A tibble of suspicious pairs.
#' @export
audit_recipe_typos <- function(recipe) {
  # 1. Extract all Targets from the recipe
  targets <- vapply(recipe, function(x) x$target, character(1))

  # 2. Fake a "Census" so we can reuse our existing logic
  # We count occurrences (though in a recipe, uniqueness of Target is common,
  # but maybe 'r' is defined in one file and 'r2' in another if we merge recipes)
  fake_census <- dplyr::tibble(variable = targets) |>
    dplyr::count(.data$variable, name = "n")

  # 3. Use the Engine we already built
  return(detect_typos(fake_census, max_distance = 1))
}
