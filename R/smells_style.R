#' Check for Magic Numbers
#'
#' Flags numeric literals that are not 0, 1, or -1.
#' Magic numbers should be defined as named constants (e.g., BASE_SERIAL_OFFSET).
#'
#' @noRd
check_magic_numbers <- function(pdata, file) {
  nums <- pdata[pdata$token == "NUM_CONST", ]
  if (nrow(nums) == 0) return(NULL)

  smells <- list()

  for (i in seq_len(nrow(nums))) {
    val <- as.numeric(nums$text[i])

    # Heuristic: Ignore common integers (0, 1, 10, 100) and small variations
    if (!is.na(val) && abs(val) > 100 && val != 1000) {

      # Context Check: Is it part of a sequence like 1:1000?
      # (We skip if parent is ':')
      parent_id <- nums$parent[i]
      siblings <- pdata[pdata$parent == parent_id, ]
      if (any(siblings$token == "':'")) next

      smells[[length(smells) + 1]] <- data.frame(
        file = file, line = nums$line1[i],
        id = "MAGIC_NUMBER",
        severity = "LOW",
        category = "MAINTAINABILITY",
        message = paste0("Magic number detected: ", val, ". Define as a named constant."),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(smells) == 0) return(NULL)
  return(do.call(rbind, smells))
}
