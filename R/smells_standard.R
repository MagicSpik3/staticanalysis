#' Check for Hardcoded/Absolute Paths
#' @noRd
check_absolute_paths <- function(pdata, file) {
  strings <- pdata[pdata$token == "STR_CONST", ]
  if (nrow(strings) == 0) {
    return(NULL)
  }

  clean_strs <- gsub("^[\"']|[\"']$", "", strings$text)

  # Logic: 4 Backslashes (UNC) or Drive Letter (C:/)
  is_unc <- startsWith(clean_strs, "\\\\\\\\") | startsWith(clean_strs, "//")
  is_drive <- nchar(clean_strs) >= 3 & grepl("^[a-zA-Z]:[\\\\/]", clean_strs)

  if (any(is_unc | is_drive)) {
    bad_strs <- strings[is_unc | is_drive, ]
    return(data.frame(
      file = file, line = bad_strs$line1,
      id = "ABSOLUTE_PATH",
      severity = "HIGH",
      category = "PORTABILITY",
      message = "Hardcoded absolute path detected. Use config or relative paths.",
      stringsAsFactors = FALSE
    ))
  }
  return(NULL)
}

#' Check for Global Assignment (<<-)
#' @noRd
check_global_assignment <- function(pdata, file) {
  super_assigns <- pdata[pdata$token == "LEFT_ASSIGN" & pdata$text == "<<-", ]
  if (nrow(super_assigns) > 0) {
    return(data.frame(
      file = file, line = super_assigns$line1,
      id = "GLOBAL_ASSIGNMENT",
      severity = "CRITICAL",
      category = "CORRECTNESS",
      message = "Mutating global state (<<-) is dangerous and hard to debug.",
      stringsAsFactors = FALSE
    ))
  }
  return(NULL)
}

#' Check for Unsafe T/F Usage
#' @noRd
check_unsafe_boolean <- function(pdata, file) {
  t_f_symbols <- pdata[pdata$token == "SYMBOL" & pdata$text %in% c("T", "F"), ]
  if (nrow(t_f_symbols) > 0) {
    return(data.frame(
      file = file, line = t_f_symbols$line1,
      id = "UNSAFE_BOOLEAN",
      severity = "LOW",
      category = "ROBUSTNESS",
      message = "Use TRUE/FALSE instead of T/F (which can be overwritten).",
      stringsAsFactors = FALSE
    ))
  }
  return(NULL)
}

#' Check for Unsafe Sequencing (1:length)
#' @noRd
check_unsafe_sequencing <- function(pdata, file) {
  colons <- pdata[pdata$token == "':'", ]
  if (nrow(colons) == 0) {
    return(NULL)
  }

  smells <- list()
  src_lines <- readLines(file, warn = FALSE)

  for (i in seq_len(nrow(colons))) {
    row <- colons[i, ]
    if (length(src_lines) >= row$line1) {
      code_line <- src_lines[row$line1]
      if (grepl("1\\s*:\\s*(length|nrow|ncol)\\(", code_line)) {
        smells[[length(smells) + 1]] <- data.frame(
          file = file, line = row$line1,
          id = "UNSAFE_SEQUENCE",
          severity = "MEDIUM",
          category = "ROBUSTNESS",
          message = "Use seq_along() or seq_len(). 1:N fails on empty inputs.",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(smells) == 0) {
    return(NULL)
  }
  return(do.call(rbind, smells))
}

#' Check for Self-Shadowing (The Ouroboros)
#' @noRd
check_self_shadowing <- function(funcs, dir_path) {
  smells <- list()

  for (i in seq_len(nrow(funcs))) {
    fn_name <- funcs$name[i]
    f_path <- file.path(dir_path, funcs$file[i])
    pdata <- get_file_ast(f_path)

    loc <- find_func_lines(pdata, fn_name)
    if (!is.null(loc)) {
      body_tokens <- pdata[pdata$line1 >= loc$start & pdata$line2 <= loc$end, ]

      shadows <- body_tokens[body_tokens$token == "SYMBOL" & body_tokens$text == fn_name, ]

      if (nrow(shadows) > 0) {
        for (j in seq_len(nrow(shadows))) {
          sym_id <- shadows$id[j]
          next_tok <- pdata[pdata$id > sym_id, ]
          if (nrow(next_tok) > 0 && next_tok[1, "token"] %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) {
            smells[[length(smells) + 1]] <- data.frame(
              file = funcs$file[i], line = shadows$line1[j],
              id = "SELF_SHADOWING",
              severity = "CRITICAL",
              category = "CORRECTNESS",
              message = paste("Function", fn_name, "overwrites itself (The Ouroboros)."),
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }

  if (length(smells) == 0) {
    return(NULL)
  }
  return(do.call(rbind, smells))
}
