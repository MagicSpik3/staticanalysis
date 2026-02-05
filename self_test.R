# 1. Load the latest version of your tools
devtools::load_all()
devtools::check()

library(staticanalysis)

# 2. Define target as the current project root
#target <- "."
target <- "D:/git/was-methods/was.methods/R"
# --- TEST 1: INVENTORY ---
message("[SCAN] Running Inventory on Self...")
inv <- audit_inventory(target)
# You should see your new modular functions here (scan_definitions, etc.)
print(inv[inv$type == "function", c("name", "file", "called_in_test", "misplaced", "export_status")])

# --- TEST 2: DEPENDENCIES ---
message("\n📦 Running Dependency Scan on Self...")
deps <- scan_dependencies(target)
# Expectation: 'testthat', 'fs', 'utils' should be used.
# 'tidytable' should be GONE (except in that one mock test).
print(deps$usage_stats)
print(paste("Ghosts:", paste(deps$undeclared_ghosts, collapse=", ")))

# --- TEST 3: DUPLICATES ---
message("\n👯 Checking for Code Duplication...")
dupes <- detect_duplicates(target)
if (!is.null(dupes)) {
  print(dupes)
} else {
  message("No duplicates found (Clean Code!).")
}

# install.packages("writexl")

# Prepare the list of sheets
report_data <- list(
  Inventory = inv,
  Dependencies = deps$usage_stats,
  Ghosts = data.frame(package = deps$undeclared_ghosts), # Save the ghosts too!
  Duplicates = if (is.null(dupes)) data.frame(Status = "Clean Code") else dupes
)

# Write single file
# Extract package name (the folder just above R/)
pkg_name <- basename(dirname(target))

# Build output filename
outfile <- paste0("static_analysis_report_", pkg_name, ".xlsx")

# Write report
writexl::write_xlsx(report_data, outfile)

message("✅ Report generated: static_analysis_report.xlsx")


library(lintr)
lint_package("D:/git/was-methods/was.methods")


#✅ 5. Use a clone detection tool (PMD CPD / SIMIAN) — works with R
#PMD’s CPD can detect duplicated logic in ANY language including R.
#Install CPD (Java required):
#  https://pmd.github.io/latest/pmd_userdocs_cpd.html
#Then from your package root:

#cpd --minimum-tokens 40 --files R --language r

# Run lintr
lints <- lintr::lint_package(target)
# Source - https://stackoverflow.com/a/60853049
# Posted by Goerman
# Retrieved 2026-02-05, License - CC BY-SA 4.0

result <- lintr::lint_package(target)
df <- as.data.frame(result)
write.csv(df, file="output_file_name.csv")

# Source - https://stackoverflow.com/a/46513285
# Posted by fmic_
# Retrieved 2026-02-05, License - CC BY-SA 3.0

capture.output(lint("myscript.R"), file="lint_output.txt")



library(tools)
target
files <- list.files(target, full.names = TRUE)
hashes <- sapply(files, function(f) {
  digest::digest(readChar(f, file.info(f)$size))
})

data.frame(
  file = basename(files),
  hash = hashes
)


txt <- lapply(files, readLines)
names(txt) <- files

for (i in seq_along(txt)) {
  for (j in seq_along(txt)) {
    if (i < j) {
      d <- adist(paste(txt[[i]], collapse=" "),
                 paste(txt[[j]], collapse=" "))
      if (d < 40) {
        cat("\nPossible duplicate:", basename(files[i]), "and", basename(files[j]), "\nDistance:", d, "\n\n")
      }
    }
  }
}

# Target directory
target <- "D:/git/was-methods/was.methods"
target <- "."
target <- "D:/git/mypkg"


# 1. Get only .R files, recursively
files <- list.files(
  path = target,
  pattern = "\\.R$",       # only .R files
  full.names = TRUE,
  recursive = TRUE
)

# 2. Compute file hashes (identical files → identical hashes)
hashes <- sapply(files, function(f) {
  digest::digest(readChar(f, file.info(f)$size))
})

# 3. Show hash table (optional)
hash_table <- data.frame(
  file = files,
  hash = hashes,
  stringsAsFactors = FALSE
)

print(hash_table)

# 4. Read file text for approximate‑duplicate detection
txt <- lapply(files, readLines, warn = FALSE)
names(txt) <- files

# 5. Pairwise comparison using adist()
for (i in seq_along(txt)) {
  for (j in seq_along(txt)) {
    if (i < j) {

      # collapse the text so adist works sensibly
      d <- adist(
        paste(txt[[i]], collapse = " "),
        paste(txt[[j]], collapse = " ")
      )

      # threshold controls similarity sensitivity
      if (d < 40) {
        cat(
          "\n⚠️  Possible duplicate:",
          "\n    •", files[i],
          "\n    •", files[j],
          "\n    Distance:", d, "\n"
        )
      }
    }
  }
}


# Duplicate function finder for R packages (recursive, .R-only, no comments)
# Requires: digest
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Please install 'digest' first: install.packages('digest')")
}

find_duplicate_functions <- function(target,
                                     max_dist = 40,       # Levenshtein threshold (tune)
                                     bucket_width = 80,   # compare only functions with |len_i - len_j| <= this
                                     min_chars = 30,      # ignore tiny functions (speed)
                                     max_pairs = 5e5      # safety cap for pairwise comparisons
) {
  # 1) Enumerate .R files recursively
  files <- list.files(target, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  if (length(files) == 0) {
    message("No .R files found under: ", target)
    return(invisible(list(exact = NULL, near = NULL, functions = NULL)))
  }

  # 2) Parse each file, collect top-level function definitions (no comments)
  funs <- list()
  for (f in files) {
    # robust parse: skip files that fail to parse
    exprs <- try(parse(f, keep.source = TRUE), silent = TRUE)
    if (inherits(exprs, "try-error")) next

    for (e in exprs) {
      # Match patterns: name <- function(...) { ... }  or  name = function(...) { ... }
      if (is.call(e) && (identical(e[[1]], as.symbol("<-")) || identical(e[[1]], as.symbol("=")))) {
        lhs <- e[[2]]
        rhs <- e[[3]]
        if (is.symbol(lhs) && is.call(rhs) && identical(rhs[[1]], as.symbol("function"))) {
          fn_name <- as.character(lhs)
          # We do not evaluate; we normalize text from the language object
          # Deparse removes comments; we also normalize whitespace
          body_txt <- paste(deparse(rhs, width.cutoff = 500L), collapse = "\n")
          # strip leading/trailing whitespace and squash internal whitespace
          norm_txt <- gsub("[ \t]+", " ", gsub("[\r\n]+", "\n", body_txt, perl = TRUE), perl = TRUE)
          norm_txt <- trimws(norm_txt)

          # Ignore tiny functions (speed + low value)
          if (nchar(norm_txt) < min_chars) next

          funs[[length(funs) + 1L]] <- list(
            file = f,
            name = fn_name,
            text = norm_txt,
            nchars = nchar(norm_txt)
          )
        }
      }
    }
  }

  if (!length(funs)) {
    message("No top-level function() definitions found.")
    return(invisible(list(exact = NULL, near = NULL, functions = NULL)))
  }

  df <- data.frame(
    file   = vapply(funs, `[[`, "", "file"),
    name   = vapply(funs, `[[`, "", "name"),
    nchars = vapply(funs, `[[`, 0L, "nchars"),
    stringsAsFactors = FALSE
  )
  texts <- vapply(funs, `[[`, "", "text")

  # 3) Exact duplicates via hash
  hashes <- vapply(texts, digest::digest, "")
  df$hash <- hashes

  exact_groups <- split(seq_along(hashes), hashes)
  exact_dups_idx <- Filter(function(ix) length(ix) > 1L, exact_groups)

  exact <- NULL
  if (length(exact_dups_idx)) {
    exact <- do.call(rbind, lapply(exact_dups_idx, function(ix) {
      data.frame(
        hash = hashes[ix[1]],
        file = df$file[ix],
        name = df$name[ix],
        nchars = df$nchars[ix],
        stringsAsFactors = FALSE
      )
    }))
    rownames(exact) <- NULL
  }

  # 4) Near duplicates (bucketed by size to avoid O(N^2))
  # Create buckets by length; only compare within bucket +/- bucket_width
  ord <- order(df$nchars)
  idx <- seq_along(ord)

  near_list <- list()
  pair_count <- 0L

  for (k in idx) {
    i <- ord[k]
    # Two-pointer window for |len_j - len_i| <= bucket_width
    j_start <- k + 1L
    if (j_start > length(ord)) break
    # Expand forward while length difference within bucket_width
    for (kk in j_start:length(ord)) {
      j <- ord[kk]
      if ((df$nchars[j] - df$nchars[i]) > bucket_width) break

      # (Optional) skip same file + same name (probably itself)
      if (identical(df$file[i], df$file[j]) && identical(df$name[i], df$name[j])) next

      pair_count <- pair_count + 1L
      if (pair_count > max_pairs) {
        warning("Reached max_pairs cap (", max_pairs, "). Increase if needed.")
        break
      }

      d <- adist(texts[i], texts[j])
      if (d <= max_dist) {
        near_list[[length(near_list) + 1L]] <- data.frame(
          file1 = df$file[i],
          name1 = df$name[i],
          file2 = df$file[j],
          name2 = df$name[j],
          dist  = as.integer(d),
          len1  = df$nchars[i],
          len2  = df$nchars[j],
          stringsAsFactors = FALSE
        )
      }
    }
    if (pair_count > max_pairs) break
  }

  near <- if (length(near_list)) do.call(rbind, near_list) else NULL

  # 5) Return everything
  res <- list(
    functions = df,   # inventory of all functions found
    exact     = exact,
    near      = near
  )
  class(res) <- "dup_function_scan"
  res
}

# Pretty printer
print.dup_function_scan <- function(x, ...) {
  cat("Functions scanned:", nrow(x$functions), "\n")
  if (!is.null(x$exact) && nrow(x$exact)) {
    cat("\n== Exact duplicates (same hash) ==\n")
    print(x$exact, row.names = FALSE)
  } else {
    cat("\n== Exact duplicates == none\n")
  }
  if (!is.null(x$near) && nrow(x$near)) {
    cat("\n== Near duplicates (Levenshtein within threshold) ==\n")
    print(utils::head(x$near[order(x$near$dist), ], 20), row.names = FALSE)
    if (nrow(x$near) > 20) cat("... (", nrow(x$near) - 20, " more)\n", sep = "")
  } else {
    cat("\n== Near duplicates == none within thresholds\n")
  }
  invisible(x)
}


target <- "D:/git/was-methods/was.methods"  # package root (not just R/)
res <- find_duplicate_functions(
  target,
  max_dist     = 40,   # lower = stricter similarity
  bucket_width = 80,   # smaller = fewer comparisons
  min_chars    = 30    # ignore trivial functions
)
print(res)

# Optional: write Excel report
# writexl::write_xlsx(list(
#   functions = res$functions,
#   exact     = res$exact,
#   near      = res$near
# ), path = "duplicate_functions_report.xlsx")





