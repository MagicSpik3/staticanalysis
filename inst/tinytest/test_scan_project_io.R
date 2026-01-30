# Test: scan_project_io correctly identifies READ operations
  temp_dir <- withr::local_tempdir()

  code <- '
    df1 <- read.csv("data/input.csv")
    df2 <- readRDS(input_var)
    df3 <- haven::read_sav("survey.sav")
  '
  writeLines(code, file.path(temp_dir, "reads.R"))

  res <- scan_project_io(temp_dir)

  expect_false(is.null(res))
  expect_equal(nrow(res), 3)

  # FIX: Filter by function name to be precise
  # Check Literal Extraction (specifically for read.csv)
  lit <- res[res$func == "read.csv", ]
  expect_equal(lit$literal_value, "data/input.csv")

  # Check Variable Extraction
  sym <- res[res$func == "readRDS", ]
  expect_equal(sym$arg_text, "input_var")

  # Check Namespaced Function
  ns <- res[res$func == "read_sav", ]
  expect_equal(ns$literal_value, "survey.sav")

# Test: scan_project_io correctly identifies WRITE operations
  temp_dir <- withr::local_tempdir()

  code <- '
    write.csv(x, "output/results.csv")
    saveRDS(object = model, file = "output/model.rds")
    ggsave("plot.png", plot = p)
  '
  writeLines(code, file.path(temp_dir, "writes.R"))

  res <- scan_project_io(temp_dir)

  expect_equal(nrow(res), 3)
  expect_true(all(res$type == "WRITE"))

  # Check standard write.csv
  csv <- res[res$func == "write.csv", ]
  expect_equal(csv$literal_value, "output/results.csv")

  # Check named argument extraction
  rds <- res[res$func == "saveRDS", ]
  expect_equal(rds$literal_value, "output/model.rds")

  # Check ggsave
  gg <- res[res$func == "ggsave", ]
  expect_equal(gg$literal_value, "plot.png")

# Test: scan_project_io captures complex argument syntax EXACTLY
  temp_dir <- withr::local_tempdir()

  code <- '
    read.csv(paths$raw_data)
    read.csv(paths[["raw_data"]])
    source(file.path(root, "script.R"))
  '
  writeLines(code, file.path(temp_dir, "syntax.R"))

  res <- scan_project_io(temp_dir)

  # 1. Dollar Sign
  dollar <- res[grepl("\\$", res$arg_text), ]
  expect_equal(dollar$arg_text, "paths$raw_data")

  # 2. Bracket Notation
  # Note: In R AST, `[[` is a function call, so this is type="call"
  bracket <- res[grepl("\\[\\[", res$arg_text), ]
  expect_match(bracket$arg_text, 'paths\\[\\["raw_data"\\]\\]')

  # 3. Call (file.path)
  # FIX: Filter specifically for file.path to avoid picking up the $ or [[ calls
  call_row <- res[grepl("file\\.path", res$arg_text), ]
  expect_match(call_row$arg_text, 'file.path\\(root, "script.R"\\)')

# Test: scan_project_io handles empty or irrelevant files
  temp_dir <- withr::local_tempdir()

  writeLines("x <- 1 + 1", file.path(temp_dir, "math.R"))
  res <- scan_project_io(temp_dir)

  expect_null(res)
