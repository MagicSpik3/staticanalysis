# Test: check_ambiguous_selection flags dataframe subsetting without drop=FALSE
  # Scenario: df[row, col] returns a vector, not a dataframe.
  # This breaks pipelines expecting 2D objects.

  code <- "
    # Bad: Implicit drop
    x <- df[i, j]

    # Bad: Explicit drop=TRUE
    y <- df[i, j, drop = TRUE]

    # Good: Explicit drop=FALSE
    z <- df[i, j, drop = FALSE]

    # Good: List selection (returns dataframe)
    a <- df['colname']
    b <- df[1]
  "

  pdata <- utils::getParseData(parse(text = code, keep.source = TRUE))

  # Use triple colon to access internal function
  smells <- staticanalysis:::check_ambiguous_selection(pdata, "test.R")

  expect_false(is.null(smells))

  # We expect 2 hits (the Bad lines)
  expect_equal(nrow(smells), 2)
  expect_equal(smells$line, c(3, 6)) # Lines 3 and 6 in the string above
  expect_equal(smells$id[1], "AMBIGUOUS_SELECTION")
