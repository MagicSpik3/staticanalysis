test_that("check_base_overwrite detects shadowing of standard functions", {
  funcs <- data.frame(
    name = c("mean", "sum", "my_func"),
    file = c("R/bad.R", "R/bad.R", "R/good.R"),
    stringsAsFactors = FALSE
  )

  res <- staticanalysis:::check_base_overwrite(funcs)

  expect_false(is.null(res))
  expect_equal(nrow(res), 2)

  # FIX: Combine messages into one string and search inside it
  all_messages <- paste(res$message, collapse = " | ")

  # Check that 'mean' is mentioned SOMEWHERE in the messages
  expect_match(all_messages, "mean")
  expect_match(all_messages, "sum")
})
