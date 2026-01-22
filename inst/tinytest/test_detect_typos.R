# Test: detect_typos identifies 'ratw' as a neighbor of 'rate'
  # 1. Create a Fake Census
  # 'rate' is common (50), 'ratw' is rare (1)
  mock_census <- dplyr::tibble(
    variable = c("rate", "ratw", "tax", "totally_different"),
    n = c(50, 1, 10, 5)
  )

  # 2. Run Detector
  report <- detect_typos(mock_census, max_distance = 1)

  # 3. Verify
  expect_equal(nrow(report), 1)
  expect_equal(report$var_a, "rate")
  expect_equal(report$var_b, "ratw")
  expect_equal(report$distance, 1)

  # Ensure 'tax' and 'totally_different' were NOT matched
  expect_false("tax" %in% report$var_a)
