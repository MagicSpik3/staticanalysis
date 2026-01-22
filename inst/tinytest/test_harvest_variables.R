# Test: Harvester counts variables across multiple files
  # 1. Setup: Create a temp directory with 2 mock files
  tmp_dir <- fs::dir_create(tempfile())

  write.csv(
    data.frame(Target = c("r", "tax")),
    file.path(tmp_dir, "file1.csv")
  )

  write.csv(
    data.frame(Target = c("r", "net", "tax")),
    file.path(tmp_dir, "file2.csv")
  )

  # 2. Run Harvester
  census <- harvest_variables(tmp_dir)

  # 3. Verify Counts
  # 'r' appears in both files (Total 2)
  # 'tax' appears in both files (Total 2)
  # 'net' appears in one file (Total 1)

  expect_equal(nrow(census), 3)

  r_count <- census$n[census$variable == "r"]
  expect_equal(r_count, 2)

  net_count <- census$n[census$variable == "net"]
  expect_equal(net_count, 1)

  # Cleanup
  fs::dir_delete(tmp_dir)
