# Global setup: Ensure CLI doesn't use colors, so we can match plain text strings.
# We do this once at the top of the file or in a helper.
options(cli.num_colors = 0)

# Test: print_smells handles clean code (empty input)
  # 1. Use expect_message because cli_alert_success writes to stderr
  expect_message(
    print_smells(NULL),
    "Clean code"
  )

  expect_message(
    print_smells(data.frame()),
    "Clean code"
  )

# Test: print_smells formats report correctly
  smells <- data.frame(
    file = "R/test_script.R",
    line = 42,
    id = "MAGIC_NUMBER",
    severity = "MEDIUM",
    category = "MAINTAINABILITY",
    message = "Avoid using 3000000.",
    sev_score = 3,
    stringsAsFactors = FALSE
  )

  # Note: cli functions often default to stderr (messages) in non-interactive sessions.
  # We use capture.output with type="message" to be safe and capture everything.

  output <- capture.output(
    print_smells(smells),
    type = "message"
  )

  # Combine into one string for easier searching
  text <- paste(output, collapse = "\n")

  expect_match(text, "Code Smell Audit Report", fixed = TRUE)
  expect_match(text, "Category: MAINTAINABILITY", fixed = TRUE)
  expect_match(text, "test_script.R:42", fixed = TRUE)

# Test: print_smells sorts critical issues to the top
  smells <- data.frame(
    file = c("R/style.R", "R/security.R"),
    line = c(10, 20),
    id = c("BAD_STYLE", "SQL_INJECTION"),
    severity = c("LOW", "CRITICAL"),
    category = c("STYLE", "SECURITY"),
    message = c("Indent wrong", "Bobby Tables"),
    stringsAsFactors = FALSE
  )

  # Capture the full output (stderr)
  output <- capture.output(
    print_smells(smells),
    type = "message"
  )
  text <- paste(output, collapse = "\n")

  # Find positions of the smell IDs
  pos_critical <- regexpr("SQL_INJECTION", text, fixed = TRUE)[1]
  pos_low      <- regexpr("BAD_STYLE", text, fixed = TRUE)[1]

  expect_true(pos_critical > 0, "Critical message missing")
  expect_true(pos_low > 0, "Low message missing")

  # Critical (index) must be smaller (earlier) than Low
  expect_true(pos_critical < pos_low, "CRITICAL should appear before LOW")
