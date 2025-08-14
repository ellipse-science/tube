# Load current source code (not published package)
suppressMessages(suppressWarnings(devtools::load_all(".", quiet = TRUE)))

# DEBUGGING TESTS:
# - Normal run: Routine output suppressed for clean results
# - Verbose mode: Set TUBE_TEST_VERBOSE=TRUE to see all output for debugging
# - Example: Sys.setenv(TUBE_TEST_VERBOSE = "TRUE"); devtools::test(filter = "basic-functions")

# Helper function for conditional output suppression following testing best practices
# Usage: Set TUBE_TEST_VERBOSE=TRUE to see all output for debugging failed tests
conditionally_suppress <- function(expr) {
  if (Sys.getenv("TUBE_TEST_VERBOSE", "FALSE") == "TRUE") {
    # Verbose mode: show all output for debugging
    expr
  } else {
    # Normal mode: suppress messages and warnings but preserve return values
    # Note: CLI alerts from cli::cli_alert_*() may still show as they bypass normal suppression
    suppressMessages(suppressWarnings(expr))
  }
}


test_that("convert_url_to_key works with HTTP URLs", {
  debug_log("Testing URL conversion utility")

  # Test basic URL conversion
  result <- convert_url_to_key("https://example.com/path/to/file.html")

  expect_equal(result, "example_com_path_to_file_html")
  expect_type(result, "character")
  expect_gt(nchar(result), 0)

  debug_log(sprintf("URL conversion result: %s", result))
})

test_that("convert_url_to_key handles different URL formats", {
  debug_log("Testing various URL format transformations")

  # Test multiple URL formats with expected outputs
  test_cases <- list(
    "https://test.com" = "test_com",
    "http://test.com/path" = "test_com_path", 
    "https://test.com/path?param=value" = "test_com_path_param_value"
  )

  for (url in names(test_cases)) {
    result <- convert_url_to_key(url)
    expected <- test_cases[[url]]

    test_detail(sprintf("URL: %s → %s", url, result))
    expect_equal(result, expected)
    expect_type(result, "character")
  }
})

test_that("convert_url_to_key handles edge cases", {
  debug_log("Testing URL conversion edge cases")

  # Test edge cases
  expect_equal(convert_url_to_key(""), "")
  expect_equal(convert_url_to_key("https://"), "")
  expect_equal(convert_url_to_key("no-protocol.com"), "no_protocol_com")
  expect_equal(convert_url_to_key("https://site.com/"), "site_com")
})

test_that("pipe operator is available and functional", {
  debug_log("Testing pipe operator functionality")

  # Test that the pipe operator works correctly
  input <- 1:3
  result <- input %>% sum()

  expect_equal(result, 6)
  expect_type(result, "integer")  # sum() of integers returns integer

  # Test more complex piping
  complex_result <- mtcars %>%
    head(5) %>%
    nrow()

  expect_equal(complex_result, 5)
})
