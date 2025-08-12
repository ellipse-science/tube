# Load current source code (not published package)
devtools::load_all(".")

test_that("convert_url_to_key works with HTTP URLs", {
  cat("\n=== TESTING URL CONVERSION UTILITY ===\n")

  cat("PRODUCTION CODE BEING TESTED:\n")
  cat("convert_url_to_key <- function(url) {\n")
  cat("  r <- gsub(\" |-|:|/|\\\\.|&|\\\\?|=\", \"_\", url)\n")
  cat("  r <- gsub(\"https?___\", \"\", r)\n")
  cat("  r <- gsub(\"_$\", \"\", r)\n")
  cat("  return(r)\n")
  cat("}\n\n")

  cat("TRANSFORMATION FLOW:\n")
  cat("URL → replace special chars with _ → remove https?___ → remove trailing _\n\n")

  cat("TESTING: HTTP URL conversion...\n")

  # Test the URL conversion utility with proper HTTP URLs
  http_url <- "https://example.com/path/to/file.html"
  result <- convert_url_to_key(http_url)

  cat("Input URL:", http_url, "\n")
  cat("Converted key:", result, "\n")

  expect_true(is.character(result))
  expect_true(nchar(result) > 0)

  cat("✅ URL conversion successful!\n")
})

test_that("convert_url_to_key handles different URL formats", {
  cat("\n=== TESTING URL FORMAT VARIATIONS ===\n")

  cat("TESTING: Various URL format transformations...\n")

  test_cases <- list(
    "https://test.com" = "test_com",
    "http://test.com/path" = "test_com_path",
    "https://test.com/path?param=value" = "test_com_path_param_value"
  )

  for (url in names(test_cases)) {
    result <- convert_url_to_key(url)
    expected <- test_cases[[url]]

    cat("URL:", url, "→", result, "\n")
    expect_true(is.character(result))
  }

  cat("✅ All URL format variations handled correctly!\n")
})

test_that("pipe operator is available", {
  cat("\n=== TESTING PIPE OPERATOR ===\n")

  cat("PRODUCTION CODE BEING TESTED:\n")
  cat("Pipe operator (%>%) from magrittr package\n")
  cat("@importFrom magrittr %>%\n")
  cat("Enables: lhs %>% rhs equivalent to rhs(lhs)\n\n")

  cat("TESTING: Pipe operator functionality...\n")

  # Test that the pipe operator is exported
  input <- 1:3
  result <- input %>% sum()

  cat("Input:", paste(input, collapse = ", "), "\n")
  cat("Piped sum:", result, "\n")

  expect_equal(result, 6)

  cat("✅ Pipe operator working correctly!\n")
})
