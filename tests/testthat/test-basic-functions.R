test_that("convert_url_to_key works with HTTP URLs", {
  # Test the URL conversion utility with proper HTTP URLs
  http_url <- "https://example.com/path/to/file.html"
  result <- convert_url_to_key(http_url)
  expect_true(is.character(result))
  expect_true(nchar(result) > 0)
})

test_that("convert_url_to_key handles different URL formats", {
  # Test various URL formats
  expect_true(is.character(convert_url_to_key("https://test.com")))
  expect_true(is.character(convert_url_to_key("http://test.com/path")))
  expect_true(is.character(convert_url_to_key("https://test.com/path?param=value")))
})

test_that("pipe operator is available", {
  # Test that the pipe operator is exported
  result <- 1:3 %>% sum()
  expect_equal(result, 6)
})
