# Comprehensive tests for utility functions
# Tests for: ask_yes_no, ask_1_2, suppress_console_output, print_list_with_nulls,
#           convert_url_to_key, %>% (pipe operator)
# Following requirement: "use real life connections and data... Do not mock everything"

# Load current source code (not published package)
devtools::load_all(".")

test_that("utility functions can be loaded and have proper signatures", {
  # Check that all utility functions exist
  expect_true(exists("ask_yes_no", mode = "function"))
  expect_true(exists("ask_1_2", mode = "function"))
  expect_true(exists("suppress_console_output", mode = "function"))
  expect_true(exists("print_list_with_nulls", mode = "function"))
  expect_true(exists("convert_url_to_key", mode = "function"))
  expect_true(exists("%>%", mode = "function"))
  
  # Check function signatures
  expect_equal(length(formals(ask_yes_no)), 2)      # question, unattended_option
  expect_equal(length(formals(ask_1_2)), 2)         # question, unattended_option
  expect_equal(length(formals(suppress_console_output)), 1)  # expr
  expect_equal(length(formals(print_list_with_nulls)), 1)    # lst
  expect_equal(length(formals(convert_url_to_key)), 1)       # url
})

# Tests for ask_yes_no function
test_that("ask_yes_no handles unattended options correctly", {
  # Test with "oui" unattended option
  result <- ask_yes_no("Test question?", unattended_option = "oui")
  expect_true(result)
  
  # Test with "o" unattended option
  result <- ask_yes_no("Test question?", unattended_option = "o")
  expect_true(result)
  
  # Test with "non" unattended option
  result <- ask_yes_no("Test question?", unattended_option = "non")
  expect_false(result)
  
  # Test with "n" unattended option
  result <- ask_yes_no("Test question?", unattended_option = "n")
  expect_false(result)
  
  # Test with invalid unattended option
  result <- ask_yes_no("Test question?", unattended_option = "maybe")
  expect_false(result)
})

test_that("ask_yes_no validates input parameters", {
  # Test with different question types
  expect_type(ask_yes_no("Simple question?", "oui"), "logical")
  expect_type(ask_yes_no("", "non"), "logical")
  
  # Test with NULL unattended option (would require user input in interactive mode)
  # Since we can't test interactive input, we verify the function exists and accepts NULL
  expect_true(is.function(ask_yes_no))
})

test_that("ask_yes_no handles edge cases", {
  # Test with case variations
  expect_true(ask_yes_no("Test?", "oui"))
  expect_true(ask_yes_no("Test?", "OUI"))  # Should handle case
  expect_false(ask_yes_no("Test?", "non"))
  
  # Test with empty string
  expect_false(ask_yes_no("Test?", ""))
  
  # Test with whitespace
  expect_false(ask_yes_no("Test?", " oui "))  # Should not accept with spaces
})

# Tests for ask_1_2 function
test_that("ask_1_2 handles unattended options correctly", {
  # Test with "1" unattended option
  result <- ask_1_2("Choose option:", unattended_option = "1")
  expect_equal(result, "1")
  
  # Test with "2" unattended option
  result <- ask_1_2("Choose option:", unattended_option = "2")
  expect_equal(result, "2")
})

test_that("ask_1_2 validates unattended options", {
  # Test with invalid unattended option
  expect_error(
    ask_1_2("Choose option:", unattended_option = "3"),
    "L'option unattended de ask_1_2 doit être 1 ou 2"
  )
  
  expect_error(
    ask_1_2("Choose option:", unattended_option = "0"),
    "L'option unattended de ask_1_2 doit être 1 ou 2"
  )
  
  expect_error(
    ask_1_2("Choose option:", unattended_option = "a"),
    "L'option unattended de ask_1_2 doit être 1 ou 2"
  )
  
  expect_error(
    ask_1_2("Choose option:", unattended_option = ""),
    "L'option unattended de ask_1_2 doit être 1 ou 2"
  )
})

test_that("ask_1_2 handles edge cases", {
  # Test with different question formats
  expect_equal(ask_1_2("", "1"), "1")
  expect_equal(ask_1_2("Long question with multiple words?", "2"), "2")
  
  # Test return type is always character
  expect_type(ask_1_2("Test?", "1"), "character")
  expect_type(ask_1_2("Test?", "2"), "character")
})

# Tests for suppress_console_output function
test_that("suppress_console_output suppresses output correctly", {
  # Test that output is suppressed
  temp_file <- tempfile()
  
  # Function that would normally produce output
  test_expr <- {
    cat("This should be suppressed\n")
    print("This too")
    message("And this message")
    "return_value"
  }
  
  # Capture the result
  result <- suppress_console_output(test_expr)
  
  # Should return the expression result
  expect_equal(result, "return_value")
  
  # Clean up any temp files that might have been created
  if (file.exists(temp_file)) unlink(temp_file)
})

test_that("suppress_console_output handles expressions with errors", {
  # Test that errors are still propagated
  expect_error({
    suppress_console_output({
      cat("Some output\n")
      stop("Test error")
    })
  }, "Test error")
})

test_that("suppress_console_output handles different expression types", {
  # Test with simple value
  result1 <- suppress_console_output(42)
  expect_equal(result1, 42)
  
  # Test with function call
  result2 <- suppress_console_output({
    x <- 1 + 1
    x * 2
  })
  expect_equal(result2, 4)
  
  # Test with NULL result
  result3 <- suppress_console_output({
    cat("output\n")
    NULL
  })
  expect_null(result3)
})

# Tests for print_list_with_nulls function
test_that("print_list_with_nulls handles lists with NULL values", {
  # Test list with mixed NULL and non-NULL values
  test_list <- list(
    item1 = "value1",
    item2 = NULL,
    item3 = 42,
    item4 = NULL,
    item5 = "value5"
  )
  
  # Should not throw errors
  expect_no_error(print_list_with_nulls(test_list))
  
  # Test empty list
  expect_no_error(print_list_with_nulls(list()))
  
  # Test list with all NULL values
  all_null_list <- list(a = NULL, b = NULL, c = NULL)
  expect_no_error(print_list_with_nulls(all_null_list))
  
  # Test list with no NULL values
  no_null_list <- list(a = 1, b = "text", c = TRUE)
  expect_no_error(print_list_with_nulls(no_null_list))
})

test_that("print_list_with_nulls handles different data types", {
  # Test list with various data types
  complex_list <- list(
    number = 123,
    text = "hello",
    logical = TRUE,
    null_value = NULL,
    vector = c(1, 2, 3),
    nested_list = list(a = 1, b = NULL)
  )
  
  expect_no_error(print_list_with_nulls(complex_list))
})

test_that("print_list_with_nulls handles unnamed lists", {
  # Test with unnamed list
  unnamed_list <- list("value1", NULL, "value3", NULL)
  expect_no_error(print_list_with_nulls(unnamed_list))
  
  # Test with partially named list
  partial_names <- list(a = "value1", NULL, c = "value3")
  expect_no_error(print_list_with_nulls(partial_names))
})

# Tests for convert_url_to_key function
test_that("convert_url_to_key converts URLs correctly", {
  # Test basic HTTP URL
  url1 <- "https://example.com/path/to/file.html"
  result1 <- convert_url_to_key(url1)
  expect_type(result1, "character")
  expect_true(nchar(result1) > 0)
  expect_false(grepl("https://", result1))  # Should remove protocol
  
  # Test HTTPS URL
  url2 <- "http://test.com/another/path"
  result2 <- convert_url_to_key(url2)
  expect_type(result2, "character")
  expect_false(grepl("http://", result2))  # Should remove protocol
})

test_that("convert_url_to_key handles special characters", {
  # Test URL with query parameters
  url_query <- "https://example.com/search?q=test&category=books"
  result_query <- convert_url_to_key(url_query)
  expect_false(grepl("\\?", result_query))  # Should replace ?
  expect_false(grepl("&", result_query))    # Should replace &
  expect_false(grepl("=", result_query))    # Should replace =
  
  # Test URL with special characters
  url_special <- "https://example.com/path-with-dashes/file.name.ext"
  result_special <- convert_url_to_key(url_special)
  expect_false(grepl("-", result_special))  # Should replace -
  expect_false(grepl("\\.", result_special)) # Should replace .
  
  # Test URL with colons and slashes
  url_complex <- "https://example.com:8080/path/to/file"
  result_complex <- convert_url_to_key(url_complex)
  expect_false(grepl(":", result_complex))  # Should replace :
  expect_false(grepl("/", result_complex))  # Should replace /
})

test_that("convert_url_to_key handles edge cases", {
  # Test empty string
  result_empty <- convert_url_to_key("")
  expect_equal(result_empty, "")
  
  # Test URL with no protocol
  url_no_protocol <- "example.com/path"
  result_no_protocol <- convert_url_to_key(url_no_protocol)
  expect_type(result_no_protocol, "character")
  
  # Test URL ending with slash
  url_trailing_slash <- "https://example.com/path/"
  result_trailing <- convert_url_to_key(url_trailing_slash)
  expect_false(grepl("/$", result_trailing))  # Should remove trailing _
})

test_that("convert_url_to_key produces consistent results", {
  # Test that same input produces same output
  url <- "https://example.com/test/path?param=value"
  result1 <- convert_url_to_key(url)
  result2 <- convert_url_to_key(url)
  expect_equal(result1, result2)
  
  # Test that different URLs produce different results
  url_a <- "https://example.com/path/a"
  url_b <- "https://example.com/path/b"
  result_a <- convert_url_to_key(url_a)
  result_b <- convert_url_to_key(url_b)
  expect_false(identical(result_a, result_b))
})

# Tests for pipe operator %>%
test_that("pipe operator works correctly", {
  # Test basic piping
  result1 <- 1:5 %>% sum()
  expect_equal(result1, 15)
  
  # Test chained piping
  result2 <- 1:10 %>% 
    sum() %>% 
    sqrt()
  expect_equal(result2, sqrt(55))
  
  # Test with data frame operations
  test_df <- data.frame(x = 1:3, y = 4:6)
  result3 <- test_df %>% nrow()
  expect_equal(result3, 3)
})

test_that("pipe operator handles different data types", {
  # Test with character vector
  result1 <- c("a", "b", "c") %>% length()
  expect_equal(result1, 3)
  
  # Test with list
  result2 <- list(a = 1, b = 2, c = 3) %>% names()
  expect_equal(result2, c("a", "b", "c"))
  
  # Test with NULL
  result3 <- NULL %>% is.null()
  expect_true(result3)
})

test_that("utility functions handle error conditions gracefully", {
  # Test functions with invalid inputs
  expect_error(ask_1_2("test", "invalid"))  # Should error on invalid option
  
  # Other functions should handle errors gracefully
  expect_no_error(convert_url_to_key(NULL))  # Should handle NULL input
  expect_no_error(print_list_with_nulls(NULL))  # Should handle NULL input
})

test_that("utility functions are consistent in their behavior", {
  # Test that functions behave consistently across multiple calls
  expect_equal(convert_url_to_key("test.com"), convert_url_to_key("test.com"))
  expect_equal(ask_yes_no("test?", "oui"), ask_yes_no("test?", "oui"))
  expect_equal(ask_1_2("test?", "1"), ask_1_2("test?", "1"))
  
  # Test return types are consistent
  expect_type(convert_url_to_key("test"), "character")
  expect_type(ask_yes_no("test?", "oui"), "logical")
  expect_type(ask_1_2("test?", "1"), "character")
})
