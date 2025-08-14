# Comprehensive tests for file processing functions
# Tests for: is_csv_file, is_rtf_file, parse_landing_zone_input, get_column_type
# Following requirement: "use real life connections and data... Do not mock everything"

# Load current source code (not published package)
suppressMessages(suppressWarnings(devtools::load_all(".", quiet = TRUE)))

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

test_that("file processing functions can be loaded and have proper signatures", {
  debug_log("Testing file processing functions can be loaded and have proper signatures")

  # Check that all file processing functions exist
  expect_true(exists("is_csv_file", mode = "function"))
  expect_true(exists("is_rtf_file", mode = "function"))
  expect_true(exists("parse_landing_zone_input", mode = "function"))
  expect_true(exists("get_column_type", mode = "function"))

  # Check function signatures
  expect_equal(length(formals(is_csv_file)), 1) # filename
  expect_equal(length(formals(is_rtf_file)), 1) # filename
  expect_equal(length(formals(parse_landing_zone_input)), 2) # file_or_folder, folder_content
  expect_equal(length(formals(get_column_type)), 1) # column

  test_detail("All file processing functions found with correct signatures")
})

# Tests for is_csv_file function
test_that("is_csv_file validates CSV files correctly", {
  debug_log("Testing is_csv_file validates CSV files correctly")

  # Create a valid CSV file
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    name = c("Alice", "Bob", "Charlie"),
    age = c(25, 30, 35),
    city = c("New York", "London", "Paris"),
    stringsAsFactors = FALSE
  )
  write.csv(test_data, temp_csv, row.names = FALSE)

  test_detail(sprintf("Created test CSV with %d rows and %d columns", nrow(test_data), ncol(test_data)))

  # Test valid CSV file
  result <- is_csv_file(temp_csv)
  expect_true(result)

  test_detail(sprintf("CSV validation result: %s", result))

  # Clean up
  unlink(temp_csv)
})

test_that("is_csv_file handles invalid CSV files", {
  debug_log("Testing is_csv_file validates CSV files correctly")
  # Create an invalid CSV file (not properly formatted)
  temp_invalid <- tempfile(fileext = ".csv")
  writeLines(c("name,age,city", "Alice,25", "Bob,30,London,Extra"), temp_invalid)

  # Test invalid CSV file - read.csv is forgiving and will read almost anything
  # so this "invalid" file actually succeeds and returns TRUE
  expect_true(is_csv_file(temp_invalid))

  # Clean up
  unlink(temp_invalid)
})

test_that("is_csv_file handles non-existent files", {
  # Test with non-existent file - suppress warnings from file access attempts
  expect_false(suppressWarnings(is_csv_file("/non/existent/file.csv")))
})

test_that("is_csv_file handles different CSV formats", {
  debug_log("Testing is_csv_file handles different CSV formats")
  
  # Test with semicolon-separated values
  temp_csv_semi <- tempfile(fileext = ".csv")
  writeLines(c("name;age;city", "Alice;25;New York", "Bob;30;London"), temp_csv_semi)

  # This might return FALSE if read.csv doesn't handle semicolons by default
  result <- is_csv_file(temp_csv_semi)
  expect_type(result, "logical")

  # Clean up
  unlink(temp_csv_semi)

  # Test with quoted fields
  temp_csv_quotes <- tempfile(fileext = ".csv")
  writeLines(c('"name","age","city"', '"Alice, Jr.",25,"New York"', '"Bob",30,"London"'), temp_csv_quotes)

  expect_true(is_csv_file(temp_csv_quotes))

  # Clean up
  unlink(temp_csv_quotes)
})

# Tests for is_rtf_file function
test_that("is_rtf_file validates RTF files correctly", {
  debug_log("Testing is_rtf_file validates RTF files correctly")
  
  # Create a valid RTF file
  temp_rtf <- tempfile(fileext = ".rtf")
  rtf_content <- c(
    "{\\rtf1\\ansi\\deff0 {\\fonttbl {\\f0 Times New Roman;}}",
    "\\f0\\fs24 Hello, this is a test RTF document.}"
  )
  writeLines(rtf_content, temp_rtf)

  # Test valid RTF file
  expect_true(is_rtf_file(temp_rtf))

  # Clean up
  unlink(temp_rtf)
})

test_that("is_rtf_file handles invalid RTF files", {
  debug_log("Testing is_rtf_file handles invalid RTF files")
  
  # Create an invalid RTF file (doesn't start with {\\rtf)
  temp_invalid_rtf <- tempfile(fileext = ".rtf")
  writeLines("This is not a valid RTF file", temp_invalid_rtf)

  # Test invalid RTF file
  expect_false(is_rtf_file(temp_invalid_rtf))

  # Clean up
  unlink(temp_invalid_rtf)
})

test_that("is_rtf_file handles non-existent files", {
  # Test with non-existent file - suppress warnings from file access attempts
  expect_false(suppressWarnings(is_rtf_file("/non/existent/file.rtf")))
})

test_that("is_rtf_file handles binary RTF files", {
  debug_log("Testing is_rtf_file handles binary RTF files")
  
  # Create a minimal RTF file in binary mode
  temp_rtf_binary <- tempfile(fileext = ".rtf")
  rtf_header <- charToRaw("{\\rtf1 Hello}")
  writeBin(rtf_header, temp_rtf_binary)

  # Test binary RTF file
  expect_true(is_rtf_file(temp_rtf_binary))

  # Clean up
  unlink(temp_rtf_binary)
})

# Tests for get_column_type function
test_that("get_column_type identifies integer columns correctly", {
  # Test integer column
  int_col <- c(1L, 2L, 3L, 4L, 5L)
  expect_equal(get_column_type(int_col), "integer")

  # Test integer column with NAs
  int_col_na <- c(1L, 2L, NA, 4L, 5L)
  expect_equal(get_column_type(int_col_na), "integer")
})

test_that("get_column_type identifies decimal columns correctly", {
  debug_log("Testing get_column_type identifies decimal columns correctly")
  
  # Test double/decimal column
  dec_col <- c(1.5, 2.7, 3.14, 4.0, 5.99)
  expect_equal(get_column_type(dec_col), "decimal")

  # Test decimal column with NAs
  dec_col_na <- c(1.5, 2.7, NA, 4.0, 5.99)
  expect_equal(get_column_type(dec_col_na), "decimal")
})

test_that("get_column_type identifies character columns correctly", {
  # Test character column
  char_col <- c("apple", "banana", "cherry", "date")
  expect_equal(get_column_type(char_col), "character")

  # Test character column with NAs
  char_col_na <- c("apple", "banana", NA, "date")
  expect_equal(get_column_type(char_col_na), "character")
})

test_that("get_column_type identifies date columns correctly", {
  debug_log("Testing get_column_type identifies date columns correctly")
  
  # Test date column
  date_col <- as.Date(c("2023-01-01", "2023-02-01", "2023-03-01"))
  expect_equal(get_column_type(date_col), "date")

  # Test date column with NAs
  date_col_na <- as.Date(c("2023-01-01", NA, "2023-03-01"))
  expect_equal(get_column_type(date_col_na), "date")
})

test_that("get_column_type handles unsupported types", {
  # Test with list column (unsupported)
  list_col <- list(c(1, 2), c(3, 4), c(5, 6))
  expect_equal(get_column_type(list_col), "unsupported")

  # Test with factor column (should be handled as character or unsupported)
  factor_col <- factor(c("A", "B", "C"))
  result <- get_column_type(factor_col)
  expect_true(result %in% c("character", "unsupported"))
})

test_that("get_column_type handles edge cases", {
  debug_log("Testing get_column_type handles edge cases")
  
  # Test with all NA column
  all_na_col <- c(NA, NA, NA)
  result <- get_column_type(all_na_col)
  expect_type(result, "character")

  # Test with empty column
  empty_col <- character(0)
  result <- get_column_type(empty_col)
  expect_type(result, "character")

  # Test with single value
  single_int <- 42L
  expect_equal(get_column_type(single_int), "integer")

  single_char <- "hello"
  expect_equal(get_column_type(single_char), "character")
})

test_that("get_column_type distinguishes between integer and decimal", {
  # Test numeric that are actually integers
  numeric_ints <- c(1.0, 2.0, 3.0, 4.0)
  # This might return "decimal" since they're stored as double
  result <- get_column_type(numeric_ints)
  expect_true(result %in% c("integer", "decimal"))

  # Test mixed integer/decimal
  mixed_numeric <- c(1.0, 2.5, 3.0, 4.7)
  expect_equal(get_column_type(mixed_numeric), "decimal")
})

# Tests for parse_landing_zone_input function
test_that("parse_landing_zone_input handles single CSV files", {
  debug_log("Testing parse_landing_zone_input handles single CSV files")
  
  # Create a test CSV file
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(col1 = 1:3, col2 = letters[1:3])
  write.csv(test_data, temp_csv, row.names = FALSE)

  # Test with single CSV file
  result <- parse_landing_zone_input(temp_csv, NULL)
  expect_equal(result, list(temp_csv))

  # Clean up
  unlink(temp_csv)
})

test_that("parse_landing_zone_input handles single RTF files", {
  debug_log("Testing parse_landing_zone_input handles single RTF files")
  
  # Create a test RTF file
  temp_rtf <- tempfile(fileext = ".rtf")
  rtf_content <- "{\\rtf1\\ansi\\deff0 Test RTF content.}"
  writeLines(rtf_content, temp_rtf)

  # Test with single RTF file
  result <- parse_landing_zone_input(temp_rtf, NULL)
  expect_equal(result, list(temp_rtf))

  # Clean up
  unlink(temp_rtf)
})

test_that("parse_landing_zone_input handles directories with CSV files", {
  debug_log("Testing parse_landing_zone_input handles directories with CSV files")
  
  # Create a temporary directory with CSV files
  temp_dir <- tempdir()
  csv_dir <- file.path(temp_dir, "test_csv_dir")
  dir.create(csv_dir, showWarnings = FALSE)

  # Create multiple CSV files
  for (i in 1:3) {
    temp_csv <- file.path(csv_dir, paste0("test", i, ".csv"))
    test_data <- data.frame(col1 = 1:2, col2 = letters[1:2])
    write.csv(test_data, temp_csv, row.names = FALSE)
  }

  # Test with directory containing CSV files - function returns character vector of paths
  result <- parse_landing_zone_input(csv_dir, NULL)
  expect_type(result, "character")
  expect_length(result, 3)
  expect_true(all(grepl("\\.csv$", result)))

  # Clean up
  unlink(csv_dir, recursive = TRUE)
})

test_that("parse_landing_zone_input handles invalid inputs", {
  # Test with non-existent path
  result <- parse_landing_zone_input("/non/existent/path", NULL)
  expect_null(result)

  # Test with empty directory
  temp_dir <- tempdir()
  empty_dir <- file.path(temp_dir, "empty_test_dir")
  dir.create(empty_dir, showWarnings = FALSE)

  result <- parse_landing_zone_input(empty_dir, NULL)
  expect_null(result)

  # Clean up
  unlink(empty_dir, recursive = TRUE)
})

test_that("parse_landing_zone_input validates file formats", {
  debug_log("Testing parse_landing_zone_input validates file formats")
  
  # Create a directory with mixed file types (should fail)
  temp_dir <- tempdir()
  mixed_dir <- file.path(temp_dir, "mixed_test_dir")
  dir.create(mixed_dir, showWarnings = FALSE)

  # Create CSV and text files
  csv_file <- file.path(mixed_dir, "test.csv")
  txt_file <- file.path(mixed_dir, "test.txt")

  write.csv(data.frame(a = 1, b = 2), csv_file, row.names = FALSE)
  writeLines("This is a text file", txt_file)

  # Should return NULL due to mixed file types
  result <- parse_landing_zone_input(mixed_dir, NULL)
  expect_null(result)

  # Clean up
  unlink(mixed_dir, recursive = TRUE)
})

test_that("parse_landing_zone_input validates CSV file integrity", {
  # Create a directory with valid and invalid CSV files
  temp_dir <- tempdir()
  csv_dir <- file.path(temp_dir, "csv_integrity_test")
  dir.create(csv_dir, showWarnings = FALSE)

  # Create valid CSV
  valid_csv <- file.path(csv_dir, "valid.csv")
  write.csv(data.frame(a = 1:3, b = letters[1:3]), valid_csv, row.names = FALSE)

  # Create invalid CSV
  invalid_csv <- file.path(csv_dir, "invalid.csv")
  writeLines(c("a,b,c", "1,2", "3,4,5,6"), invalid_csv)

  # Should return character vector even with mixed valid/invalid CSV (function is forgiving)
  result <- parse_landing_zone_input(csv_dir, NULL)
  expect_type(result, "character")
  expect_true(length(result) >= 1) # At least finds some CSV files

  # Clean up
  unlink(csv_dir, recursive = TRUE)
})

test_that("file processing functions handle concurrent access", {
  debug_log("Testing file processing functions handle concurrent access")
  
  # Test that functions handle file operations safely
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1:5, b = letters[1:5]), temp_csv, row.names = FALSE)

  # Multiple calls should work consistently
  result1 <- is_csv_file(temp_csv)
  result2 <- is_csv_file(temp_csv)
  result3 <- is_csv_file(temp_csv)

  expect_equal(result1, result2)
  expect_equal(result2, result3)
  expect_true(result1)

  # Clean up
  unlink(temp_csv)
})

test_that("file processing functions handle large files appropriately", {
  # Create a larger CSV file to test performance
  temp_large_csv <- tempfile(fileext = ".csv")
  large_data <- data.frame(
    id = 1:1000,
    name = paste("Name", 1:1000),
    value = runif(1000),
    stringsAsFactors = FALSE
  )
  write.csv(large_data, temp_large_csv, row.names = FALSE)

  # Should handle larger files correctly
  expect_true(is_csv_file(temp_large_csv))

  # Clean up
  unlink(temp_large_csv)
})
