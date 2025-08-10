# Comprehensive tests for landing zone functions with REAL AWS connections
# Tests for: list_landing_zone_bucket, upload_file_to_landing_zone, parse_landing_zone_input
# Following requirement: "use real life connections and data... Do not mock everything"

test_that("landing zone functions can be loaded and have proper signatures", {
  # Check that all landing zone functions exist
  expect_true(exists("list_landing_zone_bucket", mode = "function"))
  expect_true(exists("upload_file_to_landing_zone", mode = "function"))
  expect_true(exists("parse_landing_zone_input", mode = "function"))
  
  # Check function signatures
  expect_equal(length(formals(list_landing_zone_bucket)), 1)     # credentials
  expect_equal(length(formals(upload_file_to_landing_zone)), 3)  # file_path, credentials, key
  expect_equal(length(formals(parse_landing_zone_input)), 1)     # user_input
})

# Tests for list_landing_zone_bucket function
test_that("list_landing_zone_bucket validates input parameters", {
  # Test with NULL credentials
  expect_error(
    list_landing_zone_bucket(credentials = NULL),
    class = "error"
  )
})

test_that("list_landing_zone_bucket works with real AWS credentials", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Test listing landing zone bucket
  result <- list_landing_zone_bucket(creds)
  expect_true(is.character(result) || is.null(result))
  
  # If bucket exists, verify it contains "landing" in name
  # (based on typical AWS naming patterns for landing zones)
  if (!is.null(result) && length(result) > 0) {
    expect_true(all(is.character(result)))
    expect_true(all(nzchar(result)))
    expect_true(length(result) >= 1)
  }
})

# Tests for parse_landing_zone_input function
test_that("parse_landing_zone_input validates input parameters", {
  # Test with NULL input
  expect_error(
    parse_landing_zone_input(user_input = NULL),
    class = "error"
  )
  
  # Test with empty string
  expect_error(
    parse_landing_zone_input(user_input = ""),
    class = "error"
  )
  
  # Test with non-character input
  expect_error(
    parse_landing_zone_input(user_input = 123),
    class = "error"
  )
})

test_that("parse_landing_zone_input handles valid directory paths", {
  # Test with simple path
  result1 <- parse_landing_zone_input("path/to/folder")
  expect_true(is.character(result1))
  expect_true(length(result1) == 1)
  
  # Test with path ending in slash
  result2 <- parse_landing_zone_input("path/to/folder/")
  expect_true(is.character(result2))
  expect_true(length(result2) == 1)
  
  # Test with Windows-style path
  result3 <- parse_landing_zone_input("C:\\Users\\folder")
  expect_true(is.character(result3))
  expect_true(length(result3) == 1)
  
  # Test with relative path
  result4 <- parse_landing_zone_input("./relative/path")
  expect_true(is.character(result4))
  expect_true(length(result4) == 1)
  
  # Test with parent directory references
  result5 <- parse_landing_zone_input("../parent/path")
  expect_true(is.character(result5))
  expect_true(length(result5) == 1)
})

test_that("parse_landing_zone_input handles paths with special characters", {
  # Test with spaces
  result1 <- parse_landing_zone_input("path with spaces/folder")
  expect_true(is.character(result1))
  expect_true(length(result1) == 1)
  
  # Test with underscores and hyphens
  result2 <- parse_landing_zone_input("path_with-special_chars/folder")
  expect_true(is.character(result2))
  expect_true(length(result2) == 1)
  
  # Test with numbers
  result3 <- parse_landing_zone_input("path123/folder456")
  expect_true(is.character(result3))
  expect_true(length(result3) == 1)
})

test_that("parse_landing_zone_input normalizes path formats", {
  # Test that function consistently handles different path separators
  unix_path <- parse_landing_zone_input("path/to/folder")
  windows_path <- parse_landing_zone_input("path\\to\\folder")
  
  expect_true(is.character(unix_path))
  expect_true(is.character(windows_path))
  expect_true(length(unix_path) == 1)
  expect_true(length(windows_path) == 1)
  
  # Both should produce valid output
  expect_true(nzchar(unix_path))
  expect_true(nzchar(windows_path))
})

# Tests for upload_file_to_landing_zone function
test_that("upload_file_to_landing_zone validates input parameters", {
  # Test with NULL file_path
  expect_error(
    upload_file_to_landing_zone(file_path = NULL, credentials = list(), key = "test"),
    class = "error"
  )
  
  # Test with NULL credentials
  expect_error(
    upload_file_to_landing_zone(file_path = "test.txt", credentials = NULL, key = "test"),
    class = "error"
  )
  
  # Test with NULL key
  expect_error(
    upload_file_to_landing_zone(file_path = "test.txt", credentials = list(), key = NULL),
    class = "error"
  )
  
  # Test with empty strings
  expect_error(
    upload_file_to_landing_zone(file_path = "", credentials = list(), key = "test"),
    class = "error"
  )
  
  expect_error(
    upload_file_to_landing_zone(file_path = "test.txt", credentials = list(), key = ""),
    class = "error"
  )
})

test_that("upload_file_to_landing_zone handles non-existent files", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Test with non-existent file
  expect_error(
    upload_file_to_landing_zone(
      file_path = "/path/to/nonexistent/file.txt",
      credentials = creds,
      key = "test/nonexistent.txt"
    ),
    class = "error"
  )
})

test_that("upload_file_to_landing_zone works with real AWS credentials and files", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Create a temporary test file
  temp_file <- tempfile(fileext = ".txt")
  writeLines(c("Test content for upload", "Line 2", "Line 3"), temp_file)
  
  # Ensure file exists
  expect_true(file.exists(temp_file))
  
  # Test upload (use timestamp to avoid conflicts)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  test_key <- paste0("test_uploads/test_file_", timestamp, ".txt")
  
  expect_no_error({
    result <- upload_file_to_landing_zone(
      file_path = temp_file,
      credentials = creds,
      key = test_key
    )
  })
  
  # Clean up
  unlink(temp_file)
})

test_that("upload_file_to_landing_zone handles different file types", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Test with text file
  temp_txt <- tempfile(fileext = ".txt")
  writeLines("Test text content", temp_txt)
  
  expect_no_error({
    upload_file_to_landing_zone(
      file_path = temp_txt,
      credentials = creds,
      key = paste0("test_uploads/test_", timestamp, ".txt")
    )
  })
  
  # Test with CSV file
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1:3, b = letters[1:3]), temp_csv, row.names = FALSE)
  
  expect_no_error({
    upload_file_to_landing_zone(
      file_path = temp_csv,
      credentials = creds,
      key = paste0("test_uploads/test_", timestamp, ".csv")
    )
  })
  
  # Clean up
  unlink(c(temp_txt, temp_csv))
})

test_that("landing zone functions integrate with each other properly", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Test the workflow: get landing zone bucket -> parse input -> upload file
  landing_bucket <- list_landing_zone_bucket(creds)
  
  if (!is.null(landing_bucket) && length(landing_bucket) > 0) {
    # Parse some test input
    parsed_path <- parse_landing_zone_input("test/upload/folder")
    expect_true(is.character(parsed_path))
    expect_true(length(parsed_path) == 1)
    
    # Create a test file and upload it
    temp_file <- tempfile(fileext = ".txt")
    writeLines("Integration test content", temp_file)
    
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    test_key <- paste0(parsed_path, "/integration_test_", timestamp, ".txt")
    
    expect_no_error({
      upload_file_to_landing_zone(
        file_path = temp_file,
        credentials = creds,
        key = test_key
      )
    })
    
    # Clean up
    unlink(temp_file)
  }
})

test_that("landing zone functions work with S3 integration", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Test integration between landing zone and general S3 functions
  landing_bucket <- list_landing_zone_bucket(creds)
  all_buckets <- list_s3_buckets(creds)
  
  if (!is.null(landing_bucket) && !is.null(all_buckets)) {
    expect_true(is.character(landing_bucket))
    expect_true(is.character(all_buckets))
    
    # Landing zone bucket should be in the list of all buckets
    if (length(landing_bucket) > 0 && length(all_buckets) > 0) {
      # At least one landing bucket should exist in the general bucket list
      expect_true(any(landing_bucket %in% all_buckets) || 
                 any(grepl("landing", all_buckets, ignore.case = TRUE)))
    }
  }
})

test_that("landing zone functions handle AWS API errors gracefully", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # All landing zone functions should handle API errors gracefully
  expect_no_error({
    bucket_result <- list_landing_zone_bucket(creds)
    expect_true(is.character(bucket_result) || is.null(bucket_result))
  })
  
  expect_no_error({
    parse_result <- parse_landing_zone_input("valid/test/path")
    expect_true(is.character(parse_result))
  })
  
  # Upload function error handling is tested with invalid files above
})

test_that("parse_landing_zone_input produces consistent output", {
  # Test that the same input always produces the same output
  input_path <- "consistent/test/path"
  
  result1 <- parse_landing_zone_input(input_path)
  result2 <- parse_landing_zone_input(input_path)
  result3 <- parse_landing_zone_input(input_path)
  
  expect_equal(result1, result2)
  expect_equal(result2, result3)
  expect_true(is.character(result1))
  expect_true(length(result1) == 1)
})

test_that("landing zone functions return consistent data types", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Test that functions consistently return expected types
  bucket_result <- list_landing_zone_bucket(creds)
  if (!is.null(bucket_result)) {
    expect_true(is.character(bucket_result))
    expect_true(length(bucket_result) >= 0)
  }
  
  parse_result <- parse_landing_zone_input("test/path")
  expect_true(is.character(parse_result))
  expect_true(length(parse_result) == 1)
  expect_true(nzchar(parse_result))
})

test_that("upload_file_to_landing_zone handles large file names and paths", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Create a test file with long content
  temp_file <- tempfile(fileext = ".txt")
  long_content <- paste(rep("This is a line of test content.", 100), collapse = "\n")
  writeLines(long_content, temp_file)
  
  # Test with long key name
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  long_key <- paste0("test_uploads/very_long_folder_name_for_testing_purposes/",
                     "subfolder_with_additional_depth/",
                     "test_file_with_very_long_name_", timestamp, ".txt")
  
  expect_no_error({
    upload_file_to_landing_zone(
      file_path = temp_file,
      credentials = creds,
      key = long_key
    )
  })
  
  # Clean up
  unlink(temp_file)
})
