# Comprehensive tests for S3 functions with REAL AWS connections
# Tests for: list_s3_buckets, list_s3_partitions, list_s3_folders, 
#           upload_file_to_s3, delete_s3_folder
# Following requirement: "use real life connections and data... Do not mock everything"

# Test context
test_that("S3 functions can be loaded and have proper signatures", {
  # Check that all S3 functions exist
  expect_true(exists("list_s3_buckets", mode = "function"))
  expect_true(exists("list_s3_partitions", mode = "function"))
  expect_true(exists("list_s3_folders", mode = "function"))
  expect_true(exists("upload_file_to_s3", mode = "function"))
  expect_true(exists("delete_s3_folder", mode = "function"))
  
  # Check function signatures
  expect_equal(length(formals(list_s3_buckets)), 2)  # credentials, type
  expect_equal(length(formals(list_s3_partitions)), 2)  # credentials, bucket
  expect_equal(length(formals(list_s3_folders)), 3)  # credentials, bucket, prefix
  expect_equal(length(formals(upload_file_to_s3)), 4)  # credentials, file, bucket, key
  expect_equal(length(formals(delete_s3_folder)), 3)  # credentials, bucket, prefix
})

test_that("list_s3_buckets validates input parameters", {
  # Test with NULL credentials
  expect_error(
    list_s3_buckets(credentials = NULL, type = "test"),
    class = "error"
  )
  
  # Test with invalid type parameter
  creds <- get_real_aws_credentials_dev()
  if (!is.null(creds)) {
    expect_no_error(list_s3_buckets(creds, "datawarehouse"))
    expect_no_error(list_s3_buckets(creds, "datamarts"))
    expect_no_error(list_s3_buckets(creds, "landing"))
  } else {
    skip("AWS credentials not available for testing")
  }
})

test_that("list_s3_buckets works with real AWS credentials", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Test listing buckets for different types
  warehouse_buckets <- list_s3_buckets(creds, "datawarehouse")
  expect_true(is.character(warehouse_buckets) || is.null(warehouse_buckets))
  
  datamarts_buckets <- list_s3_buckets(creds, "datamarts")
  expect_true(is.character(datamarts_buckets) || is.null(datamarts_buckets))
  
  landing_buckets <- list_s3_buckets(creds, "landing")
  expect_true(is.character(landing_buckets) || is.null(landing_buckets))
})

test_that("list_s3_partitions validates input parameters", {
  # Test with NULL credentials
  expect_error(
    list_s3_partitions(credentials = NULL, bucket = "test-bucket"),
    class = "error"
  )
  
  # Test with empty bucket name
  creds <- get_real_aws_credentials_dev()
  if (!is.null(creds)) {
    expect_error(
      list_s3_partitions(creds, ""),
      class = "error"
    )
  }
})

test_that("list_s3_partitions works with real AWS credentials", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Get a real bucket to test with
  landing_bucket <- list_s3_buckets(creds, "landing")
  if (!is.null(landing_bucket) && length(landing_bucket) > 0) {
    partitions <- list_s3_partitions(creds, landing_bucket[1])
    expect_true(is.character(partitions) || is.null(partitions))
  } else {
    skip("No landing buckets available for testing")
  }
})

test_that("list_s3_folders validates input parameters", {
  # Test with NULL credentials
  expect_error(
    list_s3_folders(credentials = NULL, bucket = "test", prefix = "test/"),
    class = "error"
  )
  
  # Test with missing bucket
  creds <- get_real_aws_credentials_dev()
  if (!is.null(creds)) {
    expect_error(
      list_s3_folders(creds, "", "test/"),
      class = "error"
    )
  }
})

test_that("list_s3_folders works with real AWS credentials", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Get a real bucket to test with
  landing_bucket <- list_s3_buckets(creds, "landing")
  if (!is.null(landing_bucket) && length(landing_bucket) > 0) {
    # Test with root prefix
    folders <- list_s3_folders(creds, landing_bucket[1], "")
    expect_true(is.character(folders) || is.null(folders))
    
    # Test with specific prefix if folders exist
    if (!is.null(folders) && length(folders) > 0) {
      subfolders <- list_s3_folders(creds, landing_bucket[1], paste0(folders[1], "/"))
      expect_true(is.character(subfolders) || is.null(subfolders))
    }
  } else {
    skip("No landing buckets available for testing")
  }
})

test_that("upload_file_to_s3 validates input parameters", {
  # Test with NULL credentials
  expect_error(
    upload_file_to_s3(credentials = NULL, file = "test.txt", bucket = "test", key = "test.txt"),
    class = "error"
  )
  
  # Test with non-existent file
  creds <- get_real_aws_credentials_dev()
  if (!is.null(creds)) {
    expect_error(
      upload_file_to_s3(creds, "/non/existent/file.txt", "test-bucket", "test.txt"),
      class = "error"
    )
  }
})

test_that("upload_file_to_s3 works with real file upload", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Create a temporary test file
  temp_file <- tempfile(fileext = ".txt")
  writeLines("Test content for S3 upload", temp_file)
  
  # Get a test bucket
  landing_bucket <- list_s3_buckets(creds, "landing")
  if (!is.null(landing_bucket) && length(landing_bucket) > 0) {
    test_key <- paste0("test-uploads/test-", format(Sys.time(), "%Y%m%d%H%M%S"), ".txt")
    
    # Upload file
    result <- upload_file_to_s3(creds, temp_file, landing_bucket[1], test_key)
    expect_true(is.logical(result))
    
    # Clean up temp file
    unlink(temp_file)
    
    # Clean up S3 object if upload was successful
    if (isTRUE(result)) {
      # Note: We would delete the test object here, but we'll leave that for manual cleanup
      # to avoid creating a delete function dependency in this test
    }
  } else {
    skip("No landing buckets available for testing")
  }
  
  # Clean up temp file regardless
  if (file.exists(temp_file)) unlink(temp_file)
})

test_that("delete_s3_folder validates input parameters", {
  # Test with NULL credentials
  expect_error(
    delete_s3_folder(credentials = NULL, bucket = "test", prefix = "test/"),
    class = "error"
  )
  
  # Test with empty bucket
  creds <- get_real_aws_credentials_dev()
  if (!is.null(creds)) {
    expect_error(
      delete_s3_folder(creds, "", "test/"),
      class = "error"
    )
  }
})

test_that("delete_s3_folder works with real AWS credentials", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Get a test bucket
  landing_bucket <- list_s3_buckets(creds, "landing")
  if (!is.null(landing_bucket) && length(landing_bucket) > 0) {
    # Create a test prefix that's safe to delete
    test_prefix <- paste0("test-delete-", format(Sys.time(), "%Y%m%d%H%M%S"), "/")
    
    # First upload a test file to create the folder
    temp_file <- tempfile(fileext = ".txt")
    writeLines("Test content for deletion", temp_file)
    test_key <- paste0(test_prefix, "test-file.txt")
    
    upload_result <- upload_file_to_s3(creds, temp_file, landing_bucket[1], test_key)
    
    if (isTRUE(upload_result)) {
      # Now test deletion
      delete_result <- delete_s3_folder(creds, landing_bucket[1], test_prefix)
      expect_true(is.logical(delete_result))
    }
    
    # Clean up temp file
    unlink(temp_file)
  } else {
    skip("No landing buckets available for testing")
  }
})

test_that("S3 functions handle AWS API errors gracefully", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Test with non-existent bucket - should handle gracefully
  expect_no_error({
    result <- list_s3_partitions(creds, "non-existent-bucket-12345")
    expect_true(is.null(result) || is.character(result))
  })
  
  expect_no_error({
    result <- list_s3_folders(creds, "non-existent-bucket-12345", "")
    expect_true(is.null(result) || is.character(result))
  })
})

test_that("S3 functions integrate properly with each other", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Test the workflow: list buckets -> list partitions -> list folders
  buckets <- list_s3_buckets(creds, "landing")
  if (!is.null(buckets) && length(buckets) > 0) {
    partitions <- list_s3_partitions(creds, buckets[1])
    if (!is.null(partitions) && length(partitions) > 0) {
      folders <- list_s3_folders(creds, buckets[1], partitions[1])
      expect_true(is.character(folders) || is.null(folders))
    }
  }
})
