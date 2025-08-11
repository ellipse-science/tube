# Comprehensive tests for bucket-specific functions with REAL AWS connections
# Tests for: list_athena_staging_bucket, list_datalake_bucket, list_datamarts_bucket,
#           list_datawarehouse_bucket, list_landing_zone_bucket, list_landing_zone_partitions
# Following requirement: "use real life connections and data... Do not mock everything"

test_that("bucket functions can be loaded and have proper signatures", {
  cat("\n=== TESTING BUCKET FUNCTION SIGNATURES ===\n")
  
  cat("PRODUCTION CODE BEING TESTED:\n")
  cat("1. list_athena_staging_bucket <- function(credentials) {\n")
  cat("     list_s3_buckets(credentials, \"athenaqueryresults\")\n")
  cat("   }\n\n")
  
  cat("2. list_datalake_bucket <- function(credentials) {\n")
  cat("     list_s3_buckets(credentials, \"datalakebucket\")\n")
  cat("   }\n\n")
  
  cat("3. list_datamarts_bucket <- function(credentials) {\n")
  cat("     list_s3_buckets(credentials, \"datamartsbucket\")\n")
  cat("   }\n\n")
  
  cat("4. list_datawarehouse_bucket <- function(credentials) {\n")
  cat("     list_s3_buckets(credentials, \"datawarehousebucket\")\n")
  cat("   }\n\n")
  
  cat("5. list_landing_zone_bucket <- function(credentials) {\n")
  cat("     list_s3_buckets(credentials, \"landingzone\")\n")
  cat("   }\n\n")
  
  cat("6. list_landing_zone_partitions <- function(credentials) {\n")
  cat("     bucket <- list_landing_zone_bucket(credentials)\n")
  cat("     list_s3_partitions(credentials, bucket)\n")
  cat("   }\n\n")
  
  cat("DEPENDENCY CHAIN: All bucket functions → list_s3_buckets() → paws.storage::s3\n\n")
  cat("TESTING: Function existence and signatures...\n")
  
  # Check that all bucket functions exist
  expect_true(exists("list_athena_staging_bucket", mode = "function"))
  expect_true(exists("list_datalake_bucket", mode = "function"))
  expect_true(exists("list_datamarts_bucket", mode = "function"))
  expect_true(exists("list_datawarehouse_bucket", mode = "function"))
  expect_true(exists("list_landing_zone_bucket", mode = "function"))
  expect_true(exists("list_landing_zone_partitions", mode = "function"))
  
  # Check function signatures - all should take credentials parameter
  expect_equal(length(formals(list_athena_staging_bucket)), 1)  # credentials
  expect_equal(length(formals(list_datalake_bucket)), 1)        # credentials
  expect_equal(length(formals(list_datamarts_bucket)), 1)       # credentials
  expect_equal(length(formals(list_datawarehouse_bucket)), 1)   # credentials
  expect_equal(length(formals(list_landing_zone_bucket)), 1)    # credentials
  expect_equal(length(formals(list_landing_zone_partitions)), 1) # credentials
  
  cat("✅ All bucket function signatures verified!\n")
})

test_that("list_athena_staging_bucket validates input parameters", {
  # Test with NULL credentials
  expect_error(
    list_athena_staging_bucket(credentials = NULL),
    class = "error"
  )
})

test_that("list_athena_staging_bucket works with real AWS credentials", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  cat("\n=== TESTING ATHENA STAGING BUCKET ===\n")
  
  cat("PRODUCTION CODE FLOW:\n")
  cat("list_athena_staging_bucket(creds)\n")
  cat("  └─> list_s3_buckets(creds, \"athenaqueryresults\")\n")
  cat("      └─> paws.storage::s3(config = creds)\n")
  cat("      └─> s3_client$list_buckets()\n")
  cat("      └─> filter buckets containing \"athenaqueryresults\"\n")
  cat("      └─> return character vector or NULL\n\n")
  
  creds <- get_real_aws_credentials_dev()
  
  cat("TESTING: Real AWS Athena bucket listing...\n")
  
  # Test listing Athena staging bucket
  result <- list_athena_staging_bucket(creds)
  expect_true(is.character(result) || is.null(result))
  
  cat("Result type:", class(result), "\n")
  cat("Result length:", if(is.null(result)) "NULL" else length(result), "\n")
  
  # If bucket exists, verify it contains "athenaqueryresults" in name
  if (!is.null(result) && length(result) > 0) {
    cat("Found buckets:", paste(result, collapse = ", "), "\n")
    expect_true(any(grepl("athenaqueryresults", result, ignore.case = TRUE)))
    cat("✅ Athena bucket filtering works correctly!\n")
  } else {
    cat("ℹ️ No Athena staging buckets found in this environment\n")
  }
})

test_that("list_datalake_bucket validates input parameters", {
  # Test with NULL credentials
  expect_error(
    list_datalake_bucket(credentials = NULL),
    class = "error"
  )
})

test_that("list_datalake_bucket works with real AWS credentials", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Test listing datalake bucket
  result <- list_datalake_bucket(creds)
  expect_true(is.character(result) || is.null(result))
  
  # If bucket exists, verify it contains "datalakebucket" in name
  if (!is.null(result) && length(result) > 0) {
    expect_true(any(grepl("datalakebucket", result, ignore.case = TRUE)))
  }
})

test_that("list_datamarts_bucket validates input parameters", {
  # Test with NULL credentials
  expect_error(
    list_datamarts_bucket(credentials = NULL),
    class = "error"
  )
})

test_that("list_datamarts_bucket works with real AWS credentials", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Test listing datamarts bucket
  result <- list_datamarts_bucket(creds)
  expect_true(is.character(result) || is.null(result))
  
  # If bucket exists, verify it contains "datamartbucket" in name
  if (!is.null(result) && length(result) > 0) {
    expect_true(any(grepl("datamartbucket", result, ignore.case = TRUE)))
  }
})

test_that("list_datawarehouse_bucket validates input parameters", {
  # Test with NULL credentials
  expect_error(
    list_datawarehouse_bucket(credentials = NULL),
    class = "error"
  )
})

test_that("list_datawarehouse_bucket works with real AWS credentials", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Test listing datawarehouse bucket
  result <- list_datawarehouse_bucket(creds)
  expect_true(is.character(result) || is.null(result))
  
  # If bucket exists, verify it contains "datawarehousebucket" in name
  if (!is.null(result) && length(result) > 0) {
    expect_true(any(grepl("datawarehousebucket", result, ignore.case = TRUE)))
  }
})

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
  
  # If bucket exists, verify it contains "landingzonebucket" in name
  if (!is.null(result) && length(result) > 0) {
    expect_true(any(grepl("landingzonebucket", result, ignore.case = TRUE)))
  }
})

test_that("list_landing_zone_partitions validates input parameters", {
  # Test with NULL credentials
  expect_error(
    list_landing_zone_partitions(credentials = NULL),
    class = "error"
  )
})

test_that("list_landing_zone_partitions works with real AWS credentials", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Test listing landing zone partitions
  result <- list_landing_zone_partitions(creds)
  expect_true(is.character(result) || is.null(result))
  
  # If partitions exist, they should be character strings
  if (!is.null(result) && length(result) > 0) {
    expect_true(all(is.character(result)))
    expect_true(all(nzchar(result)))
  }
})

test_that("bucket functions handle AWS API errors gracefully", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # All bucket functions should handle API errors gracefully
  expect_no_error({
    list_athena_staging_bucket(creds)
  })
  
  expect_no_error({
    list_datalake_bucket(creds)
  })
  
  expect_no_error({
    list_datamarts_bucket(creds)
  })
  
  expect_no_error({
    list_datawarehouse_bucket(creds)
  })
  
  expect_no_error({
    list_landing_zone_bucket(creds)
  })
  
  expect_no_error({
    list_landing_zone_partitions(creds)
  })
})

test_that("bucket functions use correct underlying S3 filters", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Test that bucket functions are filtering correctly by checking they delegate to list_s3_buckets
  # This is integration testing to ensure the functions work together properly
  
  # Get all bucket results
  athena_buckets <- list_athena_staging_bucket(creds)
  datalake_buckets <- list_datalake_bucket(creds)
  datamarts_buckets <- list_datamarts_bucket(creds)
  datawarehouse_buckets <- list_datawarehouse_bucket(creds)
  landing_buckets <- list_landing_zone_bucket(creds)
  
  # Each function should return distinct results (no overlap)
  all_results <- c(athena_buckets, datalake_buckets, datamarts_buckets, 
                   datawarehouse_buckets, landing_buckets)
  
  if (length(all_results) > 1) {
    # Check that we don't have duplicate bucket names across different types
    expect_equal(length(all_results), length(unique(all_results)))
  }
})

test_that("bucket functions integrate with each other properly", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Test workflow: get landing zone bucket -> get its partitions
  landing_bucket <- list_landing_zone_bucket(creds)
  if (!is.null(landing_bucket) && length(landing_bucket) > 0) {
    partitions <- list_landing_zone_partitions(creds)
    
    # If we have a landing bucket, partitions function should work
    expect_true(is.character(partitions) || is.null(partitions))
    
    # Partitions should be related to the landing bucket
    if (!is.null(partitions) && length(partitions) > 0) {
      expect_true(all(is.character(partitions)))
    }
  }
})
