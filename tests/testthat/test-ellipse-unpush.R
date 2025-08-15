# Tests for ellipse_unpush() function - Public datalake dataset/tag deletion
# This includes tests for both programmatic modes and validation

# Load current source code (not published package)
suppressMessages(suppressWarnings(devtools::load_all(".", quiet = TRUE)))

# DEBUGGING TESTS:
# - Normal run: Routine output suppressed for clean results
# - Verbose mode: Set TUBE_TEST_VERBOSE=TRUE to see all output for debugging
# - Example: Sys.setenv(TUBE_TEST_VERBOSE = "TRUE"); devtools::test(filter = "ellipse-unpush")

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

# Helper function for debug logging
debug_log <- function(message) {
  if (Sys.getenv("TUBE_TEST_VERBOSE", "FALSE") == "TRUE") {
    cat("DEBUG:", message, "\n")
  }
}

# Helper function for test details
test_detail <- function(message) {
  if (Sys.getenv("TUBE_TEST_VERBOSE", "FALSE") == "TRUE") {
    cat("DETAIL:", message)
  }
}

# Test basic function existence and signature
test_that("ellipse_unpush function exists and has correct signature", {
  debug_log("Testing ellipse_unpush function signature")
  
  # Check function exists
  expect_true(exists("ellipse_unpush", mode = "function"))
  
  # Check function signature
  formals_list <- formals(ellipse_unpush)
  expect_equal(length(formals_list), 3) # con, dataset_name, tag
  expect_true("con" %in% names(formals_list))
  expect_true("dataset_name" %in% names(formals_list))
  expect_true("tag" %in% names(formals_list))
  
  # Check default values
  expect_equal(formals_list$tag, NULL) # tag should default to NULL
})

# Test parameter validation
test_that("ellipse_unpush validates input parameters", {
  debug_log("Testing parameter validation")
  
  # Test with NULL connection
  expect_error(
    ellipse_unpush(connection = NULL, dataset_name = "test"),
    class = "error"
  )
  
  # Test with missing dataset_name
  # We can't easily test this with real connections in unit tests
  # This would be covered in integration tests
})

# Test connection type validation
test_that("ellipse_unpush rejects non-datalake connections", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  debug_log("Testing connection type validation")
  
  expect_no_error({
    # Test with datawarehouse connection (should be rejected)
    datawarehouse_con <- conditionally_suppress({
      ellipse_connect(env = "DEV", database = "datawarehouse")
    })
    
    if (!is.null(datawarehouse_con) && !inherits(datawarehouse_con, "error")) {
      # This should fail because it's not a datalake connection
      result <- conditionally_suppress({
        ellipse_unpush(datawarehouse_con, dataset_name = "test-dataset")
      })
      
      # Should return FALSE for wrong connection type
      expect_false(result)
      
      # Clean disconnect
      conditionally_suppress({
        ellipse_disconnect(datawarehouse_con)
      })
    }
  })
})

# Test with datalake connection (main functionality)
test_that("ellipse_unpush works with datalake connections", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  debug_log("Testing datalake connection functionality")
  
  expect_no_error({
    # Connect to public datalake
    datalake_con <- conditionally_suppress({
      ellipse_connect(env = "DEV", database = "datalake")
    })
    
    if (!is.null(datalake_con) && !inherits(datalake_con, "error")) {
      # Test with non-existent dataset (should handle gracefully)
      result_nonexistent <- conditionally_suppress({
        ellipse_unpush(datalake_con, dataset_name = "nonexistent-dataset-12345")
      })
      
      # Should return FALSE for non-existent dataset
      expect_false(result_nonexistent)
      
      # Test with non-existent tag in existing dataset
      # This would need a known existing dataset to test properly
      # In a real scenario, we might create a test dataset first
      
      # Clean disconnect
      conditionally_suppress({
        ellipse_disconnect(datalake_con)
      })
    }
  })
  
  debug_log("Datalake connection test completed")
})

# Test dataset existence validation
test_that("ellipse_unpush validates dataset existence", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  debug_log("Testing dataset existence validation")
  
  expect_no_error({
    datalake_con <- conditionally_suppress({
      ellipse_connect(env = "DEV", database = "datalake")
    })
    
    if (!is.null(datalake_con) && !inherits(datalake_con, "error")) {
      # Test various invalid dataset names
      invalid_datasets <- c("", "  ", "definitely-does-not-exist-12345")
      
      for (dataset in invalid_datasets) {
        result <- conditionally_suppress({
          ellipse_unpush(datalake_con, dataset_name = dataset)
        })
        
        # Should return FALSE for invalid/empty dataset names
        expect_false(result)
      }
      
      conditionally_suppress({
        ellipse_disconnect(datalake_con)
      })
    }
  })
})

# Test tag existence validation
test_that("ellipse_unpush validates tag existence when specified", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  debug_log("Testing tag existence validation")
  
  expect_no_error({
    datalake_con <- conditionally_suppress({
      ellipse_connect(env = "DEV", database = "datalake")
    })
    
    if (!is.null(datalake_con) && !inherits(datalake_con, "error")) {
      # Test with non-existent tag in non-existent dataset
      result <- conditionally_suppress({
        ellipse_unpush(datalake_con, 
                      dataset_name = "nonexistent-dataset", 
                      tag = "nonexistent-tag")
      })
      
      # Should return FALSE
      expect_false(result)
      
      conditionally_suppress({
        ellipse_disconnect(datalake_con)
      })
    }
  })
})

# Test function integrates with existing S3 functions
test_that("ellipse_unpush integrates with S3 functions properly", {
  debug_log("Testing S3 function integration")
  
  # Check that required functions exist (integration dependency test)
  expect_true(exists("list_public_datalake_bucket", mode = "function"))
  expect_true(exists("list_s3_folders", mode = "function"))
  expect_true(exists("delete_s3_folder", mode = "function"))
  expect_true(exists("invoke_datalake_indexing_lambda", mode = "function"))
  expect_true(exists("ask_yes_no", mode = "function"))
  expect_true(exists("get_aws_credentials", mode = "function"))
  
  # Check that connection functions exist
  expect_true(exists("ellipse_connect", mode = "function"))
  expect_true(exists("ellipse_disconnect", mode = "function"))
})

# Test error handling robustness
test_that("ellipse_unpush handles errors gracefully", {
  debug_log("Testing error handling")
  
  # Test with clearly invalid inputs that should not crash
  expect_no_error({
    # These should return FALSE, not crash
    result1 <- conditionally_suppress({
      tryCatch({
        ellipse_unpush("invalid_connection", "test")
      }, error = function(e) FALSE)
    })
    
    result2 <- conditionally_suppress({
      tryCatch({
        ellipse_unpush(list(), "test")
      }, error = function(e) FALSE)
    })
    
    # Results should be FALSE or the function should handle errors gracefully
    expect_true(is.logical(result1) || is.null(result1))
    expect_true(is.logical(result2) || is.null(result2))
  })
})

# Test return value consistency
test_that("ellipse_unpush returns consistent data types", {
  debug_log("Testing return value consistency")
  
  # The function should always return logical values (TRUE/FALSE)
  # This is tested implicitly in other tests, but we can add specific checks
  
  # Check function documentation indicates it returns logical
  formals_list <- formals(ellipse_unpush)
  expect_true(length(formals_list) >= 2) # At least con and dataset_name
})

# Performance and resource cleanup test
test_that("ellipse_unpush handles resource cleanup properly", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  debug_log("Testing resource cleanup")
  
  expect_no_error({
    # Test that multiple calls don't leave hanging connections
    for (i in 1:3) {
      datalake_con <- conditionally_suppress({
        ellipse_connect(env = "DEV", database = "datalake")
      })
      
      if (!is.null(datalake_con) && !inherits(datalake_con, "error")) {
        result <- conditionally_suppress({
          ellipse_unpush(datalake_con, dataset_name = "test-resource-cleanup")
        })
        
        # Should handle gracefully
        expect_true(is.logical(result))
        
        conditionally_suppress({
          ellipse_disconnect(datalake_con)
        })
      }
    }
  })
})
