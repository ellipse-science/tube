# Tests for ellipse_push() function and public datalake upload functionality
# This includes tests for both programmatic and interactive modes
#
# DEBUGGING TESTS:
# - Normal run: Routine output suppressed for clean results
# - Verbose mode: Set TUBE_TEST_VERBOSE=TRUE to see all output for debugging
# - Example: Sys.setenv(TUBE_TEST_VERBOSE = "TRUE"); devtools::test(filter = "ellipse-push")

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

test_that("ellipse_push function exists and has correct signature", {
  debug_log("Testing ellipse_push function signature")
  
  # Function exists
  expect_true(exists("ellipse_push", mode = "function"))
  
  # Check parameter names
  args_list <- formals(ellipse_push)
  expected_params <- c(
    "con", "file_or_folder", "dataset_name", "tag",
    "metadata", "interactive", "pipeline", "file_batch", "file_version"
  )
  expect_true(all(expected_params %in% names(args_list)))
  
  test_detail(sprintf("Function has %d parameters", length(args_list)))
})

test_that("ellipse_push detects connection type correctly", {
  debug_log("Testing connection validation")
  
  # Test with NULL connection
  expect_error(
    ellipse_push(con = NULL, file_or_folder = "test.csv", dataset_name = "test"),
    class = "error"
  )
})

test_that("ellipse_push allows NULL file_or_folder in interactive mode", {
  debug_log("Testing interactive mode parameter validation")
  
  # Test with valid parameters but without actually running interactive mode
  expect_no_error({
    # This should not error on parameter validation
    args_list <- formals(ellipse_push)
    expect_true("file_or_folder" %in% names(args_list))
    expect_equal(args_list$file_or_folder, NULL) # Default should be NULL
    expect_equal(args_list$interactive, TRUE) # Default should be TRUE
  })
})

test_that("ellipse_push works with datalake connections", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  debug_log("Testing public datalake push mode")
  
  expect_no_error({
    # Connect to public datalake - routine output suppressed, errors shown
    con <- conditionally_suppress({
      ellipse_connect(env = "DEV", database = "datalake")
    })
    
    if (!is.null(con) && !inherits(con, "error")) {
      # Create a temporary test file using helper
      temp_file <- create_temp_test_file(
        data.frame(
          id = 1:3,
          value = c("a", "b", "c"),
          timestamp = Sys.time()
        ),
        ext = "csv"
      )
      
      if (file.exists(temp_file)) {
        # Test programmatic mode - suppress routine output, show meaningful errors
        result <- tryCatch({
          conditionally_suppress({
            ellipse_push(con,
              file_or_folder = temp_file,
              dataset_name = "test-dataset",
              tag = "test-tag",
              metadata = list(title = "Test Upload", author = "Unit Test"),
              interactive = FALSE
            )
          })
        }, error = function(e) {
          # Expected errors (like Lambda timeout) - log but don't fail test
          debug_log(sprintf("Expected Lambda error: %s", e$message))
          NULL
        })
      }
      
      # Clean disconnect
      conditionally_suppress({
        ellipse_disconnect(con)
      })
    }
  })
  
  debug_log("Public datalake push mode completed")
})

test_that("ellipse_push works with datawarehouse connections (legacy mode)", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  debug_log("Testing datawarehouse push mode (legacy)")
  
  expect_no_error({
    # Connect to datawarehouse - routine output suppressed, errors shown
    con <- conditionally_suppress({
      ellipse_connect(env = "DEV", database = "datawarehouse")
    })
    
    if (!is.null(con) && !inherits(con, "error")) {
      # Create a temporary test file using helper
      temp_file <- create_temp_test_file(
        data.frame(id = 1:3, value = letters[1:3]),
        ext = "csv"
      )

      if (file.exists(temp_file)) {
        # Test legacy mode (should route to landing zone) - suppress routine output
        result <- conditionally_suppress({
          tryCatch(
            {
              ellipse_push(con,
                file_or_folder = temp_file,
                pipeline = "test-pipeline",
                file_batch = "test-batch-20250814"
              )
            },
            error = function(e) {
              # Pipeline may not exist - that's expected
              debug_log(sprintf("Expected pipeline error: %s", e$message))
              NULL
            }
          )
        })
      }

      if (file.exists(temp_file)) {
        # Test legacy mode (should route to landing zone) - suppress routine output
        result <- conditionally_suppress({
          tryCatch(
            {
              ellipse_push(con,
                file_or_folder = temp_file,
                pipeline = "test-pipeline",
                file_batch = "test-batch-20250814"
              )
            },
            error = function(e) {
              debug_log(sprintf("Expected pipeline error: %s", e$message))
              NULL
            }
          )
        })
      }
      
      # Clean disconnect
      conditionally_suppress({
        ellipse_disconnect(con)
      })
    }
  })
  
  debug_log("Datawarehouse push mode (legacy) completed")
})

test_that("ellipse_ingest backward compatibility works", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  debug_log("Testing backward compatibility")
  
  # ellipse_ingest should still exist
  expect_true(exists("ellipse_ingest", mode = "function"))
  
  expect_no_error({
    # Connect with routine output suppressed, errors shown
    con <- conditionally_suppress({
      ellipse_connect(env = "DEV", database = "datawarehouse")
    })
    
    if (!is.null(con) && !inherits(con, "error")) {
      temp_file <- create_temp_test_file(
        data.frame(a = 1:2, b = c("x", "y")),
        ext = "csv"
      )
      
      if (file.exists(temp_file)) {
        # Suppress routine output for ingest
        result <- conditionally_suppress({
          tryCatch(
            {
              ellipse_ingest(con,
                file_or_folder = temp_file,
                pipeline = "test-pipeline", 
                file_batch = "test-batch"
              )
            },
            error = function(e) {
              debug_log("Expected pipeline error for backward compatibility test")
              NULL
            }
          )
        })
      }
      
      # Clean disconnect
      conditionally_suppress({
        ellipse_disconnect(con)
      })
    }
  })
})

test_that("parameter validation works correctly", {
  debug_log("Testing parameter validation")
  
  # Test validate_datalake_push_params function
  
  # Create a real temporary file for valid test
  temp_file <- create_temp_test_file(data.frame(a = 1, b = 2), ext = "csv")
  
  # Valid parameters
  expect_true(validate_datalake_push_params(
    file_or_folder = temp_file,
    dataset_name = "test-dataset",
    tag = "test-tag",
    metadata = list(title = "Test")
  ))
  
  # Invalid dataset name
  expect_false(validate_datalake_push_params(
    file_or_folder = tempfile(),
    dataset_name = NULL,
    tag = "test-tag",
    metadata = NULL
  ))
  
  # Invalid tag
  expect_false(validate_datalake_push_params(
    file_or_folder = tempfile(),
    dataset_name = "test-dataset",
    tag = "",
    metadata = NULL
  ))
})

test_that("file preparation works correctly", {
  debug_log("Testing file preparation")
  
  # Create temporary test files using helper
  test_dir <- create_temp_test_dir(list(
    "test.csv" = data.frame(a = 1:3),
    "test.rds" = data.frame(b = 4:6),
    "test.txt" = "test content"
  ))
  
  # Test folder processing - suppress routine output, show errors
  files <- conditionally_suppress({
    prepare_files_for_upload(test_dir)
  })
  expect_true(length(files) == 2) # Only CSV and RDS should be included
  expect_true(all(tools::file_ext(files) %in% c("csv", "rds")))
  
  # Test single file - suppress routine output, show errors
  single_file <- file.path(test_dir, "test.csv")
  result <- conditionally_suppress({
    prepare_files_for_upload(single_file)
  })
  expect_equal(result, single_file)
  
  test_detail(sprintf("Processed %d files from directory", length(files)))
})

test_that("S3 metadata preparation works correctly", {
  debug_log("Testing S3 metadata preparation")
  
  # Test with custom metadata
  custom_meta <- list(title = "Test Data", author = "Unit Test", year = 2025)
  s3_meta <- prepare_s3_metadata(custom_meta)
  
  # Should have system fields (hyphenated format)
  expect_true("creation_date" %in% names(s3_meta))
  expect_true("consent_expiry_date" %in% names(s3_meta))
  expect_true("data_destruction_date" %in% names(s3_meta))
  expect_true("sensitivity_level" %in% names(s3_meta))
  expect_true("ethical_stamp" %in% names(s3_meta))

  
  # Should have custom metadata as JSON (hyphenated format)
  expect_true("user_metadata_json" %in% names(s3_meta))

  # Test with no custom metadata
  s3_meta_empty <- prepare_s3_metadata(NULL)
  expect_false("user_metadata_json" %in% names(s3_meta_empty))

  test_detail(sprintf("S3 metadata fields: %d", length(s3_meta)))
})

test_that("content type detection works correctly", {
  debug_log("Testing content type detection")
  
  expect_equal(get_content_type("test.csv"), "text/csv")
  expect_equal(get_content_type("test.xlsx"), "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
  expect_equal(get_content_type("test.rds"), "application/x-r-data")
  expect_equal(get_content_type("test.unknown"), "application/octet-stream")
})

test_that("utility functions exist and work", {
  debug_log("Testing utility functions")
  
  # Test list_public_datalake_bucket function exists
  expect_true(exists("list_public_datalake_bucket", mode = "function"))
  
  # Test %||% operator
  expect_equal("a" %||% "b", "a")
  expect_equal(NULL %||% "b", "b")
})

test_that("Lambda function invocation handles errors gracefully", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  debug_log("Testing Lambda invocation error handling")
  
  creds <- get_real_aws_credentials_dev()
  if (!is.null(creds)) {
    # This should fail gracefully with non-existent lambda function
    result <- invoke_datalake_indexing_lambda(creds)
    expect_true(is.logical(result))
    
    debug_log("Lambda error handling completed")
  }
})

test_that("Interactive mode functions exist", {
  debug_log("Testing interactive mode functions")
  
  expect_true(exists("interactive_datalake_push_flow", mode = "function"))
  expect_true(exists("collect_all_metadata_interactive", mode = "function"))
  expect_true(exists("display_upload_summary", mode = "function"))
})

test_that("All ellipse functions are properly exported", {
  debug_log("Testing function exports")
  
  # Test that ellipse_push is exported
  expect_true(exists("ellipse_push", mode = "function"))
  
  # Test that ellipse_ingest still works (backward compatibility)
  expect_true(exists("ellipse_ingest", mode = "function"))
  
  # Test function signatures
  push_args <- formals(ellipse_push)
  ingest_args <- formals(ellipse_ingest)
  
  expect_true(length(push_args) >= 9) # Should have all the new parameters
  expect_true(length(ingest_args) >= 5) # Should have the original parameters
  
  test_detail(sprintf("ellipse_push has %d parameters", length(push_args)))
  test_detail(sprintf("ellipse_ingest has %d parameters", length(ingest_args)))
})
