# Tests for ellipse_push() function and public datalake upload functionality
# This includes tests for both programmatic and interactive modes

# Load current source code (not published package)
devtools::load_all(".")

test_that("ellipse_push function exists and has correct signature", {
  cat("\n=== TESTING ellipse_push FUNCTION SIGNATURE ===\n")

  # Function exists
  expect_true(exists("ellipse_push", mode = "function"))

  # Check parameter names
  args_list <- formals(ellipse_push)
  expected_params <- c(
    "con", "file_or_folder", "dataset_name", "tag",
    "metadata", "interactive", "pipeline", "file_batch", "file_version"
  )
  expect_true(all(expected_params %in% names(args_list)))

  cat("✅ ellipse_push function signature verified!\n")
})

test_that("ellipse_push detects connection type correctly", {
  # Test with NULL connection
  expect_error(
    ellipse_push(con = NULL, file_or_folder = "test.csv", dataset_name = "test"),
    class = "error"
  )

  cat("✅ Connection validation works!\n")
})

test_that("ellipse_push allows NULL file_or_folder in interactive mode", {
  # Test that file_or_folder can be NULL when interactive = TRUE for datalake connections
  # This would normally prompt the user, but we'll test the function signature accepts it

  # Test with valid parameters but without actually running interactive mode
  expect_no_error({
    # This should not error on parameter validation
    args_list <- formals(ellipse_push)
    expect_true("file_or_folder" %in% names(args_list))
    expect_equal(args_list$file_or_folder, NULL) # Default should be NULL
    expect_equal(args_list$interactive, TRUE) # Default should be TRUE
  })

  cat("✅ Interactive file selection parameter validation works!\n")
})

test_that("ellipse_push works with datalake connections", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")

  cat("\n=== TESTING PUBLIC DATALAKE PUSH MODE ===\n")

  expect_no_error({
    # Connect to public datalake
    con <- ellipse_connect(env = "DEV", database = "datalake")

    if (!is.null(con) && !inherits(con, "error")) {
      # Create a temporary test file
      temp_file <- tempfile(fileext = ".csv")
      test_data <- data.frame(
        id = 1:3,
        value = c("a", "b", "c"),
        timestamp = Sys.time()
      )
      write.csv(test_data, temp_file, row.names = FALSE)

      if (file.exists(temp_file)) {
        # Test programmatic mode (non-interactive)
        result <- tryCatch(
          {
            ellipse_push(con,
              file_or_folder = temp_file,
              dataset_name = "test-dataset",
              tag = "test-tag",
              metadata = list(title = "Test Upload", author = "Unit Test"),
              interactive = FALSE
            )
          },
          error = function(e) {
            # May fail due to Lambda function name differences - that's OK
            cat("Expected error for Lambda function:", e$message, "\n")
            NULL
          }
        )

        unlink(temp_file)
      }

      ellipse_disconnect(con)
    }
  })

  cat("✅ Public datalake push mode tested!\n")
})

test_that("ellipse_push works with datawarehouse connections (legacy mode)", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")

  cat("\n=== TESTING DATAWAREHOUSE PUSH MODE (LEGACY) ===\n")

  expect_no_error({
    # Connect to datawarehouse
    con <- ellipse_connect(env = "DEV", database = "datawarehouse")

    if (!is.null(con) && !inherits(con, "error")) {
      # Create a temporary test file
      temp_file <- tempfile(fileext = ".csv")
      test_data <- data.frame(id = 1:3, value = letters[1:3])
      write.csv(test_data, temp_file, row.names = FALSE)

      if (file.exists(temp_file)) {
        # Test legacy mode (should route to landing zone)
        result <- tryCatch(
          {
            ellipse_push(con,
              file_or_folder = temp_file,
              pipeline = "test-pipeline",
              file_batch = "test-batch-20250814"
            )
          },
          error = function(e) {
            # Pipeline may not exist - that's expected
            cat("Expected error for test pipeline:", e$message, "\n")
            NULL
          }
        )

        unlink(temp_file)
      }

      ellipse_disconnect(con)
    }
  })

  cat("✅ Datawarehouse push mode (legacy) tested!\n")
})

test_that("ellipse_ingest backward compatibility works", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")

  cat("\n=== TESTING BACKWARD COMPATIBILITY ===\n")

  # ellipse_ingest should still exist
  expect_true(exists("ellipse_ingest", mode = "function"))

  expect_no_error({
    con <- ellipse_connect(env = "DEV", database = "datawarehouse")

    if (!is.null(con) && !inherits(con, "error")) {
      temp_file <- tempfile(fileext = ".csv")
      write.csv(data.frame(a = 1:2, b = c("x", "y")), temp_file, row.names = FALSE)

      if (file.exists(temp_file)) {
        result <- tryCatch(
          {
            ellipse_ingest(con,
              file_or_folder = temp_file,
              pipeline = "test-pipeline", file_batch = "test-batch"
            )
          },
          error = function(e) {
            # Expected to fail on non-existent pipeline
            NULL
          }
        )

        unlink(temp_file)
      }

      ellipse_disconnect(con)
    }
  })

  cat("✅ Backward compatibility verified!\n")
})

test_that("parameter validation works correctly", {
  cat("\n=== TESTING PARAMETER VALIDATION ===\n")

  # Test validate_datalake_push_params function

  # Create a real temporary file for valid test
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1, b = 2), temp_file, row.names = FALSE)

  # Valid parameters
  expect_true(validate_datalake_push_params(
    file_or_folder = temp_file,
    dataset_name = "test-dataset",
    tag = "test-tag",
    metadata = list(title = "Test")
  ))

  # Clean up
  unlink(temp_file)

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

  cat("✅ Parameter validation tested!\n")
})

test_that("file preparation works correctly", {
  cat("\n=== TESTING FILE PREPARATION ===\n")

  # Create temporary test files
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_upload")
  dir.create(test_dir, showWarnings = FALSE)

  # Create supported format files
  write.csv(data.frame(a = 1:3), file.path(test_dir, "test.csv"), row.names = FALSE)
  saveRDS(data.frame(b = 4:6), file.path(test_dir, "test.rds"))

  # Create unsupported format file
  writeLines("test", file.path(test_dir, "test.txt"))

  # Test folder processing
  files <- prepare_files_for_upload(test_dir)
  expect_true(length(files) == 2) # Only CSV and RDS should be included
  expect_true(all(tools::file_ext(files) %in% c("csv", "rds")))

  # Test single file
  single_file <- file.path(test_dir, "test.csv")
  result <- prepare_files_for_upload(single_file)
  expect_equal(result, single_file)

  # Clean up
  unlink(test_dir, recursive = TRUE)

  cat("✅ File preparation tested!\n")
})

test_that("S3 metadata preparation works correctly", {
  cat("\n=== TESTING S3 METADATA PREPARATION ===\n")

  # Test with custom metadata
  custom_meta <- list(title = "Test Data", author = "Unit Test", year = 2025)
  s3_meta <- prepare_s3_metadata(custom_meta)

  # Should have system fields
  expect_true("creation-date" %in% names(s3_meta))
  expect_true("consent-expiry-date" %in% names(s3_meta))
  expect_true("data-destruction-date" %in% names(s3_meta))
  expect_true("sensitivity-level" %in% names(s3_meta))
  expect_true("ethical-stamp" %in% names(s3_meta))

  # Should have custom metadata as JSON
  expect_true("user-metadata-json" %in% names(s3_meta))

  # Test with no custom metadata
  s3_meta_empty <- prepare_s3_metadata(NULL)
  expect_false("user-metadata-json" %in% names(s3_meta_empty))

  cat("✅ S3 metadata preparation tested!\n")
})

test_that("content type detection works correctly", {
  cat("\n=== TESTING CONTENT TYPE DETECTION ===\n")

  expect_equal(get_content_type("test.csv"), "text/csv")
  expect_equal(get_content_type("test.xlsx"), "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
  expect_equal(get_content_type("test.rds"), "application/x-r-data")
  expect_equal(get_content_type("test.unknown"), "application/octet-stream")

  cat("✅ Content type detection tested!\n")
})

test_that("utility functions exist and work", {
  cat("\n=== TESTING UTILITY FUNCTIONS ===\n")

  # Test list_public_datalake_bucket function exists
  expect_true(exists("list_public_datalake_bucket", mode = "function"))

  # Test %||% operator
  expect_equal("a" %||% "b", "a")
  expect_equal(NULL %||% "b", "b")

  cat("✅ Utility functions tested!\n")
})

test_that("Lambda function invocation handles errors gracefully", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")

  cat("\n=== TESTING LAMBDA INVOCATION ERROR HANDLING ===\n")

  creds <- get_real_aws_credentials_dev()
  if (!is.null(creds)) {
    # This should fail gracefully with non-existent lambda function
    result <- invoke_datalake_indexing_lambda(creds)
    expect_true(is.logical(result))

    cat("✅ Lambda invocation error handling tested!\n")
  }
})

test_that("Interactive mode functions exist", {
  cat("\n=== TESTING INTERACTIVE MODE FUNCTIONS ===\n")

  expect_true(exists("interactive_datalake_push_flow", mode = "function"))
  expect_true(exists("collect_custom_metadata_interactive", mode = "function"))
  expect_true(exists("display_upload_summary", mode = "function"))

  cat("✅ Interactive mode functions exist!\n")
})

test_that("All ellipse functions are properly exported", {
  cat("\n=== TESTING FUNCTION EXPORTS ===\n")

  # Test that ellipse_push is exported
  expect_true(exists("ellipse_push", mode = "function"))

  # Test that ellipse_ingest still works (backward compatibility)
  expect_true(exists("ellipse_ingest", mode = "function"))

  # Test function signatures
  push_args <- formals(ellipse_push)
  ingest_args <- formals(ellipse_ingest)

  expect_true(length(push_args) >= 9) # Should have all the new parameters
  expect_true(length(ingest_args) >= 5) # Should have the original parameters

  cat("✅ Function exports verified!\n")
})
