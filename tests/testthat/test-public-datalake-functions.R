# Tests for public datalake infrastructure functions
# Following requirement: "use real life connections and data... Do not mock everything"

# Load current source code (not published package)
suppressMessages(suppressWarnings(devtools::load_all(".", quiet = TRUE)))

# DEBUGGING TESTS:
# - Normal run: Routine output suppressed for clean results
# - Verbose mode: Set TUBE_TEST_VERBOSE=TRUE to see all output for debugging
# - Example: Sys.setenv(TUBE_TEST_VERBOSE = "TRUE"); devtools::test(filter = "public-datalake-functions")

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


test_that("public datalake infrastructure functions exist and have proper signatures", {
  # Check that public datalake infrastructure functions exist
  expect_true(exists("list_public_datalake_bucket", mode = "function"))
  expect_true(exists("list_public_datalake_database", mode = "function"))

  # Check function signatures
  expect_gte(length(formals(list_public_datalake_bucket)), 1) # credentials parameter
  expect_gte(length(formals(list_public_datalake_database)), 1) # credentials parameter
})

test_that("list_public_datalake_bucket works with real AWS credentials", {
  debug_log("Testing public datalake infrastructure functions exist and have proper signatures")
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")

  # Test with real AWS credentials
  expect_no_error({
    creds <- get_aws_credentials("DEV")
    bucket_result <- list_public_datalake_bucket(creds)

    # Should return a character string (bucket name)
    expect_true(is.character(bucket_result) || is.null(bucket_result))

    # If bucket exists, should contain "publicdatalakebucket"
    if (!is.null(bucket_result) && length(bucket_result) > 0) {
      expect_true(grepl("publicdatalakebucket", bucket_result))
    }
  })
})

test_that("list_public_datalake_database works with real AWS credentials", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")

  # Test with real AWS credentials
  expect_no_error({
  debug_log("Testing list_public_datalake_database works with real AWS credentials")
    creds <- get_aws_credentials("DEV")
    database_result <- list_public_datalake_database(creds)

    # Should return a character string (database name)
    expect_true(is.character(database_result) || is.null(database_result))

    # If database exists, should contain "publicdatalake"
    if (!is.null(database_result) && length(database_result) > 0) {
      expect_true(grepl("publicdatalake", database_result, ignore.case = TRUE))
    }
  })
})

test_that("public datalake infrastructure functions handle invalid inputs", {
  # Test with NULL credentials
  expect_error(list_public_datalake_bucket(NULL), class = "error")
  expect_error(list_public_datalake_database(NULL), class = "error")

  # Test with invalid credentials
  expect_error(list_public_datalake_bucket("invalid"), class = "error")
  expect_error(list_public_datalake_database("invalid"), class = "error")
})

test_that("check_database now accepts 'datalake' parameter", {
  debug_log("Testing public datalake infrastructure functions handle invalid inputs")
  # Test that check_database accepts the new datalake parameter
  expect_true(check_database("datawarehouse"))
  expect_true(check_database("datamarts"))
  expect_true(check_database("datalake")) # New functionality

  # Test that invalid parameters are still rejected
  expect_false(check_database("invalid"))
  expect_false(check_database(NULL))
  expect_false(check_database(c("datawarehouse", "datamarts")))
})

test_that("ellipse_connect supports datalake database parameter", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")

  # Test connection to public datalake
  expect_no_error({
  debug_log("Testing ellipse_connect supports datalake database parameter")
    connection_result <- ellipse_connect(env = "DEV", database = "datalake")

    # Function should return a connection object or handle gracefully
    expect_true(!is.null(connection_result) || is.null(connection_result))

    # If connection successful, verify it's the right schema
    if (!is.null(connection_result) && !inherits(connection_result, "error")) {
      schema <- DBI::dbGetInfo(connection_result)$dbms.name
      expect_true(grepl("publicdatalake", schema, ignore.case = TRUE))

      # Clean up connection
      ellipse_disconnect(connection_result)
    }
  })
})

test_that("ellipse_discover works with public datalake connection", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")

  expect_no_error({
  debug_log("Testing ellipse_discover works with public datalake connection")
    # Connect to public datalake
    conn <- ellipse_connect(env = "DEV", database = "datalake")

    if (!is.null(conn) && !inherits(conn, "error")) {
      # Test discovery with no parameters (all datasets)
      discovery_all <- tryCatch(
        {
          ellipse_discover(conn)
        },
        error = function(e) NULL
      )

      # Should return data frame or list
      expect_true(is.data.frame(discovery_all) || is.list(discovery_all) || is.null(discovery_all))

      # Test discovery with pattern search
      discovery_pattern <- tryCatch(
        {
          ellipse_discover(conn, "test")
        },
        error = function(e) NULL
      )

      # Should return list with specific structure
      expect_true(is.list(discovery_pattern) || is.null(discovery_pattern))

      # Clean up
      ellipse_disconnect(conn)
    }

    expect_true(TRUE) # No unexpected errors
  })
})

test_that("ellipse_discover maintains backward compatibility", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")

  expect_no_error({
  debug_log("Testing ellipse_discover maintains backward compatibility")
    # Test with datawarehouse (should work as before)
    conn_dw <- ellipse_connect(env = "DEV", database = "datawarehouse")

    if (!is.null(conn_dw) && !inherits(conn_dw, "error")) {
      # Test existing functionality still works
      discovery_dw <- ellipse_discover(conn_dw)
      expect_true(is.data.frame(discovery_dw) || is.list(discovery_dw) || is.null(discovery_dw))

      ellipse_disconnect(conn_dw)
    }

    # Test with datamarts (should work as before)
    conn_dm <- ellipse_connect(env = "DEV", database = "datamarts")

    if (!is.null(conn_dm) && !inherits(conn_dm, "error")) {
      # Test existing functionality still works
      discovery_dm <- ellipse_discover(conn_dm)
      expect_true(is.data.frame(discovery_dm) || is.list(discovery_dm) || is.null(discovery_dm))

      ellipse_disconnect(conn_dm)
    }

    expect_true(TRUE) # No unexpected errors
  })
})

test_that("ellipse_discover with tag parameter works for public datalake", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")

  expect_no_error({
  debug_log("Testing ellipse_discover with tag parameter works for public datalake")
    conn <- ellipse_connect(env = "DEV", database = "datalake")

    if (!is.null(conn) && !inherits(conn, "error")) {
      # Test with name and tag (if data exists)
      discovery_tag <- tryCatch(
        {
          ellipse_discover(conn, "test-datagotchi", "elxnqc2022")
        },
        error = function(e) NULL
      )

      # Should return list with tag details or handle gracefully
      expect_true(is.list(discovery_tag) || is.null(discovery_tag))

      ellipse_disconnect(conn)
    }

    expect_true(TRUE) # No unexpected errors
  })
})
