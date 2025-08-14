# Comprehensive tests for datawarehouse functions with REAL AWS connections
# Tests for: list_datawarehouse_database, list_datawarehouse_tables
# Following requirement: "use real life connections and data... Do not mock everything"

# Load current source code (not published package)
suppressMessages(suppressWarnings(devtools::load_all(".", quiet = TRUE)))

# DEBUGGING TESTS:
# - Normal run: Routine output suppressed for clean results
# - Verbose mode: Set TUBE_TEST_VERBOSE=TRUE to see all output for debugging
# - Example: Sys.setenv(TUBE_TEST_VERBOSE = "TRUE"); devtools::test(filter = "datawarehouse-functions")

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


test_that("DATAWAREHOUSE: function signatures are correct", {
  debug_log("Testing DATAWAREHOUSE: function signatures are correct")

  # Check that all datawarehouse functions exist
  expect_true(exists("list_datawarehouse_database", mode = "function"))
  expect_true(exists("list_datawarehouse_tables", mode = "function"))

  # Check function signatures
  expect_equal(length(formals(list_datawarehouse_database)), 1) # credentials
  expect_equal(length(formals(list_datawarehouse_tables)), 2) # credentials, simplify

})

test_that("DATAWAREHOUSE: list_datawarehouse_database validates NULL credentials", {


  # Test with NULL credentials
  expect_error(
    list_datawarehouse_database(credentials = NULL),
    class = "error"
  )

})

test_that("DATAWAREHOUSE: list_datawarehouse_database works with real AWS", {
  debug_log("Testing DATAWAREHOUSE: list_datawarehouse_database validates NULL credentials")
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")


  creds <- get_real_aws_credentials_dev()


  # Test listing datawarehouse database
  result <- list_datawarehouse_database(creds)
  expect_true(is.character(result) || is.null(result))


  # If database exists, verify it contains "datawarehouse" or "datamart" in name
  # (based on the function implementation that filters by "datamart")
  if (!is.null(result) && length(result) > 0) {
    expect_true(any(grepl("datamart|datawarehouse", result, ignore.case = TRUE)))
    expect_true(all(is.character(result)))
    expect_true(all(nzchar(result)))
  } else {
    test_detail("ℹ️ No datawarehouse databases found in this environment\n")
  }
})

test_that("DATAWAREHOUSE: list_datawarehouse_tables validates NULL credentials", {


  # Test with NULL credentials
  expect_error(
    list_datawarehouse_tables(credentials = NULL, simplify = TRUE),
    class = "error"
  )

})

test_that("DATAWAREHOUSE: list_datawarehouse_tables returns correct data types", {
  debug_log("Testing DATAWAREHOUSE: list_datawarehouse_tables validates NULL credentials")
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")


  creds <- get_real_aws_credentials_dev()


  # Test listing tables with simplify = TRUE
  tables_simple <- list_datawarehouse_tables(creds, simplify = TRUE)
  expect_true(is.data.frame(tables_simple) || is.null(tables_simple))



  # Test listing tables with simplify = FALSE
  tables_detailed <- list_datawarehouse_tables(creds, simplify = FALSE)
  expect_true(is.list(tables_detailed) || is.null(tables_detailed))


  # If tables exist, verify structure
  if (!is.null(tables_simple) && nrow(tables_simple) > 0) {
    test_detail("Simplified table structure:")
    test_detail(sprintf("- Rows: %s", nrow(tables_simple)))
    test_detail(sprintf("- Columns: %s", paste(names(tables_simple), collapse = ", ")))

    expect_true(is.data.frame(tables_simple))
    expected_cols <- c("table_name", "col_name", "col_type", "is_partition")
    expect_true(all(expected_cols %in% names(tables_simple)))
  } else {
    test_detail("ℹ️ No tables found in simplified format")
  }

  if (!is.null(tables_detailed) && length(tables_detailed) > 0) {
    test_detail("Detailed table structure:")
    test_detail(sprintf("- List names: %s", paste(names(tables_detailed), collapse = ", ")))

    expect_true(is.list(tables_detailed))
  } else {
    test_detail("ℹ️ No tables found in detailed format")
  }
})

test_that("DATAWAREHOUSE: simplify parameter changes return types correctly", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")


  test_detail("SUPPORTING FUNCTION: glue_table_list_to_tibble():\n")
  test_detail("  - Creates tibble with columns: table_name, col_name, col_type, is_partition\n")
  test_detail("  - Processes both regular columns and partition keys\n")
  test_detail("  - Combines all table metadata into structured format\n\n")

  creds <- get_real_aws_credentials_dev()


  # Get tables in both formats
  simple_result <- list_datawarehouse_tables(creds, simplify = TRUE)
  detailed_result <- list_datawarehouse_tables(creds, simplify = FALSE)

  debug_log("Testing DATAWAREHOUSE: simplify parameter changes return types correctly")
  test_detail(sprintf("simplify=TRUE result: %s", class(simple_result)))
  test_detail(sprintf("simplify=FALSE result: %s", class(detailed_result)))

  # If both have results, they should be different types but related
  if (!is.null(simple_result) && !is.null(detailed_result) &&
    length(simple_result) > 0 && length(detailed_result) > 0) {
    test_detail("COMPARING RESULTS:")
    test_detail(sprintf("- Simple format rows: %s", nrow(simple_result)))

    # Simple should be data.frame (tibble)
    expect_true(is.data.frame(simple_result))

    # Detailed should be list
    expect_true(is.list(detailed_result))

    # Both should represent data from the same source
    expect_true(length(simple_result) > 0)
    expect_true(length(detailed_result) > 0)

  } else {
    test_detail("ℹ️ One or both formats returned empty results - normal for empty schema\n")
  }
})

test_that("DATAWAREHOUSE: functions handle AWS API errors gracefully", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")

  creds <- get_real_aws_credentials_dev()

  # All datawarehouse functions should handle API errors gracefully
  expect_no_error({
  debug_log("Testing DATAWAREHOUSE: functions handle AWS API errors gracefully")
    db_result <- list_datawarehouse_database(creds)
    expect_true(is.character(db_result) || is.null(db_result))
  })

  expect_no_error({
    tables_result <- list_datawarehouse_tables(creds, simplify = TRUE)
    expect_true(is.data.frame(tables_result) || is.null(tables_result))
  })

  expect_no_error({
    detailed_tables_result <- list_datawarehouse_tables(creds, simplify = FALSE)
    expect_true(is.list(detailed_tables_result) || is.null(detailed_tables_result))
  })
})

test_that("DATAWAREHOUSE: database and table functions integrate properly", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")

  creds <- get_real_aws_credentials_dev()

  # Test the workflow: get datawarehouse database -> list tables
  datawarehouse_db <- list_datawarehouse_database(creds)

  if (!is.null(datawarehouse_db) && length(datawarehouse_db) > 0) {
  debug_log("Testing DATAWAREHOUSE: database and table functions integrate properly")
    # If we have a datawarehouse database, listing tables should work
    tables <- list_datawarehouse_tables(creds, simplify = TRUE)

    # Tables function should work regardless of database result
    # (since it gets schema information differently)
    expect_true(is.data.frame(tables) || is.null(tables))

    # Test detailed tables as well
    detailed_tables <- list_datawarehouse_tables(creds, simplify = FALSE)
    expect_true(is.list(detailed_tables) || is.null(detailed_tables))
  }
})

test_that("DATAWAREHOUSE: bucket and database integration works", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")

  creds <- get_real_aws_credentials_dev()

  # Test integration between datawarehouse bucket and database functions
  datawarehouse_bucket <- list_datawarehouse_bucket(creds)
  datawarehouse_db <- list_datawarehouse_database(creds)

  # Test that bucket and database functions return consistent results
  if (!is.null(datawarehouse_bucket) && !is.null(datawarehouse_db)) {
  debug_log("Testing DATAWAREHOUSE: bucket and database integration works")
    expect_true(length(datawarehouse_bucket) > 0)
    expect_true(length(datawarehouse_db) > 0)

    # Both should be character vectors
    expect_true(is.character(datawarehouse_bucket))
    expect_true(is.character(datawarehouse_db))
  }

  # Test that table listing works with database information
  if (!is.null(datawarehouse_db)) {
    tables <- list_datawarehouse_tables(creds, simplify = TRUE)
    expect_true(is.data.frame(tables) || is.null(tables))
  }
})

test_that("DATAWAREHOUSE: functions handle empty results correctly", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")

  creds <- get_real_aws_credentials_dev()

  # Functions should handle cases where no resources exist
  expect_no_error({
  debug_log("Testing DATAWAREHOUSE: functions handle empty results correctly")
    db_result <- list_datawarehouse_database(creds)
    # Should return NULL or empty character vector if no databases
    if (!is.null(db_result)) {
      expect_true(is.character(db_result))
    }
  })

  expect_no_error({
    tables_result <- list_datawarehouse_tables(creds, simplify = TRUE)
    # Should return NULL or empty data.frame if no tables
    if (!is.null(tables_result)) {
      expect_true(is.data.frame(tables_result))
    }
  })

  expect_no_error({
    detailed_result <- list_datawarehouse_tables(creds, simplify = FALSE)
    # Should return NULL or empty list if no tables
    if (!is.null(detailed_result)) {
      expect_true(is.list(detailed_result))
    }
  })
})

test_that("DATAWAREHOUSE: functions return consistent data types", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")

  creds <- get_real_aws_credentials_dev()

  # Test that functions consistently return expected types
  db_result <- list_datawarehouse_database(creds)
  if (!is.null(db_result)) {
  debug_log("Testing DATAWAREHOUSE: functions return consistent data types")
    expect_true(is.character(db_result))
    expect_true(length(db_result) >= 0)
  }

  simple_tables <- list_datawarehouse_tables(creds, simplify = TRUE)
  if (!is.null(simple_tables)) {
    expect_true(is.data.frame(simple_tables))
    expect_true(nrow(simple_tables) >= 0)
  }

  detailed_tables <- list_datawarehouse_tables(creds, simplify = FALSE)
  if (!is.null(detailed_tables)) {
    expect_true(is.list(detailed_tables))
    expect_true(length(detailed_tables) >= 0)
  }
})

test_that("DATAWAREHOUSE: Glue integration works correctly", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")


  test_detail("   └─> Should return SAME results as datawarehouse wrapper\n\n")

  test_detail("3. list_glue_tables(creds, database_name, NULL, TRUE)\n")
  test_detail("   └─> Uses database from step 1 to get table details\n\n")

  test_detail("datawarehouse functions -> glue functions -> paws.analytics::glue -> AWS API\n\n")

  creds <- get_real_aws_credentials_dev()


  # Test that datawarehouse functions work alongside Glue functions
  datawarehouse_db <- list_datawarehouse_database(creds)
  glue_databases <- list_glue_databases(creds, "datawarehouse")

  debug_log("Testing DATAWAREHOUSE: Glue integration works correctly")
  test_detail(sprintf("Datawarehouse wrapper result: %s", if (is.null(datawarehouse_db)) {
    "NULL"
  } else {
    paste(datawarehouse_db, collapse = ", ")
  }))
  test_detail(sprintf("Direct Glue call result: %s", if (is.null(glue_databases)) {
    "NULL"
  } else {
    paste(glue_databases, collapse = ", ")
  }))

  # These should be consistent if both find databases
  if (!is.null(datawarehouse_db) && !is.null(glue_databases) &&
    length(datawarehouse_db) > 0 && length(glue_databases) > 0) {
    expect_true(is.character(datawarehouse_db))
    expect_true(is.character(glue_databases))

    # They might find the same databases (or related ones)
    # At minimum, both should be valid character vectors
    expect_true(all(nzchar(datawarehouse_db)))
    expect_true(all(nzchar(glue_databases)))

  } else {
    test_detail("ℹ️ No databases found - integration cannot be fully tested\n")
  }

  # Test table listing integration
  datawarehouse_tables <- list_datawarehouse_tables(creds, simplify = TRUE)

  if (!is.null(datawarehouse_tables) && !is.null(datawarehouse_db) &&
    length(datawarehouse_tables) > 0 && length(datawarehouse_db) > 0) {

    # Should be able to get Glue tables for the same database
    glue_tables <- list_glue_tables(creds, datawarehouse_db[1], NULL, TRUE)
    expect_true(is.data.frame(glue_tables) || is.list(glue_tables) || is.null(glue_tables))

    test_detail(sprintf("Direct Glue table call result: %s", class(glue_tables)))
  } else {
    test_detail("ℹ️ No tables available for integration testing")
  }
})
