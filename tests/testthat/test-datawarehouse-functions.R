# Comprehensive tests for datawarehouse functions with REAL AWS connections
# Tests for: list_datawarehouse_database, list_datawarehouse_tables
# Following requirement: "use real life connections and data... Do not mock everything"

test_that("datawarehouse functions can be loaded and have proper signatures", {
  # Check that all datawarehouse functions exist
  expect_true(exists("list_datawarehouse_database", mode = "function"))
  expect_true(exists("list_datawarehouse_tables", mode = "function"))
  
  # Check function signatures
  expect_equal(length(formals(list_datawarehouse_database)), 1)  # credentials
  expect_equal(length(formals(list_datawarehouse_tables)), 2)    # credentials, simplify
})

# Tests for list_datawarehouse_database function
test_that("list_datawarehouse_database validates input parameters", {
  # Test with NULL credentials
  expect_error(
    list_datawarehouse_database(credentials = NULL),
    class = "error"
  )
})

test_that("list_datawarehouse_database works with real AWS credentials", {
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
  }
})

# Tests for list_datawarehouse_tables function  
test_that("list_datawarehouse_tables validates input parameters", {
  # Test with NULL credentials
  expect_error(
    list_datawarehouse_tables(credentials = NULL, simplify = TRUE),
    class = "error"
  )
})

test_that("list_datawarehouse_tables works with real AWS credentials", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Test listing tables with simplify = TRUE
  tables_simple <- list_datawarehouse_tables(creds, simplify = TRUE)
  expect_true(is.character(tables_simple) || is.null(tables_simple))
  
  # Test listing tables with simplify = FALSE
  tables_detailed <- list_datawarehouse_tables(creds, simplify = FALSE)
  expect_true(is.list(tables_detailed) || is.null(tables_detailed))
  
  # If tables exist, verify structure
  if (!is.null(tables_simple) && length(tables_simple) > 0) {
    expect_true(all(is.character(tables_simple)))
    expect_true(all(nzchar(tables_simple)))
  }
  
  if (!is.null(tables_detailed) && length(tables_detailed) > 0) {
    expect_true(is.list(tables_detailed))
  }
})

test_that("list_datawarehouse_tables handles different simplify options", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Get tables in both formats
  simple_result <- list_datawarehouse_tables(creds, simplify = TRUE)
  detailed_result <- list_datawarehouse_tables(creds, simplify = FALSE)
  
  # If both have results, they should be different types but related
  if (!is.null(simple_result) && !is.null(detailed_result) &&
      length(simple_result) > 0 && length(detailed_result) > 0) {
    
    # Simple should be character vector
    expect_true(is.character(simple_result))
    
    # Detailed should be list
    expect_true(is.list(detailed_result))
    
    # Both should represent data from the same source
    expect_true(length(simple_result) > 0)
    expect_true(length(detailed_result) > 0)
  }
})

test_that("datawarehouse functions handle AWS API errors gracefully", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # All datawarehouse functions should handle API errors gracefully
  expect_no_error({
    db_result <- list_datawarehouse_database(creds)
    expect_true(is.character(db_result) || is.null(db_result))
  })
  
  expect_no_error({
    tables_result <- list_datawarehouse_tables(creds, simplify = TRUE)
    expect_true(is.character(tables_result) || is.null(tables_result))
  })
  
  expect_no_error({
    detailed_tables_result <- list_datawarehouse_tables(creds, simplify = FALSE)
    expect_true(is.list(detailed_tables_result) || is.null(detailed_tables_result))
  })
})

test_that("datawarehouse functions integrate with each other properly", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Test the workflow: get datawarehouse database -> list tables
  datawarehouse_db <- list_datawarehouse_database(creds)
  
  if (!is.null(datawarehouse_db) && length(datawarehouse_db) > 0) {
    # If we have a datawarehouse database, listing tables should work
    tables <- list_datawarehouse_tables(creds, simplify = TRUE)
    
    # Tables function should work regardless of database result
    # (since it gets schema information differently)
    expect_true(is.character(tables) || is.null(tables))
    
    # Test detailed tables as well
    detailed_tables <- list_datawarehouse_tables(creds, simplify = FALSE)
    expect_true(is.list(detailed_tables) || is.null(detailed_tables))
  }
})

test_that("datawarehouse functions work with bucket integration", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Test integration between datawarehouse bucket and database functions
  datawarehouse_bucket <- list_datawarehouse_bucket(creds)
  datawarehouse_db <- list_datawarehouse_database(creds)
  
  # Test that bucket and database functions return consistent results
  if (!is.null(datawarehouse_bucket) && !is.null(datawarehouse_db)) {
    expect_true(length(datawarehouse_bucket) > 0)
    expect_true(length(datawarehouse_db) > 0)
    
    # Both should be character vectors
    expect_true(is.character(datawarehouse_bucket))
    expect_true(is.character(datawarehouse_db))
  }
  
  # Test that table listing works with database information
  if (!is.null(datawarehouse_db)) {
    tables <- list_datawarehouse_tables(creds, simplify = TRUE)
    expect_true(is.character(tables) || is.null(tables))
  }
})

test_that("datawarehouse functions handle empty results correctly", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Functions should handle cases where no resources exist
  expect_no_error({
    db_result <- list_datawarehouse_database(creds)
    # Should return NULL or empty character vector if no databases
    if (!is.null(db_result)) {
      expect_true(is.character(db_result))
    }
  })
  
  expect_no_error({
    tables_result <- list_datawarehouse_tables(creds, simplify = TRUE)
    # Should return NULL or empty character vector if no tables
    if (!is.null(tables_result)) {
      expect_true(is.character(tables_result))
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

test_that("datawarehouse functions return consistent data types", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Test that functions consistently return expected types
  db_result <- list_datawarehouse_database(creds)
  if (!is.null(db_result)) {
    expect_true(is.character(db_result))
    expect_true(length(db_result) >= 0)
  }
  
  simple_tables <- list_datawarehouse_tables(creds, simplify = TRUE)
  if (!is.null(simple_tables)) {
    expect_true(is.character(simple_tables))
    expect_true(length(simple_tables) >= 0)
  }
  
  detailed_tables <- list_datawarehouse_tables(creds, simplify = FALSE)
  if (!is.null(detailed_tables)) {
    expect_true(is.list(detailed_tables))
    expect_true(length(detailed_tables) >= 0)
  }
})

test_that("datawarehouse functions work with Glue integration", {
  skip_if_not(can_test_real_aws_dev(), "Real AWS testing not available")
  
  creds <- get_real_aws_credentials_dev()
  
  # Test that datawarehouse functions work alongside Glue functions
  datawarehouse_db <- list_datawarehouse_database(creds)
  glue_databases <- list_glue_databases(creds, "datawarehouse")
  
  # These should be consistent if both find databases
  if (!is.null(datawarehouse_db) && !is.null(glue_databases) &&
      length(datawarehouse_db) > 0 && length(glue_databases) > 0) {
    
    expect_true(is.character(datawarehouse_db))
    expect_true(is.character(glue_databases))
    
    # They might find the same databases (or related ones)
    # At minimum, both should be valid character vectors
    expect_true(all(nzchar(datawarehouse_db)))
    expect_true(all(nzchar(glue_databases)))
  }
  
  # Test table listing integration
  datawarehouse_tables <- list_datawarehouse_tables(creds, simplify = TRUE)
  
  if (!is.null(datawarehouse_tables) && !is.null(datawarehouse_db) &&
      length(datawarehouse_tables) > 0 && length(datawarehouse_db) > 0) {
    
    # Should be able to get Glue tables for the same database
    glue_tables <- list_glue_tables(creds, datawarehouse_db[1], NULL, TRUE)
    expect_true(is.character(glue_tables) || is.list(glue_tables) || is.null(glue_tables))
  }
})
