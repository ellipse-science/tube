# Comprehensive tests for parameter validation functions
# Tests for: check_env, check_database, check_pipeline_before_ingest,
#           check_file_versioning_before_ingest, check_params_before_publish,
#           check_params_before_unpublish, check_params_before_describe,
#           check_params_before_refresh
# Following requirement: "use real life connections and data... Do not mock everything"

# Load current source code (not published package)
devtools::load_all(".")

test_that("validation functions can be loaded and have proper signatures", {
  # Check that all validation functions exist
  expect_true(exists("check_env", mode = "function"))
  expect_true(exists("check_database", mode = "function"))
  expect_true(exists("check_pipeline_before_ingest", mode = "function"))
  expect_true(exists("check_file_versioning_before_ingest", mode = "function"))
  expect_true(exists("check_params_before_publish", mode = "function"))
  expect_true(exists("check_params_before_unpublish", mode = "function"))
  expect_true(exists("check_params_before_describe", mode = "function"))
  expect_true(exists("check_params_before_refresh", mode = "function"))
  
  # Check function signatures
  expect_equal(length(formals(check_env)), 1)  # env
  expect_equal(length(formals(check_database)), 1)  # database
  expect_equal(length(formals(check_pipeline_before_ingest)), 4)  # pipeline, landing_zone_partitions, file_batch, file_version
  expect_equal(length(formals(check_file_versioning_before_ingest)), 2)  # file_batch, file_version
})

# Tests for check_env function
test_that("check_env validates environment parameters correctly", {
  # Test valid environments
  expect_true(check_env("DEV"))
  expect_true(check_env("PROD"))
  
  # Test invalid environments
  expect_false(check_env("INVALID"))
  expect_false(check_env("dev"))  # case sensitive
  expect_false(check_env("prod")) # case sensitive
  expect_false(check_env(""))
  expect_false(check_env(NULL))
  expect_false(check_env(NA))
  expect_false(check_env(123))
  expect_false(check_env(c("DEV", "PROD")))  # vector not allowed
})

test_that("check_env handles edge cases", {
  # Test with different data types
  expect_false(check_env(list("DEV")))
  expect_false(check_env(factor("DEV")))
  expect_false(check_env(TRUE))
  expect_false(check_env(FALSE))
  
  # Test with whitespace
  expect_false(check_env(" DEV "))
  expect_false(check_env("DEV "))
  expect_false(check_env(" DEV"))
})

# Tests for check_database function
test_that("check_database validates database parameters correctly", {
  # Test valid databases
  expect_true(check_database("datawarehouse"))
  expect_true(check_database("datamarts"))
  
  # Test invalid databases
  expect_false(check_database("DATAWAREHOUSE"))  # case sensitive
  expect_false(check_database("DATAMARTS"))      # case sensitive
  expect_false(check_database("invalid"))
  expect_false(check_database(""))
  expect_false(check_database(NULL))
  expect_false(check_database(NA))
  expect_false(check_database(123))
  expect_false(check_database(c("datawarehouse", "datamarts")))  # vector not allowed
})

test_that("check_database handles edge cases", {
  # Test with different data types
  expect_false(check_database(list("datawarehouse")))
  expect_false(check_database(factor("datawarehouse")))
  expect_false(check_database(TRUE))
  expect_false(check_database(FALSE))
  
  # Test with whitespace
  expect_false(check_database(" datawarehouse "))
  expect_false(check_database("datawarehouse "))
  expect_false(check_database(" datawarehouse"))
})

# Tests for check_pipeline_before_ingest function
test_that("check_pipeline_before_ingest validates pipeline parameters", {
  # Create mock landing zone partitions for testing
  mock_partitions <- c("pipeline1/", "pipeline2/", "test-pipeline/")
  
  # Test valid pipeline that exists in partitions
  expect_true(check_pipeline_before_ingest("pipeline1", mock_partitions, NULL, NULL))
  expect_true(check_pipeline_before_ingest("pipeline2", mock_partitions, NULL, NULL))
  
  # Test invalid pipeline that doesn't exist in partitions
  expect_false(check_pipeline_before_ingest("nonexistent", mock_partitions, NULL, NULL))
  expect_false(check_pipeline_before_ingest("", mock_partitions, NULL, NULL))
  expect_false(check_pipeline_before_ingest(NULL, mock_partitions, NULL, NULL))
})

test_that("check_pipeline_before_ingest handles file versioning scenarios", {
  mock_partitions <- c("pipeline1/", "pipeline2/")
  
  # Test with file_batch and file_version combinations
  expect_no_error(check_pipeline_before_ingest("pipeline1", mock_partitions, "batch1", "v1.0"))
  expect_no_error(check_pipeline_before_ingest("pipeline1", mock_partitions, "batch1", NULL))
  expect_no_error(check_pipeline_before_ingest("pipeline1", mock_partitions, NULL, "v1.0"))
  expect_no_error(check_pipeline_before_ingest("pipeline1", mock_partitions, NULL, NULL))
})

# Tests for check_file_versioning_before_ingest function
test_that("check_file_versioning_before_ingest validates versioning logic", {
  # Test valid combinations
  expect_true(check_file_versioning_before_ingest("batch1", "v1.0"))  # both provided
  expect_true(check_file_versioning_before_ingest("batch1", NULL))    # only batch
  expect_true(check_file_versioning_before_ingest(NULL, NULL))        # neither provided
  
  # Test invalid combination - version without batch should be invalid
  expect_false(check_file_versioning_before_ingest(NULL, "v1.0"))
})

test_that("check_file_versioning_before_ingest handles edge cases", {
  # Test with empty strings
  expect_false(check_file_versioning_before_ingest("", "v1.0"))
  expect_false(check_file_versioning_before_ingest("batch1", ""))
  expect_false(check_file_versioning_before_ingest("", ""))
  
  # Test with different data types
  expect_false(check_file_versioning_before_ingest(123, "v1.0"))
  expect_false(check_file_versioning_before_ingest("batch1", 123))
})

# Tests for check_params_before_publish function
test_that("check_params_before_publish validates publish parameters", {
  # Create a simple test dataframe
  test_df <- data.frame(
    col1 = c(1, 2, 3),
    col2 = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  
  # Test with valid parameters
  expect_no_error({
    result <- check_params_before_publish(
      env = "DEV",
      dataframe = test_df,
      datamart = "test_datamart",
      table = "test_table",
      data_tag = "test_tag",
      table_tags = list(category = "test"),
      table_description = "Test table"
    )
  })
  
  # Test with invalid environment
  expect_no_error({
    result <- check_params_before_publish(
      env = "INVALID",
      dataframe = test_df,
      datamart = "test_datamart",
      table = "test_table",
      data_tag = "test_tag",
      table_tags = NULL,
      table_description = NULL
    )
    # Function should return FALSE for invalid env
    expect_false(result)
  })
})

test_that("check_params_before_publish validates dataframe parameter", {
  # Test with NULL dataframe
  expect_no_error({
    result <- check_params_before_publish(
      env = "DEV",
      dataframe = NULL,
      datamart = "test_datamart",
      table = "test_table",
      data_tag = "test_tag",
      table_tags = NULL,
      table_description = NULL
    )
    expect_false(result)
  })
  
  # Test with empty dataframe
  empty_df <- data.frame()
  expect_no_error({
    result <- check_params_before_publish(
      env = "DEV",
      dataframe = empty_df,
      datamart = "test_datamart",
      table = "test_table",
      data_tag = "test_tag",
      table_tags = NULL,
      table_description = NULL
    )
    expect_false(result)
  })
})

# Tests for check_params_before_unpublish function
test_that("check_params_before_unpublish validates unpublish parameters", {
  # Test with valid parameters
  expect_no_error({
    result <- check_params_before_unpublish(
      env = "DEV",
      datamart = "test_datamart",
      table = "test_table"
    )
  })
  
  # Test with invalid environment
  expect_no_error({
    result <- check_params_before_unpublish(
      env = "INVALID",
      datamart = "test_datamart",
      table = "test_table"
    )
    expect_false(result)
  })
  
  # Test with missing parameters
  expect_no_error({
    result <- check_params_before_unpublish(
      env = "DEV",
      datamart = NULL,
      table = "test_table"
    )
    expect_false(result)
  })
  
  expect_no_error({
    result <- check_params_before_unpublish(
      env = "DEV",
      datamart = "test_datamart",
      table = NULL
    )
    expect_false(result)
  })
})

# Tests for check_params_before_describe function
test_that("check_params_before_describe validates describe parameters", {
  # Test with valid parameters
  expect_no_error({
    result <- check_params_before_describe(
      env = "DEV",
      schema = "test_schema",
      table = "test_table",
      new_table_tags = list(category = "test"),
      new_table_description = "Test description"
    )
  })
  
  # Test with invalid environment
  expect_no_error({
    result <- check_params_before_describe(
      env = "INVALID",
      schema = "test_schema",
      table = "test_table",
      new_table_tags = NULL,
      new_table_description = NULL
    )
    expect_false(result)
  })
  
  # Test with missing schema
  expect_no_error({
    result <- check_params_before_describe(
      env = "DEV",
      schema = NULL,
      table = "test_table",
      new_table_tags = NULL,
      new_table_description = NULL
    )
    expect_false(result)
  })
  
  # Test with missing table
  expect_no_error({
    result <- check_params_before_describe(
      env = "DEV",
      schema = "test_schema",
      table = NULL,
      new_table_tags = NULL,
      new_table_description = NULL
    )
    expect_false(result)
  })
})

test_that("check_params_before_describe validates table tags format", {
  # Test with valid table tags (list format)
  expect_no_error({
    result <- check_params_before_describe(
      env = "DEV",
      schema = "test_schema",
      table = "test_table",
      new_table_tags = list(category = "test", owner = "user"),
      new_table_description = NULL
    )
  })
  
  # Test with invalid table tags (not a list)
  expect_no_error({
    result <- check_params_before_describe(
      env = "DEV",
      schema = "test_schema",
      table = "test_table",
      new_table_tags = "not_a_list",
      new_table_description = NULL
    )
    expect_false(result)
  })
})

# Tests for check_params_before_refresh function
test_that("check_params_before_refresh validates refresh parameters", {
  # Create a mock connection object for testing
  mock_con <- list(
    profile_name = "DEV",
    dbms.name = "test_schema"
  )
  class(mock_con) <- "mock_connection"
  
  # Test with valid parameters
  expect_no_error({
    result <- check_params_before_refresh(
      con = mock_con,
      schema = "test_schema",
      table = "test_table"
    )
  })
  
  # Test with NULL connection
  expect_no_error({
    result <- check_params_before_refresh(
      con = NULL,
      schema = "test_schema",
      table = "test_table"
    )
    expect_false(result)
  })
  
  # Test with missing schema
  expect_no_error({
    result <- check_params_before_refresh(
      con = mock_con,
      schema = NULL,
      table = "test_table"
    )
    expect_false(result)
  })
  
  # Test with missing table
  expect_no_error({
    result <- check_params_before_refresh(
      con = mock_con,
      schema = "test_schema",
      table = NULL
    )
    expect_false(result)
  })
})

test_that("validation functions handle error conditions gracefully", {
  # All validation functions should handle unexpected inputs gracefully
  # and return FALSE rather than throwing errors
  
  expect_false(check_env(list()))
  expect_false(check_database(matrix(1:4, 2, 2)))
  
  # Test with very long strings
  long_string <- paste(rep("a", 1000), collapse = "")
  expect_false(check_env(long_string))
  expect_false(check_database(long_string))
  
  # Test with special characters
  expect_false(check_env("DEV\n"))
  expect_false(check_env("DEV\t"))
  expect_false(check_database("datawarehouse\n"))
})

test_that("validation functions are consistent in their return types", {
  # All validation functions should return logical values
  expect_type(check_env("DEV"), "logical")
  expect_type(check_env("INVALID"), "logical")
  expect_type(check_database("datawarehouse"), "logical")
  expect_type(check_database("invalid"), "logical")
  
  # Length should always be 1
  expect_length(check_env("DEV"), 1)
  expect_length(check_database("datawarehouse"), 1)
})
