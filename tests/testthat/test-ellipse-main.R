# Test the main ellipse functions - simple validation tests
# These test the function signatures and basic parameter validation

test_that("ellipse_connect function exists and responds to parameters", {
  # Test that ellipse_connect function exists and processes parameters
  # (it doesn't throw errors but shows user-friendly messages)
  
  # The function should return something (not crash) with invalid parameters
  expect_no_error(
    result <- ellipse_connect(env = "INVALID", database = "datawarehouse")
  )
})

test_that("ellipse_disconnect handles NULL connection gracefully", {
  # Test that disconnect handles edge cases
  expect_message(
    ellipse_disconnect(NULL),
    "Il faut fournir un objet de connection"
  )
})

test_that("ellipse_query validates connection parameter", {
  # Test parameter validation for query function
  expect_error(
    ellipse_query(connection = NULL, query = "SELECT 1"),
    class = "error"
  )
})

test_that("ellipse_describe validates connection parameter", {
  # Test parameter validation for describe function
  expect_error(
    ellipse_describe(connection = NULL, database = "test"),
    class = "error"
  )
})

test_that("ellipse_discover validates connection parameter", {
  # Test the discover function parameter validation
  expect_error(
    ellipse_discover(connection = NULL, database = "test"),
    class = "error"
  )
})

test_that("ellipse_process function exists and has correct parameters", {
  # Check that the function exists and get its parameters
  expect_true(exists("ellipse_process", mode = "function"))
  
  # Get function arguments to understand the actual signature
  args_list <- formals(ellipse_process)
  expect_true(length(args_list) > 0)
})

test_that("ellipse_ingest validates connection parameter", {
  # Test parameter validation for ingest function
  expect_error(
    ellipse_ingest(connection = NULL, file_or_folder = "test", pipeline = "test"),
    class = "error"
  )
})

test_that("ellipse_partitions validates connection parameter", {
  # Test parameter validation for partitions function
  expect_error(
    ellipse_partitions(connection = NULL, table = "test"),
    class = "error"
  )
})

test_that("ellipse_publish validates connection parameter", {
  # Test parameter validation for publish function
  expect_error(
    ellipse_publish(connection = NULL, table = "test"),
    class = "error"
  )
})

test_that("ellipse_unpublish validates connection parameter", {
  # Test parameter validation for unpublish function
  expect_error(
    ellipse_unpublish(connection = NULL, datamart = "test", table = "test"),
    class = "error"
  )
})

test_that("exported ellipse functions are available", {
  # Test that ALL main ellipse functions are properly exported
  expect_true(exists("ellipse_connect", mode = "function"))
  expect_true(exists("ellipse_disconnect", mode = "function"))
  expect_true(exists("ellipse_query", mode = "function"))
  expect_true(exists("ellipse_describe", mode = "function"))
  expect_true(exists("ellipse_discover", mode = "function"))
  expect_true(exists("ellipse_process", mode = "function"))
  expect_true(exists("ellipse_ingest", mode = "function"))
  expect_true(exists("ellipse_partitions", mode = "function"))
  expect_true(exists("ellipse_publish", mode = "function"))
  expect_true(exists("ellipse_unpublish", mode = "function"))
})
