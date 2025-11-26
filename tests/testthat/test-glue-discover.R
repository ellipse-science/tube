test_that("ellipse_discover works with specific table name in datamarts", {
  skip_if_not(nzchar(Sys.getenv("AWS_ACCESS_KEY_ID_DEV")),
    "AWS credentials not available")
  
  # Connect to DEV datamarts
  con <- ellipse_connect("DEV", "datamarts")
  
  # Get list of all tables first
  all_tables <- ellipse_discover(con)
  
  # Skip if no tables available
  skip_if(nrow(all_tables) == 0, "No tables in datamarts")
  
  # Get the first table name
  test_table <- all_tables$table_name[1]
  
  # Test discovering specific table
  result <- ellipse_discover(con, test_table)
  
  # Verify result structure
  expect_type(result, "list")
  expect_named(result, c("name", "description", "tags", "columns"))
  expect_equal(result$name, test_table)
  
  # Cleanup
  ellipse_disconnect(con)
})

test_that("get_aws_account_id returns valid account ID", {
  skip_if_not(nzchar(Sys.getenv("AWS_ACCESS_KEY_ID_DEV")),
    "AWS credentials not available")
  
  # Get credentials
  creds <- get_aws_credentials("DEV")
  
  # Get account ID
  account_id <- get_aws_account_id(creds)
  
  # Verify it's a valid account ID (12 digits)
  expect_type(account_id, "character")
  expect_match(account_id, "^[0-9]{12}$")
})

test_that("list_glue_table_properties works with CatalogId", {
  skip_if_not(nzchar(Sys.getenv("AWS_ACCESS_KEY_ID_DEV")),
    "AWS credentials not available")
  
  # Connect and get credentials
  con <- ellipse_connect("DEV", "datamarts")
  env <- DBI::dbGetInfo(con)$profile_name
  schema <- DBI::dbGetInfo(con)$dbms.name
  creds <- get_aws_credentials(env)
  
  # Get a table to test with
  tables <- DBI::dbGetQuery(con, paste0("SHOW TABLES IN ", schema))$tab_name
  
  skip_if(length(tables) == 0, "No tables available")
  
  test_table <- tables[1]
  
  # Test list_glue_table_properties
  result <- list_glue_table_properties(creds, schema, test_table)
  
  # Verify result
  expect_s3_class(result, "tbl_df")
  expect_true("table_name" %in% colnames(result))
  expect_equal(result$table_name, test_table)
  
  # Cleanup
  ellipse_disconnect(con)
})
