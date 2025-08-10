# Setup file for tests - run before all tests

# Register S3 methods for mock DBI connection when DBI is loaded
if (requireNamespace("DBI", quietly = TRUE)) {
  
  # Register dbGetInfo method for MockConnection
  dbGetInfo.MockConnection <- function(dbObj, ...) {
    list(
      profile_name = dbObj$profile_name,
      dbms.name = dbObj$dbms.name
    )
  }
  
  # Register dbIsValid method for MockConnection  
  dbIsValid.MockConnection <- function(dbObj, ...) {
    TRUE
  }
  
  # Register dbDisconnect method for MockConnection
  dbDisconnect.MockConnection <- function(conn, ...) {
    TRUE
  }
  
  # Manually register the S3 methods since we can't use the :: syntax in tests
  .GlobalEnv$dbGetInfo.MockConnection <- dbGetInfo.MockConnection
  .GlobalEnv$dbIsValid.MockConnection <- dbIsValid.MockConnection  
  .GlobalEnv$dbDisconnect.MockConnection <- dbDisconnect.MockConnection
}
