# Test helper functions for tube package
# These functions provide common testing utilities across all test files

# Mock AWS credentials for testing
setup_mock_aws_env <- function() {
  Sys.setenv(
    AWS_ACCESS_KEY_ID_DEV = "mock_dev_key_id",
    AWS_SECRET_ACCESS_KEY_DEV = "mock_dev_secret_key",
    AWS_ACCESS_KEY_ID_PROD = "mock_prod_key_id",
    AWS_SECRET_ACCESS_KEY_PROD = "mock_prod_secret_key",
    AWS_REGION = "ca-central-1"
  )
}

# Clean up AWS environment variables
cleanup_aws_env <- function() {
  Sys.unsetenv(c(
    "AWS_ACCESS_KEY_ID_DEV",
    "AWS_SECRET_ACCESS_KEY_DEV",
    "AWS_ACCESS_KEY_ID_PROD",
    "AWS_SECRET_ACCESS_KEY_PROD",
    "AWS_REGION"
  ))
}

# Create a mock connection object for testing
create_mock_connection <- function(env = "DEV", database = "datawarehouse") {
  # This creates a mock DBI connection-like object for testing
  # without actually connecting to AWS
  structure(
    list(
      profile_name = env,
      dbms.name = database,
      mock = TRUE
    ),
    class = c("MockConnection", "DBIConnection")
  )
}

# Mock DBI methods for testing
# Note: These will be added as S3 methods when DBI is loaded

# Helper to temporarily set environment variables for a test
with_env_vars <- function(vars, code) {
  old_vars <- Sys.getenv(names(vars), names = TRUE, unset = NA)
  do.call(Sys.setenv, vars)
  on.exit({
    # Restore old values
    to_unset <- names(old_vars)[is.na(old_vars)]
    if (length(to_unset) > 0) {
      Sys.unsetenv(to_unset)
    }
    to_set <- old_vars[!is.na(old_vars)]
    if (length(to_set) > 0) {
      do.call(Sys.setenv, as.list(to_set))
    }
  })
  force(code)
}

# Expected error messages for testing
expected_errors <- list(
  missing_env = "Oups, il faut choisir un environnement!",
  missing_database = "Oups, il faut choisir une base de données!",
  invalid_aws_creds = "Oups, il semble que vos clés d'accès ne sont pas valides!",
  missing_aws_creds = "Nous n'avons pas trouvé vos clés d'accès AWS",
  invalid_env = "Oups, l'environnement que vous avez spécifié n'est pas valide!"
)
