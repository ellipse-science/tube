# Real connection helper functions for tube package
# Following requirement: "use real life connections and data... Do not mock everything"

# REAL AWS Helper Functions
#' Get real AWS credentials for testing (DEV environment only)
#' Uses actual environment variables from .Renviron
#' @return Named list with AWS credentials or NULL if not available
get_real_aws_credentials_dev <- function() {
  access_key <- Sys.getenv("AWS_ACCESS_KEY_ID_DEV")
  secret_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY_DEV")
  region <- Sys.getenv("AWS_REGION")

  if (nzchar(access_key) && nzchar(secret_key) && nzchar(region)) {
    list(
      access_key_id = access_key,
      secret_access_key = secret_key,
      region = region
    )
  } else {
    NULL
  }
}

#' Check if real AWS DEV credentials are available for testing
#' @return Logical indicating if real AWS testing is possible
can_test_real_aws_dev <- function() {
  creds <- get_real_aws_credentials_dev()
  !is.null(creds)
}

#' Get actual AWS region from environment variables
#' @return AWS region string or NULL if not set
get_real_aws_region <- function() {
  region <- Sys.getenv("AWS_REGION")
  if (nzchar(region)) {
    region
  } else {
    NULL
  }
}

#' Validate that real AWS credentials have proper format
#' @param access_key_id AWS access key ID to validate
#' @param secret_access_key AWS secret access key to validate
#' @param region AWS region to validate
#' @return Logical indicating if credentials have valid format
validate_aws_credential_format <- function(access_key_id, secret_access_key, region) {
  access_key_valid <- grepl("^[A-Z0-9]+$", access_key_id) && nchar(access_key_id) >= 16
  secret_key_valid <- nchar(secret_access_key) >= 32
  region_valid <- grepl("^[a-z0-9-]+$", region)

  access_key_valid && secret_key_valid && region_valid
}

# REAL Database Helper Functions
#' Check if real database credentials are available for testing
#' @param env Environment ("DEV" or "PROD")
#' @return Logical indicating if real database testing is possible
can_test_real_database <- function(env = "DEV") {
  aws_creds <- can_test_real_aws_dev()
  env_valid <- env %in% c("DEV", "PROD")
  aws_creds && env_valid
}

#' Setup for real testing - check if credentials are available
#' @return Logical indicating if real testing setup is ready
setup_real_testing <- function() {
  if (!can_test_real_aws_dev()) {
    message("Real AWS DEV credentials not available in environment variables")
    message("Required: AWS_ACCESS_KEY_ID_DEV, AWS_SECRET_ACCESS_KEY_DEV, AWS_REGION")
    FALSE
  }

  creds <- get_real_aws_credentials_dev()
  if (!validate_aws_credential_format(creds$access_key_id, creds$secret_access_key, creds$region)) {
    message("AWS credentials found but format validation failed")
    FALSE
  }

  message("✅ Real testing credentials available and properly formatted")
  TRUE
}

# REAL Environment Helper Functions
#' Load environment variables from .Renviron for testing
#' @return Logical indicating if environment was loaded successfully
load_test_environment <- function() {
  if (file.exists(".Renviron")) {
    readRenviron(".Renviron")
    TRUE
  } else {
    message("❌ .Renviron file not found")
    FALSE
  }
}

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
