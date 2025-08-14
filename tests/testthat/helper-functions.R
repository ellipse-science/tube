# Real connection helper functions for tube package
# Following requirement: "use real life connections and data... Do not mock everything"

# REAL AWS Helper Functions
#' Get real AWS credentials for testing (DEV environment only)
#' Uses actual environment variables from .Renviron
#' @return Named list with AWS credentials in production-compatible format or NULL if not available
get_real_aws_credentials_dev <- function() {
  # cat("🔍 [TEST HELPER] get_real_aws_credentials_dev() called\n")

  access_key <- Sys.getenv("AWS_ACCESS_KEY_ID_DEV")
  secret_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY_DEV")
  region <- Sys.getenv("AWS_REGION")

  # cat("    Environment variables retrieved:\n")
  # cat("    AWS_ACCESS_KEY_ID_DEV: ", substr(access_key, 1, 10), "...\n")
  # cat("    AWS_SECRET_ACCESS_KEY_DEV: [HIDDEN]\n")
  # cat("    AWS_REGION: ", region, "\n")

  if (nzchar(access_key) && nzchar(secret_key) && nzchar(region)) {
    # Return structure that matches production get_aws_credentials() format
    result <- list(
      credentials = list(
        creds = list(
          access_key_id = access_key,
          secret_access_key = secret_key
        )
      )
    )
    # cat("✅ [TEST HELPER] Returning production-compatible nested credential structure:\n")
    # cat("    Type: ", typeof(result), ", Length: ", length(result), "\n")
    # cat("    Names: ", paste(names(result), collapse = ", "), "\n")
    result
  } else {
    # cat("❌ [TEST HELPER] Missing credentials, returning NULL\n")
    NULL
  }
}

#' Check if real AWS DEV credentials are available for testing
#' @return Logical indicating if real AWS testing is possible
can_test_real_aws_dev <- function() {
  # cat("🔍 [TEST HELPER] can_test_real_aws_dev() called\n")
  creds <- get_real_aws_credentials_dev()
  result <- !is.null(creds)
  result
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
#' @param creds Credential structure from get_real_aws_credentials_dev()
#' @return Logical indicating if credentials have valid format
validate_aws_credential_format <- function(creds) {
  if (is.null(creds) || is.null(creds$credentials) || is.null(creds$credentials$creds)) {
    return(FALSE)
  }

  access_key_id <- creds$credentials$creds$access_key_id
  secret_access_key <- creds$credentials$creds$secret_access_key

  access_key_valid <- grepl("^[A-Z0-9]+$", access_key_id) && nchar(access_key_id) >= 16
  secret_key_valid <- nchar(secret_access_key) >= 32

  access_key_valid && secret_key_valid
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
    return(FALSE)
  }

  creds <- get_real_aws_credentials_dev()
  if (!validate_aws_credential_format(creds)) {
    message("AWS credentials found but format validation failed")
    return(FALSE)
  }

  message("✅ Real testing credentials available and properly formatted")
  invisible(TRUE)
}

# REAL Environment Helper Functions
#' Load environment variables from .Renviron for testing
#' @return Logical indicating if environment was loaded successfully
load_test_environment <- function() {
  if (file.exists(".Renviron")) {
    readRenviron(".Renviron")
    invisible(TRUE)
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

# Security Helper Functions
#' Safely display credential structure without exposing sensitive data
#' @param creds Credential structure to display
#' @return Nothing (prints to console)
safe_display_credentials <- function(creds) {
  if (is.null(creds)) {
    cat("    NULL\n")
    return(invisible())
  }

  safe_creds <- creds

  # Handle flat structure (test helper format)
  if (!is.null(safe_creds$secret_access_key)) {
    safe_creds$secret_access_key <- "[HIDDEN]"
  }
  if (!is.null(safe_creds$access_key_id)) {
    safe_creds$access_key_id <- paste0(substr(safe_creds$access_key_id, 1, 10), "...")
  }

  # Handle nested structure (production format)
  if (!is.null(safe_creds$credentials$creds$secret_access_key)) {
    safe_creds$credentials$creds$secret_access_key <- "[HIDDEN]"
  }
  if (!is.null(safe_creds$credentials$creds$access_key_id)) {
    safe_creds$credentials$creds$access_key_id <- paste0(
      substr(safe_creds$credentials$creds$access_key_id, 1, 10), "..."
    )
  }

  str(safe_creds, max.level = 3)
}

# Production Function Call Debugging
#' Debug wrapper for production get_aws_credentials() function
#' Shows exactly what's being called and what's returned
debug_get_aws_credentials <- function(env) {
  cat("🎯 [PRODUCTION CALL] get_aws_credentials(env = '", env, "')\n")

  result <- get_aws_credentials(env)

  cat("📦 [PRODUCTION RESULT] get_aws_credentials() returned:\n")
  if (is.null(result)) {
    cat("    NULL\n")
  } else {
    cat("    Type: ", typeof(result), ", Length: ", length(result), "\n")
    cat("    Names: ", paste(names(result), collapse = ", "), "\n")
    cat("    Structure preview (sensitive data hidden):\n")
    # Create safe structure for display
    safe_result <- result
    if (!is.null(safe_result$credentials$creds$secret_access_key)) {
      safe_result$credentials$creds$secret_access_key <- "[HIDDEN]"
    }
    if (!is.null(safe_result$credentials$creds$access_key_id)) {
      safe_result$credentials$creds$access_key_id <- paste0(
        substr(safe_result$credentials$creds$access_key_id, 1, 10), "..."
      )
    }
    str(safe_result, max.level = 3)
  }

  result
}

#' Debug wrapper for production bucket functions
#' Shows function calls and credential structures being passed
debug_bucket_function <- function(func_name, credentials, ...) {
  cat("🎯 [PRODUCTION CALL] ", func_name, "() called\n")
  cat("📋 [CREDENTIALS PASSED] Structure (sensitive data hidden):\n")
  if (is.null(credentials)) {
    cat("    NULL\n")
  } else {
    cat("    Type: ", typeof(credentials), ", Length: ", length(credentials), "\n")
    cat("    Names: ", paste(names(credentials), collapse = ", "), "\n")

    # Create safe structure for display
    safe_creds <- credentials
    if (!is.null(safe_creds$secret_access_key)) {
      safe_creds$secret_access_key <- "[HIDDEN]"
    }
    if (!is.null(safe_creds$access_key_id)) {
      safe_creds$access_key_id <- paste0(substr(safe_creds$access_key_id, 1, 10), "...")
    }
    if (!is.null(safe_creds$credentials$creds$secret_access_key)) {
      safe_creds$credentials$creds$secret_access_key <- "[HIDDEN]"
    }
    if (!is.null(safe_creds$credentials$creds$access_key_id)) {
      safe_creds$credentials$creds$access_key_id <- paste0(
        substr(safe_creds$credentials$creds$access_key_id, 1, 10), "..."
      )
    }

    str(safe_creds, max.level = 3)
  }

  # Call the actual function based on name
  result <- tryCatch(
    {
      switch(func_name,
        "list_athena_staging_bucket" = list_athena_staging_bucket(credentials),
        "list_datalake_bucket" = list_datalake_bucket(credentials),
        "list_datamarts_bucket" = list_datamarts_bucket(credentials),
        "list_datawarehouse_bucket" = list_datawarehouse_bucket(credentials),
        "list_landing_zone_bucket" = list_landing_zone_bucket(credentials),
        stop("Unknown function: ", func_name)
      )
    },
    error = function(e) {
      cat("❌ [PRODUCTION ERROR] ", func_name, "() failed: ", e$message, "\n")
      stop(e)
    }
  )

  cat("✅ [PRODUCTION RESULT] ", func_name, "() returned ", length(result), " items\n")
  result
}
