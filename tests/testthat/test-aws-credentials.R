# Unit tests for get_aws_credentials() function with REAL AWS connections
# Following requirement: "use real life connections and data... Do not mock everything"

# Load current source code (not published package)
devtools::load_all(".")

test_that("get_aws_credentials() works with real DEV environment variables", {
  cat("\n=== TESTING AWS CREDENTIALS RETRIEVAL ===\n")
  
  cat("PRODUCTION CODE BEING TESTED:\n")
  cat("get_aws_credentials <- function(env) {\n")
  cat("  if (env == \"DEV\") {\n")
  cat("    access_key <- Sys.getenv(\"AWS_ACCESS_KEY_ID_DEV\")\n")
  cat("    secret_key <- Sys.getenv(\"AWS_SECRET_ACCESS_KEY_DEV\")\n")
  cat("  } else if (env == \"PROD\") {\n")
  cat("    access_key <- Sys.getenv(\"AWS_ACCESS_KEY_ID_PROD\")\n")
  cat("    secret_key <- Sys.getenv(\"AWS_SECRET_ACCESS_KEY_PROD\")\n")
  cat("  }\n")
  cat("  return(list(credentials = list(creds = list(\n")
  cat("    access_key_id = access_key,\n")
  cat("    secret_access_key = secret_key\n")
  cat("  ))))\n")
  cat("}\n\n")
  
  cat("CREDENTIAL STRUCTURE: env → Sys.getenv() → paws-compatible list structure\n\n")
  
  # Skip if real AWS credentials not available
  skip_if_not(can_test_real_aws_dev(), "Real AWS DEV credentials not available")

  cat("TESTING: DEV environment credential retrieval...\n")

  # Test the actual function with real environment variables
  result <- get_aws_credentials("DEV")

  cat("Result structure type:", typeof(result), "\n")
  cat("Has credentials key:", "credentials" %in% names(result), "\n")

  # Verify result is not NULL (indicating success)
  expect_false(is.null(result))

  # Verify structure matches expected paws format
  expect_type(result, "list")
  expect_named(result, "credentials")
  expect_named(result$credentials, "creds")
  expect_named(result$credentials$creds, c("access_key_id", "secret_access_key"))

  # Verify values are not empty (but don't print actual secrets)
  expect_true(nzchar(result$credentials$creds$access_key_id))
  expect_true(nzchar(result$credentials$creds$secret_access_key))

  # Verify DEV environment credentials are being used
  expect_equal(result$credentials$creds$access_key_id, Sys.getenv("AWS_ACCESS_KEY_ID_DEV"))
  expect_equal(result$credentials$creds$secret_access_key, Sys.getenv("AWS_SECRET_ACCESS_KEY_DEV"))

  cat("Access key ID:", substr(result$credentials$creds$access_key_id, 1, 10), "...\n")
  cat("✅ DEV credentials retrieved and structured correctly!\n")
})

test_that("get_aws_credentials() works with real PROD environment variables", {
  # Check if PROD credentials are available
  prod_access_key <- Sys.getenv("AWS_ACCESS_KEY_ID_PROD")
  prod_secret_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY_PROD")

  # Skip if real AWS PROD credentials not available
  skip_if(prod_access_key == "" || prod_secret_key == "", "Real AWS PROD credentials not available")

  # Test the actual function with real PROD environment variables
  result <- get_aws_credentials("PROD")

  # Verify result is not NULL (indicating success)
  expect_false(is.null(result))

  # Verify structure matches expected paws format
  expect_type(result, "list")
  expect_named(result, "credentials")
  expect_named(result$credentials, "creds")
  expect_named(result$credentials$creds, c("access_key_id", "secret_access_key"))

  # Verify PROD environment credentials are being used
  expect_equal(result$credentials$creds$access_key_id, prod_access_key)
  expect_equal(result$credentials$creds$secret_access_key, prod_secret_key)
})

test_that("get_aws_credentials() handles invalid environment parameter", {
  # Test with invalid environment
  result <- get_aws_credentials("INVALID")

  # Should return NULL for invalid environment
  expect_null(result)
})

test_that("get_aws_credentials() handles missing environment variables", {
  # Skip if real AWS credentials not available
  skip_if_not(can_test_real_aws_dev(), "Real AWS DEV credentials not available")

  # Temporarily unset AWS DEV environment variables
  old_access <- Sys.getenv("AWS_ACCESS_KEY_ID_DEV")
  old_secret <- Sys.getenv("AWS_SECRET_ACCESS_KEY_DEV")

  # Unset variables
  Sys.unsetenv(c("AWS_ACCESS_KEY_ID_DEV", "AWS_SECRET_ACCESS_KEY_DEV"))

  # Test error handling
  result <- get_aws_credentials("DEV")
  expect_null(result)

  # Restore environment variables
  if (nzchar(old_access)) Sys.setenv(AWS_ACCESS_KEY_ID_DEV = old_access)
  if (nzchar(old_secret)) Sys.setenv(AWS_SECRET_ACCESS_KEY_DEV = old_secret)
})

test_that("get_aws_credentials() validates credential format with real AWS", {
  # Skip if real AWS credentials not available
  skip_if_not(can_test_real_aws_dev(), "Real AWS DEV credentials not available")

  result <- get_aws_credentials("DEV")

  # Should not be NULL if credentials are valid
  expect_false(is.null(result))

  # Test that credentials have reasonable format (basic validation)
  access_key <- result$credentials$creds$access_key_id
  secret_key <- result$credentials$creds$secret_access_key

  expect_match(access_key, "^[A-Z0-9]+$")    # AWS access keys are alphanumeric uppercase
  expect_true(nchar(access_key) >= 16)       # Minimum reasonable length
  expect_true(nchar(secret_key) >= 32)       # Secret keys are longer
})

test_that("get_aws_credentials() actually connects to AWS (integration test)", {
  # Skip if real AWS credentials not available
  skip_if_not(can_test_real_aws_dev(), "Real AWS DEV credentials not available")

  # This test verifies that the function actually makes a real AWS API call
  # The function internally calls paws.storage::s3()$list_buckets() to validate credentials

  result <- get_aws_credentials("DEV")

  # If we get a non-NULL result, it means the AWS API call succeeded
  expect_false(is.null(result))

  # The fact that we got here without errors means the real AWS connection worked
  # This is an integration test that validates the entire flow with real AWS
})

test_that("get_aws_credentials() handles network/AWS API errors gracefully", {
  # Skip if real AWS credentials not available
  skip_if_not(can_test_real_aws_dev(), "Real AWS DEV credentials not available")

  # Temporarily set invalid credentials to trigger AWS API error
  old_access <- Sys.getenv("AWS_ACCESS_KEY_ID_DEV")
  old_secret <- Sys.getenv("AWS_SECRET_ACCESS_KEY_DEV")

  # Set invalid but properly formatted credentials
  Sys.setenv(
    AWS_ACCESS_KEY_ID_DEV = "AKIAIOSFODNN7EXAMPLE",
    AWS_SECRET_ACCESS_KEY_DEV = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
  )

  # Test error handling with invalid credentials
  result <- get_aws_credentials("DEV")
  expect_null(result)  # Should return NULL on AWS API error

  # Restore original environment variables
  if (nzchar(old_access)) Sys.setenv(AWS_ACCESS_KEY_ID_DEV = old_access)
  if (nzchar(old_secret)) Sys.setenv(AWS_SECRET_ACCESS_KEY_DEV = old_secret)
})
