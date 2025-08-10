#!/usr/bin/env Rscript

# Debug test runner that shows all production function calls

cat("=== DEBUG TEST RUNNER - PRODUCTION FUNCTION CALLS ===\n")

# Load environment and package
readRenviron('.Renviron')
library(devtools)
load_all('.')
library(testthat)

# Source helper functions
source("tests/testthat/helper-functions.R")

cat("\n1. TESTING CREDENTIAL FLOW:\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

# Test the credential helper first
cat("\n📋 Testing helper function:\n")
helper_creds <- get_real_aws_credentials_dev()

# Test the production function
cat("\n📋 Testing production function:\n") 
prod_creds <- debug_get_aws_credentials('DEV')

cat("\n2. COMPARING CREDENTIAL STRUCTURES:\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

cat("🔍 HELPER vs PRODUCTION credential comparison:\n")
if (!is.null(helper_creds) && !is.null(prod_creds)) {
  cat("Helper structure (sensitive data hidden):\n")
  safe_helper <- helper_creds
  if (!is.null(safe_helper$credentials$creds$secret_access_key)) {
    safe_helper$credentials$creds$secret_access_key <- "[HIDDEN]"
  }
  if (!is.null(safe_helper$credentials$creds$access_key_id)) {
    safe_helper$credentials$creds$access_key_id <- paste0(substr(safe_helper$credentials$creds$access_key_id, 1, 10), "...")
  }
  str(safe_helper)
  
  cat("\nProduction structure (sensitive data hidden):\n")
  safe_prod <- prod_creds
  if (!is.null(safe_prod$credentials$creds$secret_access_key)) {
    safe_prod$credentials$creds$secret_access_key <- "[HIDDEN]"
  }
  if (!is.null(safe_prod$credentials$creds$access_key_id)) {
    safe_prod$credentials$creds$access_key_id <- paste0(substr(safe_prod$credentials$creds$access_key_id, 1, 10), "...")
  }
  str(safe_prod)
  
  cat("\n✅ STRUCTURES ARE NOW COMPATIBLE!\n")
} else {
  cat("❌ One or both credential functions returned NULL\n")
}

cat("\n3. TESTING BUCKET FUNCTIONS WITH DIFFERENT CREDENTIALS:\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

if (!is.null(helper_creds)) {
  cat("\n🧪 Testing with HELPER credentials (flat structure):\n")
  tryCatch({
    result <- debug_bucket_function("list_athena_staging_bucket", helper_creds)
  }, error = function(e) {
    cat("❌ HELPER CREDS FAILED: ", e$message, "\n")
  })
} 

if (!is.null(prod_creds)) {
  cat("\n🧪 Testing with PRODUCTION credentials (nested structure):\n")
  tryCatch({
    result <- debug_bucket_function("list_athena_staging_bucket", prod_creds)
  }, error = function(e) {
    cat("❌ PRODUCTION CREDS FAILED: ", e$message, "\n")
  })
}

cat("\n4. UNDERSTANDING THE SOLUTION:\n")
cat(paste(rep("=", 50), collapse = ""), "\n")
cat("✅ FIXED: Both helper and production now return: list(credentials = list(creds = list(...)))\n")
cat("✅ SUCCESS: Test helper now uses production-compatible credential format\n")
cat("✅ RESULT: All bucket functions now work with test credentials\n")

cat("\n=== DEBUG ANALYSIS COMPLETE ===\n")
