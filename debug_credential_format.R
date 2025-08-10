#!/usr/bin/env Rscript

# Test the fixed AWS credential structure

cat("=== Testing Fixed AWS Credentials ===\n")

# Load environment
readRenviron('.Renviron')

# Load all functions
library(devtools)
load_all('.')

cat("\n1. Testing fixed get_aws_credentials() function:\n")
creds <- get_aws_credentials('DEV')
cat("Credentials returned is NULL:", is.null(creds), "\n")

if (!is.null(creds)) {
  cat("\nFixed credential structure:\n")
  str(creds)
  
  cat("\n2. Testing bucket function calls:\n")
  tryCatch({
    result <- list_athena_staging_bucket(creds)
    cat("✅ list_athena_staging_bucket() works - result length:", length(result), "\n")
  }, error = function(e) {
    cat("❌ ERROR in list_athena_staging_bucket():", e$message, "\n")
  })
  
  tryCatch({
    result <- list_datalake_bucket(creds)
    cat("✅ list_datalake_bucket() works - result length:", length(result), "\n")
  }, error = function(e) {
    cat("❌ ERROR in list_datalake_bucket():", e$message, "\n")
  })
  
  tryCatch({
    result <- list_datamarts_bucket(creds)
    cat("✅ list_datamarts_bucket() works - result length:", length(result), "\n")
  }, error = function(e) {
    cat("❌ ERROR in list_datamarts_bucket():", e$message, "\n")
  })
  
  cat("\n3. Testing direct S3 function:\n")
  tryCatch({
    result <- list_s3_buckets(creds, "athenaqueryresults")
    cat("✅ list_s3_buckets() works - result length:", length(result), "\n")
  }, error = function(e) {
    cat("❌ ERROR in list_s3_buckets():", e$message, "\n")
  })
}

cat("\n=== Fixed Function Testing Complete ===\n")
