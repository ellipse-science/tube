#!/usr/bin/env Rscript

# Testing landing function issues systematically
cat("=== Testing Landing Function Issues ===\n")

# Load environment
readRenviron('.Renviron')
library(devtools)
library(testthat)
devtools::load_all(".")

cat("\n1. Function signature issues:\n")
cat("   upload_file_to_landing_zone expected 3 args, actual:", length(formals(upload_file_to_landing_zone)), "\n")
cat("   parse_landing_zone_input expected 1 arg, actual:", length(formals(parse_landing_zone_input)), "\n")

cat("\n2. Testing parse_landing_zone_input with non-existent path:\n")
tryCatch({
  result <- parse_landing_zone_input("/non/existent/path", NULL)
  cat("   Result type:", class(result), "\n")
  cat("   Result value:", result, "\n")
}, error = function(e) {
  cat("   ❌ Error:", e$message, "\n")
})

cat("\n3. Testing upload_file_to_landing_zone parameter mismatch:\n")
# Check what the test is trying to call vs what function expects
cat("   Function parameters:", paste(names(formals(upload_file_to_landing_zone)), collapse=", "), "\n")

cat("\n4. Testing list_s3_buckets without type parameter:\n")
tryCatch({
  creds <- get_aws_credentials()
  result <- list_s3_buckets(creds)  # Missing 'type' parameter 
  cat("   Should fail - missing type parameter\n")
}, error = function(e) {
  cat("   ✅ Correctly failed:", e$message, "\n")
})

cat("\n5. Testing landing zone bucket listing:\n")
tryCatch({
  creds <- get_aws_credentials()
  result <- list_landing_zone_bucket(creds)
  cat("   ✅ Landing bucket found:", length(result), "buckets\n")
}, error = function(e) {
  cat("   ❌ Error:", e$message, "\n")
})

cat("\n=== Landing Issues Analysis Complete ===\n")
