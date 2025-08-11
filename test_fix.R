#!/usr/bin/env Rscript

cat("=== Test Fix for Partition Data Type ===\n")

# Load environment
readRenviron('.Renviron')

# Load the package
devtools::load_all()

cat("\n1. Getting credentials...\n")
creds <- get_real_aws_credentials_dev()

cat("\n2. Testing current behavior...\n")
landing_bucket <- list_landing_zone_bucket(creds)
current_result <- list_s3_partitions(creds, landing_bucket)
cat("Current type:", class(current_result), "\n")
cat("Current length:", length(current_result), "\n")

cat("\n3. Testing with unlist() fix...\n")
# Simulate the fix
if (!is.null(current_result)) {
  fixed_result <- unlist(current_result)
  cat("Fixed type:", class(fixed_result), "\n") 
  cat("Fixed length:", length(fixed_result), "\n")
  cat("Test expectations:\n")
  cat("  is.character(result):", is.character(fixed_result), "\n")
  cat("  is.null(result):", is.null(fixed_result), "\n")
  cat("  is.character(result) || is.null(result):", is.character(fixed_result) || is.null(fixed_result), "\n")
  
  if (length(fixed_result) > 0) {
    cat("  all(is.character(result)):", all(is.character(fixed_result)), "\n")
    cat("  all(nzchar(result)):", all(nzchar(fixed_result)), "\n")
  }
  
  cat("First few elements:\n")
  print(head(fixed_result, 3))
}

cat("\n=== Test Complete ===\n")
