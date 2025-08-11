#!/usr/bin/env Rscript

cat("=== Debug Partition Data Type ===\n")

# Load environment
readRenviron('.Renviron')

# Load the package
devtools::load_all()

cat("\n1. Getting credentials...\n")
creds <- get_real_aws_credentials_dev()
cat("✅ Credentials obtained\n")

cat("\n2. Testing list_landing_zone_partitions return type...\n")
result <- list_landing_zone_partitions(creds)

cat("Result type:", class(result), "\n")
cat("Result length:", length(result), "\n")
cat("Result structure:\n")
str(result)

if (!is.null(result) && length(result) > 0) {
  cat("\nFirst few elements:\n")
  print(head(result, 3))
  
  cat("\nElement types:\n")
  for (i in 1:min(3, length(result))) {
    cat("Element", i, "- Type:", class(result[[i]]), "Value:", result[[i]], "\n")
  }
}

cat("\n3. Testing underlying list_s3_partitions...\n")
landing_bucket <- list_landing_zone_bucket(creds)
cat("Landing bucket:", landing_bucket, "\n")

if (!is.null(landing_bucket)) {
  s3_result <- list_s3_partitions(creds, landing_bucket)
  cat("S3 partitions type:", class(s3_result), "\n")
  cat("S3 partitions length:", length(s3_result), "\n")
  cat("S3 partitions structure:\n")
  str(s3_result)
}

cat("\n=== Testing Complete ===\n")
