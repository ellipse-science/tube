# Setup file for tests - run before all tests
# Following requirement: "use real life connections and data... Do not mock everything"

# Load environment variables for real testing
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

# Verify real testing environment is available
if (requireNamespace("testthat", quietly = TRUE)) {
  # Check if real AWS credentials are available
  aws_dev_key <- Sys.getenv("AWS_ACCESS_KEY_ID_DEV")
  aws_region <- Sys.getenv("AWS_REGION")
  
  if (nzchar(aws_dev_key) && nzchar(aws_region)) {
    message("✅ Real AWS testing environment available")
  } else {
    message("⚠️  Real AWS credentials not found - some tests may be skipped")
  }
}
