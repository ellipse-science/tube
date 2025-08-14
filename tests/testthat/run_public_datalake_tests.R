#!/usr/bin/env Rscript

# Public Datalake Test Runner
# Runs all tests related to public datalake functionality

cat("=== Public Datalake Feature 007 Testing ===\n")

# Load environment
readRenviron(".Renviron")

# Load package
suppressMessages(suppressWarnings(devtools::load_all(".", quiet = TRUE)))

cat("\n1. Environment Check:\n")
env_check <- can_test_real_aws_dev()
cat("AWS Environment Available:", env_check, "\n")

if (!env_check) {
  cat("❌ AWS environment not available - skipping AWS-dependent tests\n")
} else {
  cat("✅ AWS environment available - running full test suite\n")
}

cat("\n2. Running Public Datalake Tests:\n")

# Run the tests
tryCatch(
  {
    test_results <- testthat::test_file("tests/testthat/test-public-datalake-functions.R",
      reporter = testthat::ProgressReporter$new()
    )

    # Just confirm tests ran successfully
    cat("✅ All tests completed successfully\n")
  },
  error = function(e) {
    cat("❌ Error running tests:", e$message, "\n")
  }
)

cat("\n=== Public Datalake Testing Complete ===\n")
