#!/usr/bin/env Rscript

# Master test runner - runs ALL unit tests
# Calls individual test files in proper sequence

cat("=== MASTER TEST RUNNER - ALL UNIT TESTS ===\n")

# Setup
readRenviron(".Renviron")
library(testthat, quietly = TRUE)
library(devtools, quietly = TRUE)
load_all(".", quiet = TRUE)

# Test sequence
test_files <- c(
  "tests/testthat/test-basic-functions.R",
  "tests/testthat/test-aws-credentials.R",
  "tests/testthat/test-ellipse-main.R"
)

total_tests <- 0
total_failures <- 0

for (test_file in test_files) {
  cat("\n", rep("=", 60), "\n")
  cat("Running:", basename(test_file), "\n")
  cat(rep("=", 60), "\n")

  result <- test_file(test_file, reporter = "summary")

  if (!is.null(result)) {
    total_tests <- total_tests + sum(result$nb)
    total_failures <- total_failures + sum(result$failed)
  }
}

cat("\n", rep("=", 60), "\n")
cat("=== MASTER TEST SUMMARY ===\n")
cat("Total tests run:", total_tests, "\n")
cat("Total failures:", total_failures, "\n")
cat("Success rate:", round((total_tests - total_failures) / total_tests * 100, 1), "%\n")
cat("=== ALL TESTING COMPLETE ===\n")
