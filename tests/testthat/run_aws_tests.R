#!/usr/bin/env Rscript

# Test runner for get_aws_credentials() function ONLY
# Tests: tests/testthat/test-aws-credentials.R

cat("=== TESTING get_aws_credentials() FUNCTION ===\n")

# Setup
readRenviron(".Renviron")
library(testthat, quietly = TRUE)
library(devtools, quietly = TRUE)
load_all(".", quiet = TRUE)

# Run ONLY the AWS credentials tests
cat("Running test-aws-credentials.R...\n")
test_results <- test_file("tests/testthat/test-aws-credentials.R", reporter = "summary")

cat("\n=== get_aws_credentials() TESTING COMPLETE ===\n")
