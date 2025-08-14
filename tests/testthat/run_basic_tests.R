#!/usr/bin/env Rscript

# Test runner for basic utility functions ONLY
# Tests: tests/testthat/test-basic-functions.R

cat("=== TESTING BASIC UTILITY FUNCTIONS ===\n")

# Setup
readRenviron(".Renviron")
library(testthat, quietly = TRUE)
library(devtools, quietly = TRUE)
suppressMessages(suppressWarnings(devtools::load_all(".", quiet = TRUE)))

# Run ONLY the basic function tests
cat("Running test-basic-functions.R...\n")
test_results <- test_file("tests/testthat/test-basic-functions.R", reporter = "summary")

cat("\n=== BASIC FUNCTIONS TESTING COMPLETE ===\n")
