#!/usr/bin/env Rscript

# Test runner for ellipse functions ONLY
# Tests: tests/testthat/test-ellipse-main.R

cat("=== TESTING ellipse_* FUNCTIONS ===\n")

# Setup
readRenviron('.Renviron')
library(testthat, quietly = TRUE)
library(devtools, quietly = TRUE)
load_all('.', quiet = TRUE)

# Run ONLY the ellipse tests
cat("Running test-ellipse-main.R...\n")
test_results <- test_file('tests/testthat/test-ellipse-main.R', reporter = "summary")

cat("\n=== ellipse_* FUNCTIONS TESTING COMPLETE ===\n")
