#!/usr/bin/env Rscript

# Universal ad-hoc R script for any temporary R operations
# This is the ONLY file to use for ad-hoc R testing/analysis
# Always reuse this file instead of creating new R scripts

cat("=== Ad-hoc R Script ===\n")

# Load environment if available
if (file.exists('.Renviron')) {
  readRenviron('.Renviron')
  cat("✅ Environment loaded\n")
}

# Add your ad-hoc R code here:
# Current operation: Check test coverage

cat("🔍 Checking test coverage...\n")

# Install and load covr if needed
if (!require(covr, quietly = TRUE)) {
  install.packages("covr")
  library(covr)
}

# Get coverage
tryCatch({
  coverage <- package_coverage()
  coverage_percent <- percent_coverage(coverage)
  
  cat("� Test Coverage:", coverage_percent, "%\n")
  
  if (coverage_percent >= 100) {
    cat("✅ Excellent! 100% test coverage achieved\n")
  } else if (coverage_percent >= 80) {
    cat("⚠️  Good coverage, but aim for 100%\n")
  } else {
    cat("❌ Coverage below 80% - needs improvement\n")
  }
  
  cat("📋 Coverage details:\n")
  print(coverage)
  
}, error = function(e) {
  cat("❌ Error checking coverage:", e$message, "\n")
})

cat("=== Available test runners ===\n")
test_runners <- list.files("tests/testthat", pattern = "run_.*\\.R$")
for (runner in test_runners) {
  cat("   📁 tests/testthat/", runner, "\n")
}

cat("=== End ad-hoc script ===\n")
