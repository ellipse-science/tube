#!/usr/bin/env Rscript

#' Test coverage analysis
#' 
#' Generates test coverage report for the package using covr.
#' This script helps ensure comprehensive test coverage.

cat("=== Test Coverage Analysis ===\n")

# Load required packages
required_packages <- c("covr")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("❌ Required package", pkg, "not installed\n")
    quit(status = 1)
  }
}

cat("📊 Calculating test coverage...\n")

# Calculate coverage
tryCatch({
  coverage <- covr::package_coverage()
  coverage_percent <- covr::percent_coverage(coverage)
  
  cat("📈 Test coverage:", round(coverage_percent, 2), "%\n")
  
  if (coverage_percent < 80) {
    cat("⚠️  Low test coverage detected (< 80%)\n")
  } else if (coverage_percent < 95) {
    cat("✅ Good test coverage (80-95%)\n")
  } else {
    cat("🌟 Excellent test coverage (≥ 95%)\n")
  }
  
  # Show detailed coverage report
  cat("\n📋 Detailed coverage report:\n")
  print(coverage)
  
  quit(status = 0)
  
}, error = function(e) {
  cat("❌ Error calculating coverage:\n")
  cat(e$message, "\n")
  quit(status = 1)
})
