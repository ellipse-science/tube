#!/usr/bin/env Rscript

#' Run R CMD check on the package
#'
#' Performs comprehensive package checking using devtools::check().
#' This script mirrors the package checking performed in GitHub Actions.

cat("=== Package Check ===\n")

# Set environment variables to match GitHub Actions
Sys.setenv("_R_CHECK_ASCII_CODE_" = "false")
Sys.setenv("_R_CHECK_ASCII_DATA_" = "false")

# Load required packages
required_packages <- c("devtools", "rcmdcheck")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("❌ Required package", pkg, "not installed\n")
    quit(status = 1)
  }
}

cat("📦 Running devtools::check()...\n")
cat("🔧 Environment: ASCII checks disabled (matching GitHub Actions)\n")

# Run package check
tryCatch({
  check_results <- devtools::check(
    args = c("--no-manual", "--no-tests", "--ignore-vignettes"),
    error_on = "error"
  )
  
  cat("✅ Package check completed successfully!\n")
  cat("📊 Check summary:\n")
  print(check_results)
  quit(status = 0)
  
}, error = function(e) {
  cat("❌ Package check failed:\n")
  cat(e$message, "\n")
  quit(status = 1)
})
