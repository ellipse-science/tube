#!/usr/bin/env Rscript

#!/usr/bin/env Rscript

#' Run package tests
#'
#' Runs the test suite using devtools::test().
#' This script mirrors the testing performed in GitHub Actions.

cat("=== Package Testing ===\n")

# Load required packages
required_packages <- c("devtools", "testthat")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("❌ Required package", pkg, "not installed\n")
    quit(status = 1)
  }
}

cat("🧪 Running devtools::test()...\n")

# Run tests
tryCatch({
  test_results <- devtools::test()
  
  if (any(test_results$failed > 0)) {
    cat("❌ Tests failed!\n")
    print(test_results)
    quit(status = 1)
  } else {
    cat("✅ All tests passed!\n")
    cat("📊 Test summary:\n")
    print(test_results)
    quit(status = 0)
  }
}, error = function(e) {
  cat("❌ Error running tests:\n")
  cat(e$message, "\n")
  quit(status = 1)
})
