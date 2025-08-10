# Quality Assurance Development Scripts
# Run these scripts during development to ensure code quality

#' Run comprehensive linting checks
#' @return TRUE if all checks pass, FALSE otherwise
lint_and_check <- function() {
  cat("🔍 Running linting checks...\n")
  lints <- lintr::lint_package()
  
  if (length(lints) > 0) {
    cat("❌ Linting failed\n")
    print(lints)
    return(FALSE)
  } else {
    cat("✅ Linting passed\n")
    return(TRUE)
  }
}

#' Run all package tests
#' @return TRUE if all tests pass, FALSE otherwise
run_all_tests <- function() {
  cat("🧪 Running all tests...\n")
  
  # Set up mock environment variables for testing
  old_env <- Sys.getenv(c(
    "AWS_ACCESS_KEY_ID_DEV", "AWS_SECRET_ACCESS_KEY_DEV",
    "AWS_ACCESS_KEY_ID_PROD", "AWS_SECRET_ACCESS_KEY_PROD"
  ), names = TRUE, unset = NA)
  
  on.exit({
    # Restore environment variables
    missing_vars <- names(old_env)[is.na(old_env)]
    if (length(missing_vars) > 0) {
      Sys.unsetenv(missing_vars)
    }
    present_vars <- old_env[!is.na(old_env)]
    if (length(present_vars) > 0) {
      do.call(Sys.setenv, as.list(present_vars))
    }
  })
  
  # Set mock credentials for testing
  Sys.setenv(
    AWS_ACCESS_KEY_ID_DEV = "mock_dev_key_for_testing",
    AWS_SECRET_ACCESS_KEY_DEV = "mock_dev_secret_for_testing",
    AWS_ACCESS_KEY_ID_PROD = "mock_prod_key_for_testing", 
    AWS_SECRET_ACCESS_KEY_PROD = "mock_prod_secret_for_testing"
  )
  
  tryCatch({
    test_results <- devtools::test()
    failed_tests <- sum(sapply(test_results, function(x) length(x$failed)))
    
    if (failed_tests > 0) {
      cat("❌ Some tests failed\n")
      return(FALSE)
    } else {
      cat("✅ All tests passed\n")
      return(TRUE)
    }
  }, error = function(e) {
    cat("❌ Error running tests:", e$message, "\n")
    return(FALSE)
  })
}

#' Check test coverage
#' @return Coverage percentage
check_test_coverage <- function() {
  cat("📊 Checking test coverage...\n")
  
  tryCatch({
    coverage <- covr::package_coverage()
    coverage_percent <- covr::percent_coverage(coverage)
    
    cat("Test coverage:", coverage_percent, "%\n")
    
    if (coverage_percent < 100) {
      cat("⚠️  Coverage is below 100%. Uncovered lines:\n")
      uncovered <- covr::tally_coverage(coverage)
      uncovered_lines <- uncovered[uncovered$value == 0, ]
      if (nrow(uncovered_lines) > 0) {
        print(uncovered_lines[, c("filename", "line")])
      }
    }
    
    return(coverage_percent)
  }, error = function(e) {
    cat("❌ Error checking coverage:", e$message, "\n")
    return(0)
  })
}

#' Run full package check
#' @return TRUE if check passes, FALSE otherwise
run_package_check <- function() {
  cat("📦 Running full package check...\n")
  
  tryCatch({
    check_results <- devtools::check(
      args = c("--no-manual", "--as-cran"),
      error_on = "never"
    )
    
    if (length(check_results$errors) > 0) {
      cat("❌ Package check failed with errors:\n")
      lapply(check_results$errors, cat)
      return(FALSE)
    }
    
    if (length(check_results$warnings) > 0) {
      cat("⚠️  Package check completed with warnings:\n")
      lapply(check_results$warnings, cat)
    }
    
    cat("✅ Package check completed successfully\n")
    return(TRUE)
  }, error = function(e) {
    cat("❌ Error during package check:", e$message, "\n")
    return(FALSE)
  })
}

#' Run complete quality assurance pipeline
#' @return TRUE if all checks pass, FALSE otherwise
qa_pipeline <- function() {
  cat("🚀 Running complete QA pipeline...\n\n")
  
  results <- list(
    linting = lint_and_check(),
    tests = run_all_tests(),
    coverage = check_test_coverage() >= 95,  # Minimum 95% for now
    package_check = run_package_check()
  )
  
  cat("\n📋 QA Pipeline Results:\n")
  cat("Linting:", ifelse(results$linting, "✅ PASS", "❌ FAIL"), "\n")
  cat("Tests:", ifelse(results$tests, "✅ PASS", "❌ FAIL"), "\n") 
  cat("Coverage:", ifelse(results$coverage, "✅ PASS", "❌ FAIL"), "\n")
  cat("Package Check:", ifelse(results$package_check, "✅ PASS", "❌ FAIL"), "\n")
  
  all_passed <- all(unlist(results))
  
  if (all_passed) {
    cat("\n🎉 All QA checks passed. Ready for commit.\n")
  } else {
    cat("\n⚠️  Some QA checks failed. Fix issues before committing.\n")
  }
  
  return(all_passed)
}

#' Quick development check (linting + tests only)
#' @return TRUE if basic checks pass, FALSE otherwise  
quick_check <- function() {
  cat("⚡ Running quick development check...\n\n")
  
  linting_ok <- lint_and_check()
  tests_ok <- run_all_tests()
  
  if (linting_ok && tests_ok) {
    cat("\n✅ Quick check passed. Safe to continue development.\n")
    return(TRUE)
  } else {
    cat("\n❌ Quick check failed. Fix issues before continuing.\n")
    return(FALSE)
  }
}
