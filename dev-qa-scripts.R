# Quality Assurance Development Scripts
# Run these scripts during development to ensure code quality

#' Run comprehensive linting checks
#' @return TRUE if all checks pass, FALSE otherwise
lint_and_check <- function() {
  cat("🔍 Running linting checks...\n")
  lints <- lintr::lint_package()
  
  if (length(lints) > 0) {
    cat("❌ Linting failed!\n")
    cat("Found", length(lints), "linting issues\n\n")
    
    # Use enhanced analysis
    issue_summary <- .analyze_linting_issues(lints)
    .display_linting_summary(issue_summary, lints)
    
    cat("\n💡 To fix:\n")
    cat("  - Run lint_interactive() for guided fixing\n")
    cat("  - Run styler::style_pkg() for auto-fixes\n")
    cat("  - Use lint_files() for specific files\n")
    
    return(FALSE)
  } else {
    cat("✅ Linting passed!\n")
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

# Enhanced Linting Development Tools

#' Interactive linting check with detailed reporting
#' @param show_progress Logical, whether to show progress information
#' @param return_issues Logical, whether to return the linting issues as a list
#' @param fix_mode Logical, whether to provide interactive fixing suggestions
#' @return If return_issues is TRUE, returns a list of linting issues, otherwise invisible(TRUE/FALSE)
lint_check <- function(show_progress = TRUE, return_issues = FALSE, fix_mode = FALSE) {
  if (show_progress) {
    cat("🔍 Running comprehensive linting check...\n")
  }
  
  # Run linting
  lints <- lintr::lint_package()
  
  # Count issues by type
  if (length(lints) == 0) {
    if (show_progress) {
      cat("✅ No linting issues found! Package is clean.\n")
    }
    
    if (return_issues) {
      return(list())
    } else {
      return(invisible(TRUE))
    }
  }
  
  # Analyze linting results
  issue_summary <- .analyze_linting_issues(lints)
  
  if (show_progress) {
    .display_linting_summary(issue_summary, lints)
  }
  
  # Interactive fixing mode
  if (fix_mode) {
    .interactive_lint_fixing(lints, issue_summary)
  }
  
  if (return_issues) {
    return(lints)
  } else {
    return(invisible(FALSE))
  }
}

#' Run linting check for specific files
#' @param files Character vector of file paths to lint
#' @param show_progress Logical, whether to show progress information
#' @return Invisible TRUE if no issues, FALSE if issues found
lint_files <- function(files, show_progress = TRUE) {
  if (show_progress) {
    cat("🔍 Linting", length(files), "file(s)...\n")
  }
  
  # Validate files exist
  missing_files <- files[!file.exists(files)]
  if (length(missing_files) > 0) {
    cat("❌ File(s) not found:", paste(missing_files, collapse = ", "), "\n")
    return(invisible(FALSE))
  }
  
  # Run linting on specific files
  all_lints <- list()
  for (file in files) {
    file_lints <- lintr::lint(file)
    if (length(file_lints) > 0) {
      all_lints[[file]] <- file_lints
    }
  }
  
  total_issues <- sum(sapply(all_lints, length))
  
  if (total_issues == 0) {
    if (show_progress) {
      cat("✅ No linting issues found in specified files!\n")
    }
    return(invisible(TRUE))
  }
  
  if (show_progress) {
    cat("⚠️  Found", total_issues, "linting issue(s) in", length(all_lints), "file(s)\n")
    
    for (file in names(all_lints)) {
      cat("  📄", basename(file), ":", length(all_lints[[file]]), "issue(s)\n")
    }
  }
  
  invisible(FALSE)
}

#' Interactive linting fixing workflow
#' @return TRUE if ready to commit, FALSE if issues need fixing
lint_interactive <- function() {
  cat("🔧 Interactive Linting Workflow\n\n")
  
  # Run linting check
  cat("Running linting checks...\n")
  lints <- lintr::lint_package()
  
  if (length(lints) == 0) {
    cat("✅ No linting issues found! Ready to commit.\n")
    return(TRUE)
  }
  
  # Analyze and display issues
  issue_summary <- .analyze_linting_issues(lints)
  .display_linting_summary(issue_summary, lints)
  .interactive_lint_fixing(lints, issue_summary)
  
  return(FALSE)
}

#' Pre-commit development workflow
#' @param auto_style Logical, whether to run styler::style_pkg() first
#' @return TRUE if ready to commit, FALSE if issues remain
precommit_check <- function(auto_style = FALSE) {
  cat("🚀 Pre-commit Development Check\n\n")
  
  # Step 1: Auto-styling (optional)
  if (auto_style) {
    cat("🎨 Running automatic styling...\n")
    tryCatch({
      styler::style_pkg()
      cat("✅ Automatic styling completed\n")
    }, error = function(e) {
      cat("⚠️  Styling failed:", e$message, "\n")
    })
  }
  
  # Step 2: Run linting
  linting_ok <- lint_and_check()
  
  # Step 3: Run tests
  tests_ok <- run_all_tests()
  
  if (linting_ok && tests_ok) {
    cat("\n🎉 All checks passed! Ready to commit.\n")
    return(TRUE)
  } else {
    cat("\n❌ Some checks failed. Fix issues before committing.\n")
    return(FALSE)
  }
}

# Internal helper functions for enhanced linting

#' Analyze linting issues and categorize them
#' @param lints List of linting issues from lintr
#' @return List with issue analysis
.analyze_linting_issues <- function(lints) {
  if (length(lints) == 0) {
    return(list(
      total = 0,
      by_type = list(),
      by_file = list(),
      fixable = 0
    ))
  }
  
  # Extract issue information
  lint_data <- data.frame(
    file = sapply(lints, function(x) x$filename),
    line = sapply(lints, function(x) x$line_number),
    type = sapply(lints, function(x) x$type),
    linter = sapply(lints, function(x) class(x$linter)[1]),
    message = sapply(lints, function(x) x$message),
    stringsAsFactors = FALSE
  )
  
  # Count by type
  by_type <- table(lint_data$type)
  
  # Count by file
  by_file <- table(basename(lint_data$file))
  
  # Identify potentially auto-fixable issues
  auto_fixable_linters <- c(
    "trailing_whitespace_linter",
    "trailing_blank_lines_linter"
  )
  
  fixable_count <- sum(lint_data$linter %in% auto_fixable_linters)
  
  list(
    total = length(lints),
    by_type = as.list(by_type),
    by_file = as.list(by_file),
    fixable = fixable_count,
    detail = lint_data
  )
}

#' Display a summary of linting issues
#' @param issue_summary List from .analyze_linting_issues
#' @param lints Original linting issues
.display_linting_summary <- function(issue_summary, lints) {
  cat("⚠️  Found", issue_summary$total, "linting issue(s)\n")
  
  # Show breakdown by type
  if (length(issue_summary$by_type) > 0) {
    cat("\n📊 Issues by type:\n")
    for (type in names(issue_summary$by_type)) {
      cat("  ", type, ":", issue_summary$by_type[[type]], "\n")
    }
  }
  
  # Show breakdown by file
  if (length(issue_summary$by_file) > 0) {
    cat("\n📁 Issues by file:\n")
    # Show top 10 files with most issues
    file_counts <- sort(unlist(issue_summary$by_file), decreasing = TRUE)
    top_files <- head(file_counts, 10)
    
    for (file in names(top_files)) {
      cat("  ", file, ":", top_files[[file]], "\n")
    }
    
    if (length(file_counts) > 10) {
      remaining <- length(file_counts) - 10
      cat("  ... and", remaining, "more file(s)\n")
    }
  }
  
  # Show auto-fixable count
  if (issue_summary$fixable > 0) {
    cat("\n🔧", issue_summary$fixable, "issue(s) may be auto-fixable\n")
  }
}

#' Interactive linting fixing workflow
#' @param lints List of linting issues
#' @param issue_summary Analysis from .analyze_linting_issues
.interactive_lint_fixing <- function(lints, issue_summary) {
  cat("\n🔧 Interactive Fixing Mode\n")
  
  # Group issues by file for easier fixing
  files_with_issues <- unique(sapply(lints, function(x) x$filename))
  
  cat("\nFiles with linting issues:\n")
  for (i in seq_along(files_with_issues)) {
    file_issues <- sum(sapply(lints, function(x) x$filename == files_with_issues[i]))
    cat(i, ". ", basename(files_with_issues[i]), " (", file_issues, " issues)\n", sep = "")
  }
  
  # Suggest fixing strategies
  cat("\n💡 Recommended fixing order:\n")
  cat("1. Fix trailing whitespace: run styler::style_pkg() or manually remove spaces\n")
  cat("2. Fix return statements: remove explicit return() where implicit return works\n")
  cat("3. Fix indentation: use consistent spacing (2 spaces for R)\n")
  cat("4. Fix line length: break long lines at 120 characters\n")
  cat("5. Fix other style issues: follow existing code patterns\n")
  
  cat("\n💡 Use lint_files() to check progress on specific files\n")
}
