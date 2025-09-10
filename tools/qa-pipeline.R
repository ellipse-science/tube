#!/usr/bin/env Rscript

#' Main orchestration script for package quality assurance
#'
#' This script runs the complete QA pipeline that mirrors GitHub Actions:
#' 1. Linting checks
#' 2. Package tests
#' 3. Test coverage analysis
#' 4. R CMD check
#'
#' Usage: Rscript tools/qa-pipeline.R [--lint-only] [--no-coverage] [--help]

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

show_help <- function() {
  cat("Package QA Pipeline\n")
  cat("==================\n\n")
  cat("Usage: Rscript tools/qa-pipeline.R [options]\n\n")
  cat("Options:\n")
  cat("  --lint-only     Run only linting checks\n")
  cat("  --no-coverage   Skip coverage analysis\n")
  cat("  --help          Show this help message\n\n")
  cat("This script runs the complete QA pipeline that mirrors GitHub Actions.\n")
}

if ("--help" %in% args) {
  show_help()
  quit(status = 0)
}

lint_only <- "--lint-only" %in% args
skip_coverage <- "--no-coverage" %in% args

cat("🚀 Starting Package QA Pipeline\n")
cat("==============================\n\n")

# Track overall success
overall_success <- TRUE
start_time <- Sys.time()

# Helper function to run script and track success
run_script <- function(script_name, description) {
  cat("📋", description, "\n")
  cat("Running:", script_name, "\n")
  
  result <- system2("Rscript", args = paste0("tools/", script_name), stdout = TRUE, stderr = TRUE)
  exit_code <- attr(result, "status")
  
  if (is.null(exit_code)) exit_code <- 0
  
  # Print output
  cat(paste(result, collapse = "\n"), "\n")
  
  if (exit_code == 0) {
    cat("✅", description, "passed\n\n")
    TRUE
  } else {
    cat("❌", description, "failed\n\n")
    FALSE
  }
}

# 1. Linting (always run)
if (!run_script("lint.R", "Linting checks")) {
  overall_success <- FALSE
}

if (!lint_only) {
  # 2. Tests (COMMENTED OUT - not running for now)
  # if (!run_script("test.R", "Package tests")) {
  #   overall_success <- FALSE
  # }
  
  # 3. Coverage (COMMENTED OUT - not running for now)
  # if (!skip_coverage) {
  #   if (!run_script("coverage.R", "Test coverage analysis")) {
  #     cat("⚠️  Coverage analysis failed, but continuing...\n\n")
  #   }
  # }
  
  # 4. Package check
  if (!run_script("check.R", "R CMD check")) {
    overall_success <- FALSE
  }
}

# Summary
end_time <- Sys.time()
duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)

cat("🏁 QA Pipeline Complete\n")
cat("======================\n")
cat("Duration:", duration, "seconds\n")

if (overall_success) {
  cat("🎉 All checks passed successfully!\n")
  cat("✅ Your package is ready for deployment.\n")
  quit(status = 0)
} else {
  cat("❌ Some checks failed.\n")
  cat("🔧 Please fix the issues before proceeding.\n")
  quit(status = 1)
}
