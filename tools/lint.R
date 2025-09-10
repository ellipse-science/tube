#!/usr/bin/env Rscript

#' Lint the R package code
#'
#' Runs comprehensive linting checks on the package using the project's .lintr configuration.
#' This script mirrors the linting checks performed in GitHub Actions.

cat("=== Package Linting Check ===\n")

# Check if .lintr file exists
if (!file.exists(".lintr")) {
  cat("❌ .lintr configuration file not found\n")
  quit(status = 1)
}

cat("📋 Running lintr::lint_package()...\n")

# Run linting
tryCatch({
  lints <- lintr::lint_package()
  
  # Also lint tools directory
  cat("📋 Running linting on tools/ directory...\n")
  tools_files <- list.files("tools", pattern = "\\.R$", full.names = TRUE)
  for (file in tools_files) {
    tools_lints <- lintr::lint(file)
    lints <- c(lints, tools_lints)
  }
  
  if (length(lints) > 0) {
    cat("❌ Linting failed! Found", length(lints), "issues:\n\n")
    print(lints)
    cat("\n")
    quit(status = 1)
  } else {
    cat("✅ All linting checks passed!\n")
    quit(status = 0)
  }
}, error = function(e) {
  cat("❌ Error running lintr:\n")
  cat(e$message, "\n")
  quit(status = 1)
})
