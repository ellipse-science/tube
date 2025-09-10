#!/usr/bin/env Rscript

#' Development Environment Setup for tube Package Contributors
#'
#' This script sets up the complete development environment for contributing
#' to the tube package, including linting rules, QA tools, and dependencies.
#'
#' Usage: Rscript setup-dev-environment.R

cat("🚀 Setting up tube package development environment\n")
cat("=" %R% 55, "\n\n")

# Helper function for consistent output
`%R%` <- function(x, n) paste(rep(x, n), collapse = "")

# Required packages for development
required_packages <- c(
  "devtools",      # Package development
  "lintr",         # Code linting
  "rcmdcheck",     # Package checking
  "testthat",      # Testing framework (future use)
  "covr",          # Coverage analysis (future use)
  "styler",        # Code styling
  "roxygen2",      # Documentation
  "usethis",       # Development utilities
  "cli"            # Better console output
)

cat("📦 Installing required development packages...\n")

# Install missing packages
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("  Installing", pkg, "...\n")
    install.packages(pkg, repos = "https://cran.r-project.org")
  } else {
    cat("  ✅", pkg, "already installed\n")
  }
}

cat("\n🔍 Validating lintr configuration...\n")

# Check if .lintr file exists and is properly configured
if (file.exists(".lintr")) {
  cat("  ✅ .lintr configuration file found\n")
  
  # Test lintr configuration
  tryCatch({
    if (requireNamespace("lintr", quietly = TRUE)) {
      # Quick test on a simple file to validate config
      temp_test <- tempfile(fileext = ".R")
      writeLines("# Test file\ntest_function <- function() {\n  return(TRUE)\n}", temp_test)
      
      lints <- lintr::lint(temp_test)
      unlink(temp_test)
      
      cat("  ✅ Linting configuration is working\n")
    }
  }, error = function(e) {
    cat("  ⚠️ Issue with lintr configuration:", e$message, "\n")
  })
} else {
  cat("  ❌ .lintr file not found\n")
}

cat("\n🛠️ Validating QA tools...\n")

# Check if tools directory exists
if (dir.exists("tools")) {
  cat("  ✅ tools/ directory found\n")
  
  # Check for required tool scripts
  required_tools <- c("qa-pipeline.R", "lint.R", "check.R", "test.R", "coverage.R")
  for (tool in required_tools) {
    tool_path <- file.path("tools", tool)
    if (file.exists(tool_path)) {
      cat("  ✅", tool, "found\n")
    } else {
      cat("  ❌", tool, "missing\n")
    }
  }
  
  # Test QA pipeline
  cat("\n🧪 Testing QA pipeline (lint only)...\n")
  tryCatch({
    if (file.exists("tools/qa-pipeline.R")) {
      # Run lint-only test
      result <- system2("Rscript",
        args = c("tools/qa-pipeline.R", "--lint-only"),
        stdout = TRUE, stderr = TRUE)
      
      if (attr(result, "status") == 0 || is.null(attr(result, "status"))) {
        cat("  ✅ QA pipeline test passed\n")
      } else {
        cat("  ⚠️ QA pipeline test had issues\n")
        cat("  Result:", paste(result, collapse = "\n"), "\n")
      }
    }
  }, error = function(e) {
    cat("  ⚠️ Could not test QA pipeline:", e$message, "\n")
  })
} else {
  cat("  ❌ tools/ directory not found\n")
}

cat("\n📝 Environment setup recommendations:\n")
cat("  • Use .Renviron file for environment variables\n")
cat("  • Install IDE extensions for R development:\n")
cat("    - VS Code: R Extension\n")
cat("    - RStudio: Built-in R support\n")
cat("  • Consider setting up git hooks for pre-commit linting\n")

cat("\n🎯 Quick development workflow:\n")
cat("  1. Make code changes\n")
cat("  2. Run: Rscript tools/qa-pipeline.R --lint-only\n")
cat("  3. Fix any linting issues\n")
cat("  4. Run: Rscript tools/qa-pipeline.R (full check)\n")
cat("  5. Commit and push changes\n")

cat("\n📖 Next steps:\n")
cat("  • Read CONTRIBUTING.md for detailed guidelines\n")
cat("  • Check README.md for project overview\n")
cat("  • Run 'Rscript tools/qa-pipeline.R --lint-only' to test setup\n")

cat("\n🎉 Development environment setup complete!\n")
cat("=" %R% 45, "\n")

# Create a simple test to verify everything works
cat("\n✅ Running verification test...\n")
if (requireNamespace("lintr", quietly = TRUE) &&
    requireNamespace("devtools", quietly = TRUE) &&
    file.exists(".lintr") &&
    dir.exists("tools")) {
  cat("✅ All core components verified successfully!\n")
  cat("🚀 You're ready to contribute to the tube package!\n")
} else {
  cat("⚠️ Some components may need attention. Check the output above.\n")
}
