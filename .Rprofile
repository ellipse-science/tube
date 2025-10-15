# tube package development .Rprofile
# This file is loaded when R starts in the tube project directory

# Helper function for repeated characters
`%R%` <- function(x, n) paste(rep(x, n), collapse = "")

# Display startup message
if (interactive()) {
  cat("\n🚰 tube Package Development Environment\n")
  cat("=" %R% 40, "\n")
  cat("Quick commands:\n")
  cat("• setup_env()     - Run development setup\n")
  cat("• lint_check()    - Run linting only\n")
  cat("• full_qa()       - Run full QA pipeline\n")
  cat("• help_dev()      - Show development help\n")
  cat("\nFor full setup: Rscript setup-dev-environment.R\n")
  cat("=" %R% 40, "\n\n")
}

# Development helper functions
setup_env <- function() {
  cat("Running development environment setup...\n")
  source("setup-dev-environment.R")
}

lint_check <- function() {
  cat("Running linting check...\n")
  system2("Rscript", args = c("tools/qa-pipeline.R", "--lint-only"))
}

full_qa <- function() {
  cat("Running full QA pipeline...\n")
  system2("Rscript", args = "tools/qa-pipeline.R")
}

help_dev <- function() {
  cat("\n🛠️ tube Development Commands:\n")
  cat("=" %R% 30, "\n")
  cat("setup_env()     - Run setup-dev-environment.R\n")
  cat("lint_check()    - Quick linting check\n")
  cat("full_qa()       - Full QA pipeline\n")
  cat("\n📂 Key Files:\n")
  cat("• setup-dev-environment.R  - Environment setup\n")
  cat("• CONTRIBUTING.md          - Contributor guidelines\n")
  cat("• tools/qa-pipeline.R      - QA orchestration\n")
  cat("• .lintr                   - Linting configuration\n")
  cat("\n📖 Documentation:\n")
  cat("• README.md                - Project overview\n")
  cat("• DEVELOPMENT-CHECKLIST.md - Development checklist\n")
  cat("=" %R% 30, "\n\n")
}

# Load commonly used development packages silently
suppressMessages({
  if (requireNamespace("devtools", quietly = TRUE)) {
    library(devtools, quietly = TRUE)
  }
  if (requireNamespace("usethis", quietly = TRUE)) {
    library(usethis, quietly = TRUE)
  }
})

# Set options for development
options(
  # Better error handling
  error = utils::recover,
  
  # Wider console output
  width = 120,
  
  # More descriptive warnings
  warn = 1,
  
  # Keep source for debugging
  keep.source = TRUE,
  keep.source.pkgs = TRUE
)
