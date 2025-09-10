#!/usr/bin/env Rscript

#' Detailed contributor setup script for tools directory
#' 
#' This script provides advanced setup options and validation
#' for contributors working on the QA pipeline and tools.

cat("🔧 Advanced Contributor Setup for tube Package\n")
cat("=============================================\n\n")

# Function to check if command is available
command_exists <- function(cmd) {
  result <- suppressWarnings(system2("which", cmd, stdout = FALSE, stderr = FALSE))
  return(result == 0)
}

# Advanced package recommendations
advanced_packages <- c(
  "remotes",       # Install from GitHub
  "pkgdown",       # Documentation websites  
  "goodpractice",  # Package quality checks
  "spelling",      # Spell checking
  "urlchecker"     # Check URLs in documentation
)

cat("📦 Installing advanced development packages...\n")
for (pkg in advanced_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("  Installing", pkg, "...\n")
    install.packages(pkg, repos = "https://cran.r-project.org")
  } else {
    cat("  ✅", pkg, "already available\n")
  }
}

cat("\n🔍 Validating development environment...\n")

# Check R version
r_version <- getRversion()
cat("  R version:", as.character(r_version))
if (r_version >= "4.0.0") {
  cat(" ✅\n")
} else {
  cat(" ⚠️ (R 4.0+ recommended)\n")
}

# Check Git availability
if (command_exists("git")) {
  cat("  Git: Available ✅\n")
  
  # Check git config
  tryCatch({
    user_name <- system2("git", c("config", "user.name"), stdout = TRUE, stderr = FALSE)
    user_email <- system2("git", c("config", "user.email"), stdout = TRUE, stderr = FALSE)
    
    if (length(user_name) > 0 && length(user_email) > 0) {
      cat("  Git config: Configured ✅\n")
    } else {
      cat("  Git config: ⚠️ Please set user.name and user.email\n")
    }
  }, error = function(e) {
    cat("  Git config: ⚠️ Could not check configuration\n")
  })
} else {
  cat("  Git: ❌ Not available\n")
}

cat("\n🛠️ Setting up IDE recommendations...\n")

# Create .vscode settings if VS Code is being used
if (dir.exists(".vscode") || Sys.getenv("TERM_PROGRAM") == "vscode") {
  cat("  VS Code detected - creating recommended settings...\n")
  
  if (!dir.exists(".vscode")) {
    dir.create(".vscode")
  }
  
  # VS Code settings for R development
  vscode_settings <- list(
    "r.rterm.windows" = "C:\\\\Program Files\\\\R\\\\R-4.5.0\\\\bin\\\\x64\\\\R.exe",
    "r.rterm.mac" = "/usr/local/bin/R",
    "r.rterm.linux" = "/usr/bin/R",
    "r.lsp.enabled" = TRUE,
    "r.lsp.debug" = FALSE,
    "r.alwaysUseActiveTerminal" = TRUE,
    "files.associations" = list(
      "*.R" = "r",
      "*.Rmd" = "rmd"
    ),
    "editor.rulers" = list(120),
    "editor.wordWrap" = "wordWrapColumn",
    "editor.wordWrapColumn" = 120
  )
  
  tryCatch({
    jsonlite::write_json(vscode_settings, ".vscode/settings.json", 
                        pretty = TRUE, auto_unbox = TRUE)
    cat("  ✅ VS Code settings created\n")
  }, error = function(e) {
    cat("  ⚠️ Could not create VS Code settings\n")
  })
}

cat("\n🎯 Advanced QA validation...\n")

# Test all individual tools
tools_to_test <- c("lint.R", "check.R")
for (tool in tools_to_test) {
  tool_path <- file.path("tools", tool)
  if (file.exists(tool_path)) {
    cat("  Testing", tool, "...\n")
    tryCatch({
      result <- system2("Rscript", args = tool_path, 
                       stdout = TRUE, stderr = TRUE, timeout = 30)
      
      status <- attr(result, "status")
      if (is.null(status) || status == 0) {
        cat("    ✅", tool, "works correctly\n")
      } else {
        cat("    ⚠️", tool, "completed with warnings\n")
      }
    }, error = function(e) {
      cat("    ❌", tool, "failed:", e$message, "\n")
    })
  }
}

cat("\n📋 Creating development checklists...\n")

# Create a local development checklist
checklist_content <- "# tube Package Development Checklist

## Before Starting Work
- [ ] Run `Rscript setup-dev-environment.R`
- [ ] Pull latest changes: `git pull`
- [ ] Create feature branch: `git checkout -b feature/your-feature`

## During Development  
- [ ] Follow naming conventions (snake_case)
- [ ] Keep lines under 120 characters
- [ ] Add roxygen documentation for new functions
- [ ] Use French text with proper encoding
- [ ] Run `Rscript tools/qa-pipeline.R --lint-only` frequently

## Before Committing
- [ ] All linting checks pass
- [ ] Package validation successful
- [ ] Documentation complete
- [ ] No hardcoded sensitive values
- [ ] Test your changes manually

## Pull Request
- [ ] Descriptive branch name
- [ ] Clear commit messages
- [ ] Complete PR description
- [ ] All CI checks pass
"

writeLines(checklist_content, "DEVELOPMENT-CHECKLIST.md")
cat("  ✅ Development checklist created (DEVELOPMENT-CHECKLIST.md)\n")

cat("\n🔗 Git hooks setup (optional)...\n")

# Offer to set up git hooks
cat("  Would you like to set up pre-commit hooks for automatic linting? (y/N): ")
if (interactive()) {
  response <- readline()
  if (tolower(trimws(response)) %in% c("y", "yes")) {
    
    if (!dir.exists(".git/hooks")) {
      dir.create(".git/hooks", recursive = TRUE)
    }
    
    pre_commit_hook <- "#!/bin/bash
# Pre-commit hook for tube package
echo \"Running pre-commit linting check...\"
Rscript tools/qa-pipeline.R --lint-only
if [ $? -ne 0 ]; then
    echo \"❌ Linting failed! Please fix issues before committing.\"
    exit 1
fi
echo \"✅ Linting passed!\"
"
    
    writeLines(pre_commit_hook, ".git/hooks/pre-commit")
    Sys.chmod(".git/hooks/pre-commit", mode = "0755")
    cat("  ✅ Pre-commit hook installed\n")
  } else {
    cat("  Pre-commit hooks skipped\n")
  }
} else {
  cat("  (Run interactively to set up git hooks)\n")
}

cat("\n📚 Documentation setup...\n")

# Check if pkgdown can build docs
if (requireNamespace("pkgdown", quietly = TRUE)) {
  cat("  pkgdown available for documentation building ✅\n")
  cat("  Run 'pkgdown::build_site()' to build local documentation\n")
}

cat("\n🎉 Advanced setup complete!\n")
cat("=" %R% 30, "\n")

cat("\n💡 Pro tips:\n")
cat("  • Use 'usethis::use_*' functions for common R package tasks\n")
cat("  • Check 'goodpractice::gp()' for package quality recommendations\n")
cat("  • Use 'devtools::load_all()' to test changes without installing\n")
cat("  • Run 'spelling::spell_check_package()' to check documentation spelling\n")

# Helper function for repeated characters
`%R%` <- function(x, n) paste(rep(x, n), collapse = "")
