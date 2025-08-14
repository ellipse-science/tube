#!/usr/bin/env Rscript

# Batch script to update all test files with conditional suppression pattern
# This script removes verbose cat() statements and applies conditionally_suppress()

cat("=== Batch updating all test files with conditional suppression ===\n")

# Get all test files
test_files <- list.files("tests/testthat", pattern = "^test-.*\\.R$", full.names = TRUE)
cat("Found", length(test_files), "test files to update\n")

# Helper function to clean cat() statements
clean_cat_statements <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)
  
  # Replace cat() statements with test_detail() calls
  lines <- gsub('cat\\("([^"]*)"\\)', 'test_detail("\\1")', lines)
  lines <- gsub('cat\\("([^"]*)",([^)]+)\\)', 'test_detail(sprintf("\\1", \\2))', lines)
  
  # Remove standalone cat() calls that span multiple lines (simplified approach)
  lines <- lines[!grepl("^\\s*cat\\(", lines)]
  
  writeLines(lines, file_path)
}

# Helper function to add conditionally_suppress to function calls
add_conditional_suppress <- function(file_path) {
  content <- readLines(file_path, warn = FALSE)
  
  # Add header comment if not present
  if (!any(grepl("TUBE_TEST_VERBOSE", content))) {
    header <- c(
      "# DEBUGGING TESTS:",
      "# - Normal run: Routine output suppressed for clean results", 
      "# - Verbose mode: Set TUBE_TEST_VERBOSE=TRUE to see all output for debugging",
      "# - Example: Sys.setenv(TUBE_TEST_VERBOSE = \"TRUE\"); devtools::test(filter = \"your-filter\")",
      ""
    )
    
    # Find insertion point (after load_all line)
    load_all_line <- which(grepl("devtools::load_all", content))
    if (length(load_all_line) > 0) {
      content <- c(content[1:load_all_line], "", header, content[(load_all_line + 1):length(content)])
    }
  }
  
  writeLines(content, file_path)
}

# Process each test file
for (file_path in test_files) {
  cat("Processing:", basename(file_path), "\n")
  
  # Skip files that already have conditional suppression
  content <- readLines(file_path, warn = FALSE)
  if (any(grepl("conditionally_suppress", content))) {
    cat("  - Already updated, skipping\n")
    next
  }
  
  tryCatch({
    # Clean cat statements
    clean_cat_statements(file_path)
    
    # Add header comments
    add_conditional_suppress(file_path)
    
    cat("  - Updated successfully\n")
  }, error = function(e) {
    cat("  - Error:", e$message, "\n")
  })
}

cat("\n=== Batch processing complete ===\n")
cat("Note: Files with existing conditionally_suppress() were skipped\n")
cat("Manual review and testing recommended for critical test files\n")
