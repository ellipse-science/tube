#!/usr/bin/env Rscript

cat("=== CURRENT LINTING STATUS ===\n")

# Check current total violations
lints <- lintr::lint_package()
cat("Total violations:", length(lints), "\n")

if (length(lints) > 0) {
  # Group by file
  files <- sapply(lints, function(x) x$filename)
  file_counts <- table(files)
  cat("\nViolations by file:\n")
  for (i in seq_along(file_counts)) {
    cat("  ", names(file_counts)[i], ":", file_counts[i], "\n")
  }
  
  # Show most problematic files
  sorted_counts <- sort(file_counts, decreasing = TRUE)
  top_files <- head(sorted_counts, 5)
  cat("\nTop 5 most problematic files:\n")
  for (i in seq_along(top_files)) {
    cat("  ", names(top_files)[i], ":", top_files[i], "violations\n")
  }
}

cat("\n=== DONE ===\n")
    lints <- lintr::lint(file_path)
    cat(file, "violations:", length(lints), "\n")
  }
}

cat("\n=== TOTAL PACKAGE VIOLATIONS ===\n")
total_lints <- lintr::lint_package()
cat("Total package violations:", length(total_lints), "\n")
if (file.exists('.Renviron')) {
  readRenviron('.Renviron')
}

cat("Running lintr::lint_package()...\n")
lints <- lintr::lint_package()

cat("Number of lints found:", length(lints), "\n")

if (length(lints) > 0) {
  cat("\n❌ LINTING ISSUES FOUND:\n")
  for (i in seq_along(lints)) {
    lint_item <- lints[[i]]
    cat(sprintf("%d. %s:%d:%d - %s: %s\n", 
                i, 
                lint_item$filename %||% "unknown",
                lint_item$line_number %||% 0,
                lint_item$column_number %||% 0,
                lint_item$type %||% "unknown",
                lint_item$message %||% "no message"))
  }
  cat("\n")
} else {
  cat("✅ NO LINTING ISSUES FOUND\n")
}

cat("=== LINTING CHECK COMPLETE ===\n")