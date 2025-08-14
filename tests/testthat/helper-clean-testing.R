# Clean Testing Infrastructure
# Provides utilities for clean, focused test output while maintaining
# the ability to enable detailed debugging when needed.

#' Debug logging function - only outputs when debug mode is enabled
#' @param message The message to log
#' @param level Log level (INFO, WARN, ERROR)
debug_log <- function(message, level = "INFO") {
  if (Sys.getenv("TUBE_TEST_DEBUG", "FALSE") == "TRUE") {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    cat(sprintf("[%s] %s: %s\n", timestamp, level, message))
  }
}

#' Setup test logging infrastructure
#' Creates log directory and sets up file logging if enabled
setup_test_logging <- function() {
  if (Sys.getenv("TUBE_TEST_SAVE_LOGS", "FALSE") == "TRUE") {
    log_dir <- "tests/logs"
    dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
    
    log_file <- file.path(log_dir, sprintf("test-run-%s.log", 
                         format(Sys.time(), "%Y%m%d-%H%M%S")))
    
    # Setup file logging
    debug_log(sprintf("Detailed logs will be saved to: %s", log_file))
    
    # Return log file path for potential use
    return(log_file)
  }
  return(NULL)
}

#' Conditional detailed output for complex operations
#' Use this instead of cat() for test details that are useful for debugging
#' but shouldn't clutter normal test runs
test_detail <- function(message) {
  if (Sys.getenv("TUBE_TEST_VERBOSE", "FALSE") == "TRUE") {
    cat("  ", message, "\n")
  }
}

#' Helper to create temporary test files with cleanup
#' @param content Data frame or content to write
#' @param ext File extension (default: "csv")
#' @param cleanup_on_exit Whether to clean up automatically (default: TRUE)
create_temp_test_file <- function(content, ext = "csv", cleanup_on_exit = TRUE) {
  temp_file <- tempfile(fileext = paste0(".", ext))
  
  if (is.data.frame(content)) {
    if (ext == "csv") {
      write.csv(content, temp_file, row.names = FALSE)
    } else if (ext == "rds") {
      saveRDS(content, temp_file)
    }
  } else {
    writeLines(as.character(content), temp_file)
  }
  
  if (cleanup_on_exit) {
    # Register cleanup
    do.call(on.exit, list(substitute(unlink(temp_file)), add = TRUE), envir = parent.frame())
  }
  
  return(temp_file)
}

#' Helper to create temporary test directory with files
create_temp_test_dir <- function(files_list, cleanup_on_exit = TRUE) {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  
  for (filename in names(files_list)) {
    file_path <- file.path(temp_dir, filename)
    content <- files_list[[filename]]
    
    if (is.data.frame(content)) {
      write.csv(content, file_path, row.names = FALSE)
    } else {
      writeLines(as.character(content), file_path)
    }
  }
  
  if (cleanup_on_exit) {
    do.call(on.exit, list(substitute(unlink(temp_dir, recursive = TRUE)), add = TRUE), envir = parent.frame())
  }
  
  return(temp_dir)
}

# Initialize logging on load
if (interactive()) {
  setup_test_logging()
}
