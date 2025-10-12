#!/usr/bin/env Rscript

# Simple retry script for failed S3 objects
suppressPackageStartupMessages({
  library(optparse)
})

# Parse timestamp from S3 key
parse_key_time <- function(key) {
  m <- regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}Z", key)
  if (m == -1) return(NA_character_)
  substr(key, m, m + attr(m, "match.length") - 1)
}

# Extract partition path from S3 key
extract_partition_path <- function(key) {
  parts <- strsplit(key, "/")[[1]]
  if (length(parts) >= 2) {
    return(paste0(paste(parts[1:2], collapse = "/"), "/"))
  }
  return("r-media-headlines/FXN/")  # fallback
}

# Option parsing
option_list <- list(
  make_option("--environment", type = "character", default = NULL, help = "PROD or DEV"),
  make_option("--failed_files_list", type = "character", default = "failed_files.txt", help = "File containing list of failed S3 keys")
)
opt <- parse_args(OptionParser(option_list = option_list))

if (is.null(opt$environment)) {
  stop("Missing required argument: environment", call. = FALSE)
}

if (!file.exists(opt$failed_files_list)) {
  stop(paste("Failed files list not found:", opt$failed_files_list), call. = FALSE)
}

# Read failed files list
failed_keys <- readLines(opt$failed_files_list)
failed_keys <- failed_keys[nzchar(failed_keys)]

cat(sprintf("Found %d failed files to retry\n", length(failed_keys)))

successful <- 0
failed_count <- 0

# Process each failed key
for (i in seq_along(failed_keys)) {
  key <- failed_keys[i]
  cat(sprintf("\n[%d/%d] Processing: %s\n", i, length(failed_keys), substring(key, 1, 80)))
  
  # Extract timestamp and partition path
  timestamp_str <- parse_key_time(key)
  partition_path <- extract_partition_path(key)
  
  if (is.na(timestamp_str)) {
    cat("  ❌ ERROR: Could not parse timestamp\n")
    failed_count <- failed_count + 1
    next
  }
  
  # Create very narrow time window (±1 second to target just this specific file)
  base_time <- as.POSIXct(sub("Z$", "", timestamp_str), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  start_time <- base_time - 1
  end_time <- base_time + 1
  
  start_timestamp <- format(start_time, "%Y-%m-%dT%H:%M:%SZ")
  end_timestamp <- format(end_time, "%Y-%m-%dT%H:%M:%SZ")
  
  cat(sprintf("  Time window: %s to %s\n", start_timestamp, end_timestamp))
  
  # Build and run command
  cmd <- sprintf("Rscript tools/s3_object_reprocessor.R --environment %s --partition_path \"%s\" --start_timestamp \"%s\" --end_timestamp \"%s\" --log_file \"temp_retry_%d.csv\" 2>/dev/null",
                 opt$environment, partition_path, start_timestamp, end_timestamp, i)
  
  exit_code <- system(cmd, ignore.stdout = TRUE, ignore.stderr = FALSE)
  
  if (exit_code == 0) {
    cat("  ✅ SUCCESS\n")
    successful <- successful + 1
    
    # Clean up temp file
    temp_file <- sprintf("temp_retry_%d.csv", i)
    if (file.exists(temp_file)) file.remove(temp_file)
  } else {
    cat(sprintf("  ❌ FAILED (exit code %d)\n", exit_code))
    failed_count <- failed_count + 1
  }
  
  Sys.sleep(1)  # Brief pause between retries
}

# Summary
cat(sprintf("\n=== FINAL SUMMARY ===\n"))
cat(sprintf("Total objects: %d\n", length(failed_keys)))
cat(sprintf("Successful: %d\n", successful))
cat(sprintf("Failed: %d\n", failed_count))
cat(sprintf("Success rate: %.1f%%\n", (successful / length(failed_keys)) * 100))
