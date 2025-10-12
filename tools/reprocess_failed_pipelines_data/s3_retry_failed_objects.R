#!/usr/bin/env Rscript

# s3_retry_failed_objects.R
# Script to retry copying specific S3 objects that previously failed
# Usage: Rscript tools/s3_retry_failed_objects.R --environment PROD --failed_files_list failed_files.txt

suppressPackageStartupMessages({
  library(optparse)
  library(data.table)
})

# Parse timestamp from S3 key
parse_key_time <- function(key) {
  if (is.null(key) || length(key) == 0 || is.na(key)) return(NA_character_)
  m <- regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}Z", key)
  if (length(m) == 1 && m == -1) return(NA_character_)
  substr(key, m, m + attr(m, "match.length") - 1)
}

# Extract partition path from S3 key
extract_partition_path <- function(key) {
  # Extract everything up to the timestamp part
  # e.g., "r-media-headlines/FXN/2025-09-02T05:37:14.854Z-..." -> "r-media-headlines/FXN/"
  parts <- strsplit(key, "/")[[1]]
  if (length(parts) >= 2) {
    # Take first two parts and add trailing slash
    return(paste0(paste(parts[1:2], collapse = "/"), "/"))
  }
  return(key)  # fallback
}

# Option parsing
option_list <- list(
  make_option("--environment", type = "character", default = NULL, help = "PROD or DEV"),
  make_option("--failed_files_list", type = "character", default = "failed_files.txt", help = "File containing list of failed S3 keys"),
  make_option("--log_file", type = "character", default = "s3_retry_log.csv", help = "Log file"),
  make_option("--dry_run", action = "store_true", default = FALSE, help = "Preview actions only")
)
opt <- parse_args(OptionParser(option_list = option_list))

# Validate required options
if (is.null(opt$environment)) {
  stop("Missing required argument: environment", call. = FALSE)
}

if (!file.exists(opt$failed_files_list)) {
  stop(paste("Failed files list not found:", opt$failed_files_list), call. = FALSE)
}

# Read failed files list
failed_keys <- readLines(opt$failed_files_list)
failed_keys <- failed_keys[nzchar(failed_keys)]  # Remove empty lines

if (length(failed_keys) == 0) {
  stop("No failed files found in the list", call. = FALSE)
}

cat(sprintf("Found %d failed files to retry\n", length(failed_keys)))

# Initialize results tracking
results <- data.table(key = character(0), status = character(0), error = character(0))

cat(sprintf("Starting retry processing for %d objects using main reprocessor...\n", length(failed_keys)))

# Process each failed key by calling the main reprocessor script
for (i in seq_along(failed_keys)) {
  key <- failed_keys[i]
  cat(sprintf("\n[%d/%d] Processing: %s\n", i, length(failed_keys), key))
  
  # Extract timestamp and partition path from the key
  timestamp_str <- parse_key_time(key)
  partition_path <- extract_partition_path(key)
  
  if (is.na(timestamp_str)) {
    cat(sprintf("  ❌ ERROR: Could not parse timestamp from key\n"))
    results <- rbindlist(list(results, data.table(key = key, status = "error", error = "Could not parse timestamp")), fill = TRUE)
    next
  }
  
  # Create a narrow time window around this specific object (±1 minute)
  base_time <- as.POSIXct(sub("Z$", "", timestamp_str), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  start_time <- base_time - 60  # 1 minute before
  end_time <- base_time + 60    # 1 minute after
  
  start_timestamp <- format(start_time, "%Y-%m-%dT%H:%M:%SZ")
  end_timestamp <- format(end_time, "%Y-%m-%dT%H:%M:%SZ")
  
  cat(sprintf("  Timestamp: %s\n", timestamp_str))
  cat(sprintf("  Partition: %s\n", partition_path))
  cat(sprintf("  Time window: %s to %s\n", start_timestamp, end_timestamp))
  
  # Build command to call main reprocessor
  cmd <- sprintf("Rscript tools/s3_object_reprocessor.R --environment %s --partition_path \"%s\" --start_timestamp \"%s\" --end_timestamp \"%s\" --log_file \"retry_temp_%d.csv\"",
                 opt$environment, partition_path, start_timestamp, end_timestamp, i)
  
  if (opt$dry_run) {
    cmd <- paste(cmd, "--dry_run")
    cat(sprintf("[DRY RUN] Would run: %s\n", cmd))
    results <- rbindlist(list(results, data.table(key = key, status = "dry_run", error = "")), fill = TRUE)
  } else {
    cat(sprintf("  Running: %s\n", cmd))
    
    # Execute the command
    exit_code <- system(cmd, ignore.stdout = FALSE, ignore.stderr = FALSE)
    
    if (exit_code == 0) {
      cat(sprintf("  ✅ SUCCESS: Reprocessor completed successfully\n"))
      results <- rbindlist(list(results, data.table(key = key, status = "success", error = "")), fill = TRUE)
      
      # Clean up temporary log file
      temp_log <- sprintf("retry_temp_%d.csv", i)
      if (file.exists(temp_log)) {
        file.remove(temp_log)
      }
    } else {
      cat(sprintf("  ❌ ERROR: Reprocessor failed with exit code %d\n", exit_code))
      results <- rbindlist(list(results, data.table(key = key, status = "error", error = sprintf("Exit code %d", exit_code))), fill = TRUE)
    }
  }
  
  # Small delay between retries
  Sys.sleep(2)
}

# Final summary
successful <- sum(results$status == "success")
failed <- sum(results$status == "error")
dry_run_count <- sum(results$status == "dry_run")

cat(sprintf("\n=== RETRY PROCESSING SUMMARY ===\n"))
cat(sprintf("Total objects retried: %d\n", length(failed_keys)))
if (opt$dry_run) {
  cat(sprintf("Dry run simulations: %d\n", dry_run_count))
} else {
  cat(sprintf("Successful: %d\n", successful))
  cat(sprintf("Still failed: %d\n", failed))
  cat(sprintf("Success rate: %.1f%%\n", if(length(failed_keys) > 0) (successful / length(failed_keys)) * 100 else 0))
}

# Add run parameters as header rows to the log file
run_info <- data.table(
  key = c("RETRY_RUN_PARAMETERS", "environment", "failed_files_list", "total_objects", "mode"),
  status = c("info", "info", "info", "info", "info"),
  error = c(as.character(Sys.time()), opt$environment, opt$failed_files_list, as.character(length(failed_keys)), if(opt$dry_run) "DRY_RUN" else "ACTUAL_RETRY")
)

# Combine run info with processing results
final_log <- rbindlist(list(run_info, results), fill = TRUE)
fwrite(final_log, opt$log_file)
cat(sprintf("Processing complete. Log written to %s\n", opt$log_file))

if (failed > 0) {
  cat(sprintf("\nWARNING: %d objects still failed after retry. Check %s for details.\n", failed, opt$log_file))
}
