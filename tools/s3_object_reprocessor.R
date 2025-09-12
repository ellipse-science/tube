#!/usr/bin/env Rscript

# s3_object_reprocessor.R
# Script to copy S3 objects onto themselves in batches, triggering Lambda ETL
# Usage: see function documentation below

suppressPackageStartupMessages({
  library(tube)
  library(paws)
  library(optparse)
  library(lubridate)
  library(data.table)
})

# Source internal functions from the package
source("R/aws.R")
source("R/datalake.R")
source("R/s3.R")
source("R/utils_check_params.R")  # For check_env function

#' Copy S3 objects onto themselves in batches, triggering Lambda ETL
#'
#' @param environment "PROD" or "DEV"
#' @param partition_path S3 partition path (e.g., "r-media-headlines/FXN/")
#' @param start_timestamp ISO 8601 start timestamp (e.g., "2024-08-21T00:00:00Z")
#' @param end_timestamp ISO 8601 end timestamp (e.g., "2024-08-22T00:00:00Z")
#' @param batch_size Number of objects per batch (default: 20)
#' @param batch_sleep Seconds to sleep between batches (default: 10)
#' @param object_sleep Seconds to sleep between objects (default: 0.5)
#' @param log_file File to save detailed log with run parameters (default: "s3_reprocess_log.csv")
#' @param dry_run If TRUE, only preview actions
#' @return None. Writes log file with run parameters and processing results.
#' @examples
#' # Run from command line:
#' # Rscript tools/s3_object_reprocessor.R
#' # --environment PROD
#' # --partition_path r-media-headlines/FXN/
#' # --start_timestamp 2024-08-21T00:00:00Z
#' # --end_timestamp 2024-08-22T00:00:00Z

# Option parsing
option_list <- list(
  make_option("--environment", type = "character", default = NULL, help = "PROD or DEV"),
  make_option("--partition_path", type = "character", default = NULL, help = "S3 partition path"),
  make_option("--start_timestamp", type = "character", default = NULL, help = "Start ISO 8601 timestamp"),
  make_option("--end_timestamp", type = "character", default = NULL, help = "End ISO 8601 timestamp"),
  make_option("--batch_size", type = "integer", default = 20, help = "Objects per batch"),
  make_option("--batch_sleep", type = "double", default = 10, help = "Seconds to sleep between batches"),
  make_option("--object_sleep", type = "double", default = 0.5, help = "Seconds to sleep between objects"),
  make_option("--log_file", type = "character", default = "s3_reprocess_log.csv", help = "Log file"),
  make_option("--dry_run", action = "store_true", default = FALSE, help = "Preview actions only")
)
opt <- parse_args(OptionParser(option_list = option_list))

# Validate required options
required_opts <- c("environment", "partition_path", "start_timestamp", "end_timestamp")
for (opt_name in required_opts) {
  if (is.null(opt[[opt_name]])) {
    stop(paste("Missing required argument:", opt_name), call. = FALSE)
  }
}

# Connect to AWS and get credentials
con <- tube::ellipse_connect(opt$environment)
creds <- get_aws_credentials(opt$environment)
buckets <- list_datalake_bucket(creds)

# Select the PRIVATE datalake bucket (not the public one)
# Contains "datalake" but does not contain "public"
bucket <- buckets[grep("datalake", buckets)]
bucket <- bucket[!grepl("public", bucket)]
if (length(bucket) > 1) bucket <- bucket[1]
if (length(bucket) == 0) stop("No private datalake bucket found", call. = FALSE)

cat(sprintf("Available buckets: %s\n", paste(buckets, collapse = ", ")))
cat(sprintf("Using PRIVATE datalake bucket: %s\n", bucket))
cat(sprintf("Searching in partition path: %s\n", opt$partition_path))

# Setup S3 client with credentials
s3 <- paws.storage::s3(config = c(creds, close_connection = TRUE))


# Paginated S3 object listing with MaxKeys = batch_size
cat("Listing objects in partition path (paginated)...\n")
parse_key_time <- function(key) {
  if (is.null(key) || length(key) == 0 || is.na(key)) return(NA_character_)
  m <- regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}Z", key)
  if (length(m) == 1 && m == -1) return(NA_character_)
  substr(key, m, m + attr(m, "match.length") - 1)
}

start_ts <- ymd_hms(sub("Z$", "", opt$start_timestamp), tz = "UTC")
end_ts <- ymd_hms(sub("Z$", "", opt$end_timestamp), tz = "UTC")

# Initialize log data table
log_dt <- suppressWarnings(
  data.table(key = character(), timestamp = character(), status = character(), error = character())
)

continuation_token <- NULL
batch_num <- 1
total_found <- 0
total_processed <- 0
empty_batches_count <- 0  # Counter for consecutive empty batches
max_empty_batches <- 50    # Stop after 50 consecutive empty batches (increased for large time gaps)

# Calculate StartAfter based on start timestamp to jump to the right section
# Format: r-media-headlines/FXN/2025-02-09T00:00:00.000Z
start_date_formatted <- sub("Z$", ".000Z", opt$start_timestamp)
start_after_key <- paste0(opt$partition_path, start_date_formatted)
cat(sprintf("Using StartAfter key: %s\n", start_after_key))

repeat {
  # List a page of objects
  tryCatch({
    if (is.null(continuation_token)) {
      objects <- s3$list_objects_v2(
        Bucket = bucket,
        Prefix = opt$partition_path,
        MaxKeys = as.integer(opt$batch_size),
        StartAfter = start_after_key
      )
    } else {
      objects <- s3$list_objects_v2(
        Bucket = bucket,
        Prefix = opt$partition_path,
        MaxKeys = as.integer(opt$batch_size),
        ContinuationToken = continuation_token
      )
    }
    cat(sprintf("S3 API call successful for batch %d\n", batch_num))
  }, error = function(e) {
    cat(sprintf("S3 API error: %s\n", e$message))
    stop(sprintf("Failed to list S3 objects: %s", e$message), call. = FALSE)
  })
  if (is.null(objects$Contents)) {
    if (batch_num == 1) {
      cat("No objects found in partition path.\n")
      cat("This could mean:\n")
      cat("1. The partition path doesn't exist\n")
      cat("2. The bucket is empty\n")
      cat("3. There are no objects matching the prefix\n")
    }
    break
  }
  
  cat(sprintf("Found %d objects in S3 response\n", length(objects$Contents)))
  
  # Extract object keys
  keys <- vapply(objects$Contents, function(x) x$Key, "")
  
  # Debug: Show first few keys
  if (length(keys) > 0) {
    cat("Sample keys found:\n")
    for (i in seq_len(min(3, length(keys)))) {
      cat(sprintf("  Key %d: %s\n", i, keys[i]))
    }
  }
  
  # Parse timestamps
  timestamps_str <- sapply(keys, parse_key_time, USE.NAMES = FALSE)
  
  # Debug: Show parsed timestamps
  if (length(timestamps_str) > 0) {
    cat("Sample parsed timestamps:\n")
    for (i in seq_len(min(3, length(timestamps_str)))) {
      cat(sprintf("  Timestamp %d: %s\n", i, if(is.na(timestamps_str[i])) "NA" else timestamps_str[i]))
    }
  }
  
  # Filter out keys without valid timestamps
  valid_idx <- !is.na(timestamps_str) & timestamps_str != ""
  keys <- keys[valid_idx]
  timestamps_str <- timestamps_str[valid_idx]
  
  if (length(keys) == 0) {
    # No objects in this page match filter, continue to next page
    if (!isTRUE(objects$IsTruncated)) break
    continuation_token <- objects$NextContinuationToken
    batch_num <- batch_num + 1
    next
  }
  
  # Convert to POSIXct and filter by time range
  timestamps <- ymd_hms(sub("Z$", "", timestamps_str), tz = "UTC")
  
  # Early termination check: if the first object is past our end time, we're done
  if (length(timestamps) > 0) {
    first_timestamp <- min(timestamps, na.rm = TRUE)
    if (first_timestamp > end_ts) {
      cat("Early termination: First object timestamp", format(first_timestamp, "%Y-%m-%d %H:%M:%S"), 
          "is past end time", format(end_ts, "%Y-%m-%d %H:%M:%S"), "\n")
      break
    }
  }
  
  time_filter <- timestamps >= start_ts & timestamps <= end_ts
  keys <- keys[time_filter]
  timestamps <- timestamps[time_filter]
  
  batch_size_actual <- length(keys)
  total_found <- total_found + batch_size_actual
  
  if (batch_size_actual == 0) {
    empty_batches_count <- empty_batches_count + 1
    cat(
      sprintf(
        "Batch %d: 0 objects in time range (empty batch %d/%d)\n", batch_num, empty_batches_count, max_empty_batches
      )
    )
    
    # Stop if we've had too many consecutive empty batches
    if (empty_batches_count >= max_empty_batches) {
      cat("Stopping: Too many consecutive empty batches - likely past time range\n")
      break
    }
    
    # Continue to next page
    if (!isTRUE(objects$IsTruncated)) break
    continuation_token <- objects$NextContinuationToken
    batch_num <- batch_num + 1
    next
  } else {
    empty_batches_count <- 0  # Reset counter when we find objects
    cat(sprintf("Batch %d: %d objects in time range\n", batch_num, batch_size_actual))
  }
  
  for (i in seq_along(keys)) {
    key <- keys[i]
    ts <- timestamps[i]
    status <- "success"
    error <- ""
    if (opt$dry_run) {
      cat(sprintf("[DRY RUN] Would copy: %s\n", key))
    } else {
      tryCatch({
        # Get current object metadata first
        head_result <- s3$head_object(Bucket = bucket, Key = key)
        
        # Prepare metadata with reprocessing timestamp
        current_metadata <- head_result$Metadata
        if (is.null(current_metadata)) current_metadata <- list()
        current_metadata$reprocessed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
        
        s3$copy_object(
          Bucket = bucket,
          CopySource = paste0(bucket, "/", key),
          Key = key,
          MetadataDirective = "REPLACE",
          Metadata = current_metadata,
          ContentType = head_result$ContentType %||% "text/html"
        )
        Sys.sleep(opt$object_sleep)
      }, error = function(e) {
        status <<- "error"
        error <<- e$message
        cat(sprintf("Error copying %s: %s\n", key, error))
      })
    }
    # Create new row avoiding data.table key interpretation issues
    new_row <- list(key = key, timestamp = as.character(ts), status = status, error = error)
    log_dt <- rbindlist(list(log_dt, new_row), fill = TRUE)
  }
  
  # BATCH SUMMARY - Perfect for reading during sleep/breaks!
  cat("\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("BATCH", batch_num, "SUMMARY\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("Objects found in S3 response:", length(objects$Contents), "\n")
  cat("Objects in target time range:", batch_size_actual, "\n")
  cat("Total processed so far:", total_processed + batch_size_actual, "\n")
  cat("Total batches examined:", batch_num, "\n")
  if (batch_size_actual > 0) {
    time_range_start <- min(timestamps, na.rm = TRUE)
    time_range_end <- max(timestamps, na.rm = TRUE)
    cat("Time range in this batch:",
      format(time_range_start, "%Y-%m-%d %H:%M"), "to",
      format(time_range_end, "%Y-%m-%d %H:%M"), "\n")
  }
  cat("Processing mode:", if(opt$dry_run) "DRY RUN" else "LIVE COPY", "\n")
  cat("Perfect time for a coffee/reading break!\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("\n")
  
  # Update the total processed count
  total_processed <- total_processed + batch_size_actual
  
  # Sleep between batches (but not after the last batch)
  if (batch_size_actual > 0 && isTRUE(objects$IsTruncated)) {
    Sys.sleep(opt$batch_sleep)
  }
  if (!isTRUE(objects$IsTruncated)) break
  continuation_token <- objects$NextContinuationToken
  batch_num <- batch_num + 1
}

# Final summary
cat(sprintf("\n=== PROCESSING SUMMARY ===\n"))
cat(sprintf("Total batches examined: %d\n", batch_num))
cat(sprintf("Total objects found in time range: %d\n", total_found))
cat(sprintf("Total objects processed/copied: %d\n", total_processed))
cat(sprintf("Time range: %s to %s\n", opt$start_timestamp, opt$end_timestamp))
cat(sprintf("Partition path: %s\n", opt$partition_path))
cat(sprintf("Mode: %s\n", if(opt$dry_run) "DRY RUN" else "ACTUAL COPY"))

# Add run parameters as header rows to the log file
run_info <- data.table(
  s3_key = c("RUN_PARAMETERS", "environment", "partition_path", "start_timestamp", "end_timestamp", "batch_size", "mode"),
  timestamp = c(as.character(Sys.time()), opt$environment, opt$partition_path, opt$start_timestamp, opt$end_timestamp, as.character(opt$batch_size), if(opt$dry_run) "DRY_RUN" else "ACTUAL_COPY"),
  status = c("info", "info", "info", "info", "info", "info", "info"),
  error = c("", "", "", "", "", "", "")
)

# Rename column to match log_dt structure
setnames(run_info, "s3_key", "key")

# Combine run info with processing log
final_log <- rbindlist(list(run_info, log_dt), fill = TRUE)
fwrite(final_log, opt$log_file)
cat(sprintf("Processing complete. Log written to %s\n", opt$log_file))
