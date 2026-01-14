#' @title List S3 buckets
#' @description List all the S3 buckets that match the type
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @param type The type of bucket to list
#' @return A list of S3 buckets
list_s3_buckets <- function(credentials, type) {
  logger::log_debug("[tube::list_s3_buckets] entering function")

  logger::log_debug("[tube::list_s3_buckets] instanciating s3 client")
  s3_client <- paws.storage::s3(
    config = c(
      credentials,
      close_connection = TRUE
    )
  )

  logger::log_debug("[tube::list_s3_buckets] listing buckets")
  r <- tryCatch(
    {
      s3_client$list_buckets()
    },
    error = function(e) {
      logger::log_error(paste("[tube::list_s3_buckets] AWS API error:", e$message))
      return(NULL)
    }
  )

  if (is.null(r) || is.null(r$Buckets) || length(r$Buckets) == 0) {
    logger::log_warn("[tube::list_s3_buckets] no buckets returned from AWS API")
    return(NULL)
  }

  logger::log_debug("[tube::list_s3_buckets] wrangling result")
  list <- unlist(r$Buckets)
  logger::log_debug(paste("[tube::list_s3_buckets] found", length(list), "total buckets"))
  logger::log_debug(paste("[tube::list_s3_buckets] searching for pattern:", type))
  
  bucket_list <- list[grep(type, list)]
  logger::log_debug(paste("[tube::list_s3_buckets] found", length(bucket_list), "buckets matching pattern"))
  
  if (length(bucket_list) == 0) {
    logger::log_warn(paste0(
      "[tube::list_s3_buckets] no bucket found with pattern '", type, "'. ",
      "Available buckets: ", paste(list, collapse = ", ")
    ))
    return(NULL)
  }
  
  bucket_list <- as.list(bucket_list)

  # Only assign names if bucket_list is not empty
  if (length(bucket_list) > 0) {
    names(bucket_list) <- ""
  }

  bucket_list <- unlist(bucket_list)
  logger::log_debug(paste("[tube::list_s3_buckets] returning bucket:", bucket_list))

  logger::log_debug("[tube::list_s3_buckets] returning results")
  bucket_list
}

#' @title List S3 partitions
#' @description List all the S3 partitions in a bucket even the empty ones (containing no object)
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @param bucket The bucket to list partitions from
#' @return A character vector of S3 partitions
list_s3_partitions <- function(credentials, bucket) {
  logger::log_debug("[tube::list_s3_partitions] entering function")

  # Input validation
  if (is.null(bucket) || nchar(bucket) == 0) {
    stop("bucket parameter cannot be NULL or empty", call. = FALSE)
  }

  logger::log_debug("[tube::list_s3_partitions] instanciating s3 client")
  s3_client <- paws.storage::s3(
    config = c(
      credentials,
      close_connection = TRUE
    )
  )

  logger::log_debug("[tube::list_s3_partitions] listing partitions")
  r <- tryCatch(
    {
      s3_client$list_objects_v2(
        Bucket = bucket,
        Delimiter = "/"
      )
    },
    error = function(e) {
      if (grepl("NoSuchBucket", e$message)) {
        logger::log_debug("[tube::list_s3_partitions] bucket does not exist")
        NULL
      } else {
        logger::log_error("[tube::list_s3_partitions] AWS API error")
        NULL
      }
    }
  )

  if (is.null(r)) {
    return(NULL)
  }

  # Make a unnamed list of it
  logger::log_debug("[tube::list_s3_partitions] wrangling result")
  partition_list <- r$CommonPrefixes
  partition_list <- lapply(partition_list, function(x) x$Prefix)
  partition_list <- unlist(partition_list)
  logger::log_debug("[tube::list_s3_partitions] returning results")
  partition_list
}

#' @title List S3 folders
#' @description List all the S3 folders within a particular prefix (or partition) in a bucket
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @param bucket The bucket to list folders from
#' @param prefix The prefix (or partition) to list folders from
#' @return A character vector of S3 folders
list_s3_folders <- function(credentials, bucket, prefix) {
  logger::log_debug("[tube::list_s3_folders] entering function")

  # Input validation
  if (is.null(bucket) || nchar(bucket) == 0) {
    stop("bucket parameter cannot be NULL or empty", call. = FALSE)
  }

  logger::log_debug("[tube::list_s3_folders] instanciating s3 client")
  s3_client <- paws.storage::s3(
    config = c(
      credentials,
      close_connection = TRUE
    )
  )

  logger::log_debug("[tube::list_s3_folders] listing folders")
  r <- tryCatch(
    {
      s3_client$list_objects_v2(
        Bucket = bucket,
        Prefix = prefix,
        Delimiter = "/"
      )
    },
    error = function(e) {
      if (grepl("NoSuchBucket", e$message)) {
        logger::log_debug("[tube::list_s3_folders] bucket does not exist")
        NULL
      } else {
        logger::log_error("[tube::list_s3_folders] AWS API error")
        NULL
      }
    }
  )

  if (is.null(r)) {
    return(NULL)
  }

  # Make a unnamed list of it
  logger::log_debug("[tube::list_s3_folders] wrangling result")
  folder_list <- r$CommonPrefixes
  folder_list <- lapply(folder_list, function(x) x$Prefix)
  # Fix regex to handle both root-level and nested folders
  folder_list <- lapply(folder_list, function(x) {
    # Remove trailing slash first
    cleaned <- gsub("/$", "", x)
    # Extract the last part after the last slash (or the whole string if no slash)
    if (grepl("/", cleaned)) {
      # Has slashes - extract last part
      regmatches(cleaned, regexec(".*/(.*)$", cleaned))[[1]][2]
    } else {
      # No slashes - return as is
      cleaned
    }
  })
  folder_list <- unlist(folder_list)
  logger::log_debug("[tube::list_s3_folders] returning results")
  folder_list
}

#' Upload a file to an S3 bucket
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @param file The file to upload
#' @param bucket The bucket to upload the file to
#' @param key The key to use for the file
#' @return TRUE if the file was uploaded successfully
upload_file_to_s3 <- function(credentials, file, bucket, key) {
  logger::log_debug("[tube::upload_file_to_s3] entering function")

  # Check that the file exists
  if (!file.exists(file)) {
    logger::log_error("[tube::upload_file_to_s3] file does not exist")
    stop("File does not exist: ", file, call. = FALSE)
  }

  logger::log_debug("[tube::upload_file_to_s3] instanciating s3 client")
  s3_client <- paws.storage::s3(
    config = c(
      credentials,
      close_connection = TRUE
    )
  )

  tryCatch(
    {
      logger::log_debug("[tube::upload_file_to_s3] uploading file")
      s3_client$put_object(
        Bucket = bucket,
        Body = file,
        Key = key
      )
    },
    error = function(e) {
      logger::log_error("[tube::upload_file_to_s3] error uploading file")
      logger::log_error(e$message)
      FALSE
    }
  )

  logger::log_debug("[tube::upload_file_to_s3] returning results")
  TRUE
}

#' Delete s3 folder
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @param bucket The bucket to delete the folder from
#' @param prefix The prefix (or partition) to delete
#' @return TRUE if the folder was deleted successfully
delete_s3_folder <- function(credentials, bucket, prefix) {
  logger::log_debug("[tube::delete_s3_folder] entering function")

  # Input validation
  if (is.null(bucket) || nchar(bucket) == 0) {
    stop("bucket parameter cannot be NULL or empty", call. = FALSE)
  }

  # if prefix does not end with / add it
  if (!endsWith(prefix, "/")) {
    prefix <- paste0(prefix, "/")
  }

  logger::log_debug("[tube::delete_s3_folder] instanciating s3 client")
  s3_client <- paws.storage::s3(
    config = c(
      credentials,
      close_connection = TRUE
    )
  )

  logger::log_debug(paste("[tube::delete_s3_folder] listing objects in bucket", bucket, "prefix", prefix))
  pass <- 1
  r <- s3_client$list_objects_v2(
    Bucket = bucket,
    Prefix = prefix,
    MaxKeys = 1000
  )

  object_list <- r$Contents

  while (!is.null(r$NextContinuationToken) && length(r$NextContinuationToken) > 0) {
    pass <- pass + 1
    logger::log_debug(paste("[tube::delete_s3_folder] more objects to list in pass", pass))
    r <- s3_client$list_objects_v2(
      Bucket = bucket,
      Prefix = prefix,
      MaxKeys = 1000,
      ContinuationToken = r$NextContinuationToken
    )
    object_list <- c(object_list, r$Contents)
  }

  # Check if the folder is empty
  if (length(object_list) > 0) {
    logger::log_debug(paste("[tube::delete_s3_folder] folder contains", length(object_list), "objects"))

    # Loop through each object and delete it
    for (object in object_list) {
      if (startsWith(object$Key, prefix)) {
        tryCatch(
          {
            logger::log_debug("[tube::delete_s3_folder] deleting object")
            s3_client$delete_object(
              Bucket = bucket,
              Key = object$Key
            )
          },
          error = function(e) {
            logger::log_error("[tube::delete_s3_folder] error deleting object")
            logger::log_error(e$message)
            FALSE
          }
        )
      }
    }
  } else {
    logger::log_debug("[tube::delete_s3_folder] folder is empty : deleting folder only")
    # Delete the folder
    tryCatch(
      {
        logger::log_debug("[tube::delete_s3_folder] deleting folder")
        s3_client$delete_object(
          Bucket = bucket,
          Key = prefix
        )
      },
      error = function(e) {
        logger::log_error("[tube::delete_s3_folder] error deleting folder")
        logger::log_error(e$message)
        FALSE
      }
    )
  }

  logger::log_debug("[tube::delete_s3_folder] returning results")
  TRUE
}

#' Download S3 file to temporary location
#' @param s3_path S3 path (s3://bucket/key format)
#' @param credentials AWS credentials from get_aws_credentials()
#' @keywords internal
download_s3_file_to_temp <- function(s3_path, credentials) {
  # Parse S3 path
  s3_parts <- gsub("^s3://", "", s3_path)
  bucket_and_key <- strsplit(s3_parts, "/", fixed = TRUE)[[1]]
  bucket <- bucket_and_key[1]
  key <- paste(bucket_and_key[-1], collapse = "/")

  # Create temp file with appropriate extension
  temp_file <- tempfile(fileext = paste0(".", tools::file_ext(key)))

  # Download using paws.storage with credentials
  s3 <- paws.storage::s3(
    config = c(
      credentials,
      close_connection = TRUE
    )
  )
  s3$download_file(
    Bucket = bucket,
    Key = key,
    Filename = temp_file
  )

  temp_file
}
