#' Returns the landing zone bucket name
#'
#' Technically the function returns all the buckets for which
#' the name contains the string `datalakebucket` but in our infrastructure,
#' there is only one per AWS account (DEV/PROD)
#'
#' The landing zone is used by ellipse_ingest to allow users
#' to ingest raw data into the dataplatform so that it gets
#' processed by the pipeline (extractor - loader)
#'
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @returns A string containing the name of the datalake bucket
list_landing_zone_bucket <- function(credentials) {
  logger::log_debug("[tube::list_landing_zone_bucket] entering function")

  datalake_list <- list_s3_buckets(credentials, "landingzonebucket")

  logger::log_debug("[tube::list_landing_zone_bucket] returning results")
  return(datalake_list)
}

# Returns the first level partition names of the landing zone bucket
#
list_landing_zone_partitions <- function(credentials) {
  logger::log_debug("[tube::list_landing_zone_partitions] entering function")

  landing_zone_bucket <- list_landing_zone_bucket(credentials)
  partitions_list <- list_s3_partitions(credentials, landing_zone_bucket)

  logger::log_debug("[tube::list_landing_zone_partitions] returning results")
  return(partitions_list)
}

#' Uploads the file specified file to the landing zone bucket.
#'
#' This function is used by the ellipse_ingest function to upload the raw data
#' to the landing zone bucket so that it gets processed by the pipeline (extractor - loader)
#'
#' @param credentials An AWS session object with your credentials and the aws ressources required
#' @param filepath The path to the file to upload
#' @param pipeline_name The name of the pipeline (i.e.: the name of the first folder in the path within the landding zone bucket)
#' @param file_batch mandatory parameter to specify the batch name.  if batch is specified, then version must be NULL.  Batch is used ONLY for factual data to help you retrieve your data in the datawarehouse
#' @param file_version mandatory parameter to specify the version of the file. if version is specified, then batch must be NULL. Version is used ONLY for reference data (dimensions, dictionaries) to help you retrieve your data in the datawarehouse
#'
#' @returns the status of each file upload
#' @examples \dontrun{
#' r <- upload_file_to_landing_zone(get_aws_credentials(), "my_folder", "my_pipeline", NULL, TRUE)
#' print(r)
#' }
upload_file_to_landing_zone <- function(credentials, filepath, pipeline_name, file_batch = NULL, file_version = NULL) {
  logger::log_debug("[tube::upload_file_to_landing_zone] entering function")

  # instanciate s3 client
  s3_client <- paws.storage::s3(
    config = c(
      credentials,
      close_connection = TRUE
    )
  )

  # remove last slash in path if any
  filepath <- gsub("/$", "", filepath)

  landing_zone_bucket <- list_landing_zone_bucket(credentials)
  prefix <- paste0(pipeline_name, "/DEFAULT/")

  filename <- basename(filepath)

  if (!is.null(file_batch)) {
    file_prefix <- file_batch
  } else {
    if (!is.null(file_version)) {
      file_prefix <- file_version
    } else {
      stop("Either file_batch or version must be specified")
    }
  }

  prefix_for_filename <- iconv(file_prefix, "ASCII", "UTF-8", sub = "")
  prefix_for_filename <- gsub("[^[:alnum:]\\-_./]", "", prefix_for_filename)

  filename <- paste0(prefix_for_filename, "-", filename)

  key <- paste0(prefix, filename)

  logger::log_debug(paste("[tube::upload_file_to_landing_zone] uploading file: ", filepath, " to key: ", key))
  tryCatch(
    {
      s3_client$put_object(
        Bucket = landing_zone_bucket,
        Key = key,
        Body = filepath,
        Metadata = list(
          batch = if (!is.null(file_batch)) file_batch else NULL,
          version = if (!is.null(file_version)) file_version else NULL
        ),
        ContentType = "application/octet-stream; charset=utf-8"
      )
      logger::log_debug(paste("[tube::upload_file_to_landing_zone] file: ", filepath, " uploaded to key: ", key))
      TRUE
    },
    error = function(e) {
      logger::log_error(paste("[tube::upload_file_to_landing_zone] an error occurred while uploading file: ", filepath, " to key: ", key))
      FALSE
    }
  )

  logger::log_debug("[tube::upload_file_to_landing_zone] exiting function")
  TRUE
}
