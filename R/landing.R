# Returns the name of the landing zone bucket
#
list_landing_zone_bucket <- function(credentials) {
  logger::log_debug("[tube::list_landing_zone_bucket] entering function")

  datalake_list <- list_s3_buckets("landingzonebucket", credentials)

  logger::log_debug("[tube::list_landing_zone_bucket] returning results")
  return(datalake_list)
}

# Returns the first level partition names of the landing zone bucket
#
list_landing_zone_partitions <- function(credentials) {
  logger::log_debug("[tube::list_landing_zone_partitions] entering function")

  landing_zone_bucket <- list_landing_zone_bucket(credentials)
  partitions_list <- list_s3_partitions(landing_zone_bucket, credentials)

  logger::log_debug("[tube::list_landing_zone_partitions] returning results")
  return(partitions_list)
}

#' Uploads the file specified to the landing zone bucket.
#'
#' @param credentials An AWS session object with your credentials and the aws ressources required
#' @param filepath The path to the file to upload
#' @param pipeline_name The name of the pipeline (i.e.: the name of the first folder in the path within the landding zone bucket)
#' @param file_batch mandatory parameter to specify the batch name.  if batch is specified, then version must be NULL.  Batch is used ONLY for factual data to help you retrieve your data in the datawarehouse
#' @param file_version mandatory parameter to specify the version of the file. if version is specified, then batch must be NULL. Version is used ONLY for reference data (dimensions, dictionaries) to help you retrieve your data in the datawarehouse
#'
#' @returns the status of each file upload
#' @examples \dontrun{
#'  r <- upload_file_to_landing_zone(get_aws_credentials(), "my_folder", "my_pipeline", NULL, TRUE)
#' print(r)
#' }
upload_file_to_landing_zone <- function(credentials, filepath, pipeline_name, file_batch = NULL, file_version = NULL) {
  logger::log_debug("[tube::upload_file_to_landing_zone] entering function")

  # instanciate s3 client
  s3_client <- paws.storage::s3(
    config = c(
      credentials,
      close_connection = TRUE)
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

  prefix_for_filename <- iconv(file_prefix, "ASCII", "UTF-8", sub="")
  prefix_for_filename <- gsub("[^[:alnum:]\\-_./]", "", prefix_for_filename)

  filename <- paste0(prefix_for_filename, '-', filename)

  key <- paste0(prefix, filename)

  logger::log_debug(paste("[tube::upload_file_to_landing_zone] uploading file: ", filepath, " to key: ", key))
  tryCatch({
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
    return(TRUE)
  }, error = function(e) {
    logger::log_error(paste("[tube::upload_file_to_landing_zone] an error occurred while uploading file: ", filepath, " to key: ", key))
    return(FALSE)
  })
 
  logger::log_debug("[tube::upload_file_to_landing_zone] exiting function")
  TRUE
}


#' Checks is the files in the landing zone have been processed and tells the user if he can 
#' upload more
#'
#' @param credentials An AWS session object with your credentials and the aws ressources required
#' @param pipeline_name The name of the pipeline (i.e.: the name of the first folder in the path within the landding zone bucket)
#'
#' @returns the number of unprocessed files in the landing zone
#' @export
#' @examples \dontrun{
#'  r <- check_landing_zone(get_aws_credentials(), "my_pipeline")
#' print(r)
#' }
check_landing_zone <- function(credentials, pipeline_name) {
  logger::log_debug("[tube::check_landing_zone] entering function")

  checkmate::assert_string(pipeline_name)

  # instanciate s3 client
  s3_client <- paws.storage::s3(
    config = c(
      credentials$credentials,
      close_connection = TRUE)
  )

  landing_zone_bucket <- credentials$landing_zone_bucket
  prefix <- paste0(pipeline_name, "/DEFAULT/")

  # verify if the pipeline exists in the landing zone bucket
  # by listing folders at the root of the landing zone bucket
  objects <- s3_client$list_objects_v2(Bucket = landing_zone_bucket)

  # Take only unprocessed objects and files
  objects_list <- objects$Contents
  filtered_data <- Filter(function(x) {
    !grepl("processed", x$Key) && 
    !grepl(paste0("^", pipeline_name, "/?$"), x$Key) && 
    !grepl(paste0("^", pipeline_name, "/DEFAULT/?$"), x$Key)
  }, objects_list)

  # Take only objects from the pipelines specified by pipeline_name
  nb_unprocessed_files <- Filter(function(x) {
    grepl(paste0("^", pipeline_name, "/"), x$Key)
  }, filtered_data)

  logger::log_debug(
    paste("[tube::check_landing_zone] There are currently",
      length(nb_unprocessed_files),
      "unprocessed files in the landing zone for pipeline",
      pipeline_name)
  )

  logger::log_debug("[tube::upload_file_to_landing_zone] exiting function")
  return(length(nb_unprocessed_files))
}



