# Returns the name of the landing zone bucket
# Private function
#
list_landing_zone_bucket <- function(credentials) {
  logger::log_debug("[tube::list_landing_zone_bucket] entering function")

  datalake_list <- list_s3_buckets("landingzonebucket", credentials)

  logger::log_debug("[tube::list_landing_zone_bucket] returning results")
  return(datalake_list)
}

#' Uploads the files in the specified local folder to the landing zone bucket
#'
#' @param creds An AWS session object with your credentials and the aws ressources required
#' @param local_folder The path to the local folder containing the files to upload
#' @param pipeline_name The name of the pipeline (i.e.: the name of the first folder in the path within the landding zone bucket)
#' @param batch mandatory parameter to specify the batch name.  If not provided, the date and time batch will be used
#'
#' @returns the status of each file upload
#' @export
#' @examples \dontrun{
#'  r <- upload_to_landing_zone(aws_session(), "my_pipeline")
#' print(r)
#' }
upload_to_landing_zone <- function(creds, local_folder, pipeline_name, batch) {
  logger::log_debug("[tube::upload_to_landing_zone] entering function")

  if (is.null(batch)) {
    # Use UTC time as batch name
    batch <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  } else {
    batch <- toupper(batch)
  }

  # checkmate on parameters
  checkmate::assert_string(local_folder)
  checkmate::assert_string(pipeline_name)
  checkmate::assert_string(batch)

  # instanciate s3 client
  s3_client <- paws.storage::s3(
    config = c(
      creds$credentials,
      close_connection = TRUE)
  )

  # list files in local folder in try catch
  tryCatch({
    logger::log_debug("[tube::upload_to_landing_zone] listing files in the local folder")
    files <- list.files(local_folder, full.names = TRUE)
    logger::log_debug(paste("[tube::upload_to_landing_zone] found", length(files), "files in the local folder:", local_folder))
  }, error = function(e) {
    logger::log_error("[tube::upload_to_landing_zone] an error occurred while listing files in the local folder.  Check the path and the permissions")
  })

  landing_zone_bucket <- creds$landing_zone_bucket
  prefix <- paste0(pipeline_name, "/DEFAULT/")

  # verify if the pipeline exists in the landing zone bucket
  # by listing folders at the root of the landing zone bucket
  objects <- s3_client$list_objects_v2(Bucket = landing_zone_bucket)
  prefixes_found <- sapply(objects$Contents, function(x) x$Key == prefix)
  is_found <- any(prefixes_found)

  if (!is_found) {
    message <- "[tube::upload_to_landing_zone] the pipeline folder does not exist in the landing zone bucket.  \
               Please ask your data platform administrator to create it for you and to ensure that the data pipeline\
               components have been implemented to process the data to the datawarehouse"
    logger::log_error(message)
    return(NULL)
  }

  uploaded_files <- list()

  # upload files to the landing zone bucket
  for (file in files) {
    key <- paste0(prefix, basename(file))

    logger::log_debug(paste("[tube::upload_to_landing_zone] uploading file: ", file, " to key: ", key))
    tryCatch({
      s3_client$put_object(Bucket = landing_zone_bucket, Key = key, Body = file, Metadata=list(batch=batch))
      uploaded_files <- c(uploaded_files, key)
      logger::log_debug(paste("[tube::upload_to_landing_zone] file: ", file, " uploaded to key: ", key))
    }, error = function(e) {
      logger::log_error(paste("[tube::upload_to_landing_zone] an error occurred while uploading file: ", file, " to key: ", key))
    })
  }

  logger::log_info(paste("[tube::upload_to_landing_zone]", length(uploaded_files), "files uploaded to the landing zone bucket"))

  logger::log_debug("[tube::upload_to_landing_zone] exiting function")
  return(uploaded_files)
}