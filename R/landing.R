# Returns the name of the landing zone bucket
# Private function
#
#' @export
list_landing_zone_bucket <- function(credentials) {
  logger::log_debug("[tube::list_landing_zone_bucket] entering function")

  datalake_list <- list_s3_buckets("landingzonebucket", credentials)

  logger::log_debug("[tube::list_landing_zone_bucket] returning results")
  return(datalake_list)
}

#' Uploads the files in the specified local folder to the landing zone bucket.  The landing zone takes a maximum of 30 files 
#' at a time.  If your local_folder contains more than 30 files, the files will be segmented into sets of 30 files and uploaded
#' one set at a time.  The function will wait 6 minutes between each set to ensure that the data platform has time to process the files.
#' Please ensure that your pipeline has been configured by your data platform administrator before uploading data to the landing zone, otherwise
#' the function will return an error message.
#'
#' @param creds An AWS session object with your credentials and the aws ressources required
#' @param local_folder The path to the local folder containing the files to upload
#' @param pipeline_name The name of the pipeline (i.e.: the name of the first folder in the path within the landding zone bucket)
#' @param batch mandatory parameter to specify the batch name.  If not provided, the date and time batch will be used
#' @param timestamp_files If TRUE, the files will be timestamped with the current date and time.
#'   This will ensure that the files are unique in the landing zone bucket
#'
#' @returns the status of each file upload
#' @export
#' @examples \dontrun{
#'  r <- upload_to_landing_zone(aws_session(), "my_filder", "my_pipeline", NULL, TRUE)
#' print(r)
#' }
upload_to_landing_zone <- function(creds, local_folder, pipeline_name, batch = NULL, timestamp_files = FALSE) {
  landing_zone_capacity <- 30
  time_buffer <- 600
  logger::log_debug("[tube::upload_to_landing_zone] entering function")

  if (is.null(batch)) {
    # raise error message
    stop("[tube::upload_to_landing_zone] The batch parameter is mandatory.  \
       Please provide a batch name.  The batch is used to help you retrieve your data in the datawarehouse.  \
       It can be a topic, a project name, your name, or anything that describes the data you are uploading to the data platform etc.
       If you do not have a batch name, you can use the date and time as a batch name.  \
       For example: ", Sys.Date())
  } else {
    batch <- toupper(batch)
  }

  # checkmate on parameters
  # test folder exists with checkmate
  checkmate::assert_directory(local_folder)
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
    all_files <- list.files(local_folder, full.names = TRUE)
    files <- all_files[!sapply(all_files, function(x) file.info(x)$isdir)]
    if (length(files) == 0) {
      logger::log_error("[tube::upload_to_landing_zone] no file in folder.")
      stop()
    } else {
      logger::log_info(paste("[tube::upload_to_landing_zone] There are", length(files), "files in the local folder:", local_folder))
      if (length(files) > landing_zone_capacity) {
        logger::log_info(paste0("[tube::upload_to_landing_zone] Segmenting files into sets of", landing_zone_capacity, "files"))
      }
    }
  }, error = function(e) {
    logger::log_error("[tube::upload_to_landing_zone] an error occurred while listing files in the local folder.\
      - Check the path and the permissions\
      - Check that the local folder exists\
      - check that it contains files ")
  })


  nb_files_in_landing_zone <- check_landing_zone(creds, pipeline_name)

  num_files <- length(files)
  num_batches <- ceiling(num_files / landing_zone_capacity)

  uploaded_files <- c()

  for (i in 1:num_batches) {

    logger::log_info(paste("[tube::upload_to_landing_zone] uploading set", i, "of", num_batches))

    batch_start <- (i - 1) * landing_zone_capacity + 1
    batch_end <- min(i * landing_zone_capacity, num_files)

    batch_files <- files[batch_start:batch_end]
    batch_folder <- file.path(local_folder, paste0("set_", i))

    dir.create(batch_folder)
    file.rename(batch_files, file.path(batch_folder, basename(batch_files)))

    # construct a new batch_files list with the batch_folder within
    batch_files <- list.files(batch_folder, full.names = TRUE)

    # check that the batch size + the nb files in landing zone are not > landing_zone_capacity
    if (length(batch_files) + nb_files_in_landing_zone > landing_zone_capacity) {
      logger::log_info("[tube::upload_to_landing_zone] Too many files currently remain in the landing zone.  \
            Please wait for those files to be processed by the data platform before uploading more.\
            The landing zone supports a maximum of 30 simultaneous files.  Waiting 6 minutes for landing zone to be free...")
      Sys.sleep(time_buffer)  # sleep for 10 minutes
    }

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

    # upload files to the landing zone bucket
    for (file in batch_files) {
      if (timestamp_files) {
        # convert basename of file by appending the UTC date time in front of it
        # this will ensure that the file is unique in the landing zone bucket
        # and that the file is not overwritten
        filename <- paste0(format(Sys.time(), tz = "UTC", usetz = TRUE), "-", basename(file))
        filename <- gsub("UTC", "Z", filename)
        filename <- gsub(" (\\d{2}:\\d{2}:\\d{2}) Z", "T\\1.000Z", filename)
      } else {
        filename <- basename(file)
      }

      key <- paste0(prefix, filename)

      logger::log_debug(paste("[tube::upload_to_landing_zone] uploading file: ", file, " to key: ", key))
      tryCatch({
        s3_client$put_object(Bucket = landing_zone_bucket, Key = key, Body = file, Metadata=list(batch=batch))
        uploaded_files <- c(uploaded_files, key)
        cat(".")
        if (length(uploaded_files) == length(files)) cat("\n")
        logger::log_debug(paste("[tube::upload_to_landing_zone] file: ", file, " uploaded to key: ", key))
      }, error = function(e) {
        logger::log_error(paste("[tube::upload_to_landing_zone] an error occurred while uploading file: ", file, " to key: ", key))
      })
    }
    cat("\n")
    logger::log_info(paste("[tube::upload_to_landing_zone] set", i, "of", num_batches, "uploaded to the landing zone bucket"))

    if (i < num_batches) {
      logger::log_info(paste("[tube::upload_to_landing_zone] waiting 6 minute before uploading the next set"))
      Sys.sleep(time_buffer)  # sleep for 10 minutes
    }
  }

  logger::log_info(paste("[tube::upload_to_landing_zone]", length(uploaded_files), "files uploaded to the landing zone bucket"))

  logger::log_debug("[tube::upload_to_landing_zone] exiting function")
}


#' Checks is the files in the landing zone have been processed and tells the user if he can 
#' upload more
#'
#' @param creds An AWS session object with your credentials and the aws ressources required
#' @param pipeline_name The name of the pipeline (i.e.: the name of the first folder in the path within the landding zone bucket)
#'
#' @returns the number of unprocessed files in the landing zone
#' @export
#' @examples \dontrun{
#'  r <- check_landing_zone(aws_session(), "my_pipeline")
#' print(r)
#' }
check_landing_zone <- function(creds, pipeline_name) {
  logger::log_debug("[tube::check_landing_zone] entering function")

  checkmate::assert_string(pipeline_name)

  # instanciate s3 client
  s3_client <- paws.storage::s3(
    config = c(
      creds$credentials,
      close_connection = TRUE)
  )

  landing_zone_bucket <- creds$landing_zone_bucket
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

  logger::log_info(
    paste("[tube::check_landing_zone] There are currently",
      length(nb_unprocessed_files),
      "unprocessed files in the landing zone for pipeline",
      pipeline_name)
  )

  logger::log_debug("[tube::upload_to_landing_zone] exiting function")
  return(length(nb_unprocessed_files))
}
