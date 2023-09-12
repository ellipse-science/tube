#' commits an R object to an S3 bucket
#' @param aws_client
#' @param bucket
#' @param metadata
#' @param object
#' @param objectname
#' @param base_path
#' @param keep_histrory
#' @param history_schema
#' @param refresh_data
#'
#' @examples
#' \dontrun{
#'   # pute some sample code here as an example
#' }
#'
#' @export
commit_r_object_to_datalake <- function(aws_client, bucket, metadata, object, objectname, base_path, keep_history, history_schema, refresh_data) {
  logger::log_debug("[pumpr::commit_r_object_to_datalake] entering function")
  logger::log_info("[pumpr::commit_r_object_to_datalake] committing object to datalake")

  # TODO: checkmate parameters validations and error handling
  checkmate::assertChoice(metadata$format, c("pdf", "html", "xml", "json", "docx", "xlsx", "csv"))
  
  # figure our path (s3 prefix) based on whether we have to keep_history or not
  if (keep_history) {
    # we'll handle the history_granularity later
    # for now if keey history, then we partition the path based on partition schema
    # Partition schemas:
    # YYYY
    # YYYY/MM
    # YYYY/MM/DD
    # YYYY/MM/DD/HH
    # YYYY/WEEKNUM 

    # compute the partition prefix
    partition_prefix <- dplyr::case_when(
      history_schema == "YYYY" ~ format(Sys.Date(), format="%Y"),
      history_schema == "YYYY/MM" ~ format(Sys.Date(), format="%Y/%m"),
      history_schema == "YYYY/MM/DD" ~ format(Sys.Date(), format="%Y/%m/%d"),
      history_schema == "YYYY/MM/DD/HH" ~ format(Sys.time(), format="%Y/%m/%d/%H"),
      history_schema == "YYYY/WEEKNUM" ~ format(Sys.time(), format="%Y/%W"),      
    )

    base_path <- paste(base_path, partition_prefix, sep="/")

  }

  #split_metadata <- paste(paste("metadata", names(metadata), sep="."), metadata, collapse = ",", sep = ":")
  #ist(split_metadata)

  names(metadata) <- paste("metadata", names(metadata), sep = ".")

  # we're in lambda so we'll use a temporary fildsystem
  td <- tempdir()
  filename <- paste(objectname, "json", sep = ".")

  # build json object
  json_object <- jsonlite::toJSON(
    c(
      key = gsub(".json$", "", paste(base_path,filename,sep="/")),
      metadata,
      data = object
    ),
    auto_unbox = T
  )

  # todo : detect object type (df, list, character etc) and write accordingly
  write(json_object, file.path(td, filename))

  # put the object in s3 bucket 
  aws_client$put_object(
    Bucket = bucket,
    Body = file.path(td, filename),
    Key = paste(base_path,filename,sep="/"),
    ContentType = "application/json; charset=utf-8"
  )  

  #TODO : Error management

  logger::log_debug("[pumpr::commit_r_object_to_datalake] exiting function")
}





#' retreieves and returns an R object to an S3 bucket
#' @param aws_client
#' @param bucket
#' @param objectname
#' @param base_path
#' @param history_version
#'
#' @examples
#' \dontrun{
#'   # pute some sample code here as an example
#' }
#'
#' @export
get_r_object_from_datalake <- function(aws_client, bucket, base_path, objectname, history_version = "") {
  logger::log_debug("[pumpr::get_r_object_from_datalake] entering function")
  logger::log_info(
    paste(
      "[pumpr::get_r_object_from_datalake] retrieving object",
      paste(base_path, objectname, sep="/"),
      "from bucket",
      bucket
    )
  )

  # TODO: checkmate parameters validations and error handling

  # put the object in s3 bucket 
  # TODO: mettre la gestion des erreur autour de ce code
  object <- aws_client$get_object( 
    Bucket = bucket,
    Key = paste(base_path, objectname, sep="/")
  )

  
  logger::log_debug("[pumpr::commit_r_object_to_datalake] exiting function and returning rawToChar object")

  return(object$Body %>%  rawToChar)

  #TODO : Error management

  # aws.s3::put_object(
  #   file = file.path(td, filename), 
  #   object = paste(path,objectname,sep="/"),
  #   bucket = bucket,
  #   headers = metadata
  # )

}




#' retrieves a list of R object from an S3 bucket through GLUE
#' @param aws_client
#' @param bucket
#' @param path
#' @param metadata_filter
#'
#' @examples
#' \dontrun{
#'   # pute some sample code here as an example
#' }
#'
#' @export
get_datalake_content <- function(aws_client, bucket, path, metadata_filter, history_filter) {
  logger::log_debug("[pumpr::get_datalake_inventory] entering function")

  # TODO: checkmate parameters validations and error handling

  con <- DBI::dbConnect(
    noctua::athena(),
    aws_access_key_id=Sys.getenv("AWS_ACCESS_KEY_ID"),
    aws_secret_access_key=Sys.getenv("AWS_SECRET_ACCESS_KEY"),
    s3_staging_dir='s3://ellipse-datalake/',
    region_name='ca-central-1'
  )

  con <- DBI::dbConnect(
    noctua::athena(),
    s3_staging_dir='s3://ellipse-datalake/',
    region_name='ca-central-1'
  )

  logger::log_debug(paste("[", scriptname, "] listing tables", sep = ""))
  DBI::dbListTables(con)

  logger::log_debug(paste("[", scriptname, "] executing query", sep = ""))
  res <- DBI::dbExecute(con, "SELECT * FROM \"datalake-agora\".\"a_qc_press_releases\";")

  df <- DBI::dbFetch(res)
  DBI::dbClearResult(res)
  
  logger::log_debug("[pumpr::commit_r_object_to_datalake] exiting function")
  return(l)
}

