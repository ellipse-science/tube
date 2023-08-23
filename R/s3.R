#' commits an R object to an S3 bucket
#' @param object
#' @param metadata
#' @param objectname
#' @param path
#' @param object_ext
#' @param bucket
#' @param refresh_data
#'
#' @examples
#' \dontrun{
#'   # pute some sample code here as an example
#' }
#'
#' @export
commit_r_object_to_datalake <- function(aws_client, bucket, object, metadata, objectname, base_path, object_ext, keep_history, history_schema, refresh_data) {
  logger::log_debug("[pumpr::commit_r_object_to_datalake] entering function")
  logger::log_info("[pumpr::commit_r_object_to_datalake] committing object to datalake")

  # TODO: checkmate parameters validations and error handling
  
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
      history_schema == "YYYY/MM/DD/HH" ~ format(Sys.time(), format="%Y/%m/%d/%H/%M"),
      history_schema == "YYYY/WEEKNUM" ~ format(Sys.time(), format="%Y/%W"),      
    )

    base_path <- paste(base_path, partition_prefix, sep="/")

  }

  # build json object
  json_object <- jsonlite::toJSON(
    list(
      metadata = c(
        metadata,
        format = object_ext
      ),
      data = object
    ),
    auto_unbox = T
  )

  # we're in lambda so we'll use a temporary fildsystem
  td <- tempdir()
  filename <- paste(objectname, object_ext, sep = ".")

  # todo : detect object type (df, list, character etc) and write accordingly
  write(json_object, file.path(td, filename))


  # put the object in s3 bucket 
  aws_client$put_object(
    Bucket = bucket,
    Body = file.path(td, filename),
    Key = paste(base_path,objectname,sep="/"),
    #Tagging = URLencode(paste(names(tags), tags, collapse="&", sep="="))
  )  

  #TODO : Error management

  # aws.s3::put_object(
  #   file = file.path(td, filename), 
  #   object = paste(path,objectname,sep="/"),
  #   bucket = bucket,
  #   headers = metadata
  # )

  logger::log_debug("[pumpr::commit_r_object_to_datalake] exiting function")
}



#' retrieves a list of R object from an S3 bucket
#' @param bucket
#' @param metadata
#' @param objectname
#' @param path
#' @param object_ext
#' @param bucket
#' @param refresh_data
#'
#' @examples
#' \dontrun{
#'   # pute some sample code here as an example
#' }
#'
#' @export
get_datalake_inventory <- function(aws_client, bucket, path, tags_filter) {
  logger::log_debug("[pumpr::get_datalake_inventory] entering function")
  logger::log_info("[pumpr::get_datalake_inventory] listing objects from datalake")

  # TODO: checkmate parameters validations and error handling
  
  #names(metadata_filter) <- paste("x-amz-meta-", names(metadata_filter), sep = "")

  r <- aws_client$list_objects_v2(
    Bucket = bucket, 
    Prefix = paste(path, "/", sep=""),
    Delimiter = "/")

  r$Contents[[1]] <- NULL

  l <- list()

  logger::log_info("[pumpr::get_datalake_inventory] filtering objects in datalake")

  for (o in r$Contents) {
    t <- aws_client$get_object_tagging(bucket, o$Key)
    t$VersionId <- NULL
    df <- as.data.frame(sapply((sapply(t, unname)), unname))
    names(df) <- unlist(df[1,])
    df <- df[-c(1),]
    l1 <- lapply(as.list(df), unlist)
    int <- intersect(names(l1), names(tags_filter))
    l2 <- l1[int]
    if (FALSE %in% (l2 %in% tags_filter == names(l2) %in% names(tags_filter))) next
    l[length(l)+1] <-list(l1)
  }

  # r <- aws.s3::get_bucket(
  #   bucket = bucket,
  #   prefix = paste(path, "/", sep=""),
  #   delimiter = "/",
  #   headers =  metadata_filter
  # )


  logger::log_debug("[pumpr::commit_r_object_to_datalake] exiting function")
  return(l)
}

