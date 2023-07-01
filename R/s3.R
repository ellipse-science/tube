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
commit_r_object_to_datalake <- function(aws_client, bucket, object, tags, objectname, path, object_ext, refresh_data) {
  logger::log_debug("[pump::commit_r_object_to_datalake] entering function")
  logger::log_info("[pump::commit_r_object_to_datalake] committing object to datalake")

  # TODO: checkmate parameters validations and error handling
  
  td <- tempdir()
  filename <- paste(objectname, object_ext, sep = ".")

  # todo : detect object type (df, list, character etc) and write accordingly
  write(object, file.path(td, filename))

  #names(metadata) <- paste("x-amz-meta-", names(metadata), sep = "")
  aws_client$put_object(
    Bucket = bucket,
    Body = file.path(td, filename),
    Key = paste(path,objectname,sep="/"),
    Tagging = URLencode(paste(names(tags), tags, collapse="&", sep="="))
  )  

  #TODO : Error management

  # aws.s3::put_object(
  #   file = file.path(td, filename), 
  #   object = paste(path,objectname,sep="/"),
  #   bucket = bucket,
  #   headers = metadata
  # )

  logger::log_debug("[pump::commit_r_object_to_datalake] exiting function")
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
  logger::log_debug("[pump::get_datalake_inventory] entering function")
  logger::log_info("[pump::get_datalake_inventory] listing object from datalake")

  # TODO: checkmate parameters validations and error handling
  
  #names(metadata_filter) <- paste("x-amz-meta-", names(metadata_filter), sep = "")

  r <- aws_client$list_objects_v2(
    Bucket = bucket, 
    Prefix = paste(path, "/", sep=""),
    Delimiter = "/")

  r$Contents[[1]] <- NULL

  l <- list()

  for (o in r$Contents) {
    t <- s3_client$get_object_tagging(datalake_bucket, o$Key)
    t$VersionId <- NULL
    df <- as.data.frame(sapply((sapply(t, unname)), unname))
    names(df) <- unlist(df[1,])
    df <- df[-c(1),]
    l1 <- lapply(as.list(df), unlist)
    int <- intersect(names(l1), names(tags_filter))
    l2 <- l1[int]
    if (FALSE %in% (l2 %in% tags_filter == names(l2) %in% names(tags_filters))) next
    l[length(l)+1] <-list(l1)
  }

  # r <- aws.s3::get_bucket(
  #   bucket = bucket,
  #   prefix = paste(path, "/", sep=""),
  #   delimiter = "/",
  #   headers =  metadata_filter
  # )


  logger::log_debug("[pump::commit_r_object_to_datalake] exiting function")
  return(l)
}

