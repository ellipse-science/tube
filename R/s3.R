#' commits a dataframe to an S3 object
#' @param object
#' @param metadata
#' @param objectname
#' @param object_ext
#' @param bucket
#'
#' @examples
#' \dontrun{
#'   # pute some sample code here as an example
#' }
#'
#' @export
commit_r_object_to_datalake <- function(object, metadata, objectname, path, object_ext, bucket, refresh_data) {
  logger::log_info("committing object to datalake")
  
  td <- tempdir()
  filename <- paste(objectname, object_ext, sep = ".")

  # todo : detect object type (df, list, character etc) and write accordingly
  write(object, file.path(td, filename))

  names(metadata) <- paste("x-amz-meta-", names(metadata), sep = "")
  
  aws.s3::put_object(
    file = file.path(td, filename), 
    object = paste(path,objectname,sep="/"),
    bucket = bucket,
    headers = metadata,
    verbose = TRUE
  )

}