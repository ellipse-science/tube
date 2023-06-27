#' commits a dataframe to an S3 object
#' @param df
#' @param metadata
#' @param bucket
#'
#' @examples
#' \dontrun{
#'   # pute some sample code here as an example
#' }
#'
#' @export
commit_df_to_datalake <- function(df, metadata, objectname, bucket) {
  logger::log_info("committing df to datalake")
  
  td <- tempdir()
  write.csv(df, file.path(td, "object.csv"))
  aws.s3::put_object(
    file = file.path(td, "object.csv"), 
    object = objectname, 
    bucket = "ellipse-datalake"
  )
}