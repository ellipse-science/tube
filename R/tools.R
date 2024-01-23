#' display the package version
#'
#' @param none
#'
#' @examples
#' \dontrun{
#'   v <- version()
#'   print(v)
#' }
#'
#' @export
version <- function() {
  return("0.0.1")
}



#' convert a url into a string
#'
#' @param url
#'
#' @examples
#' \dontrun{
#'   r <- convert_url_to_key(url)
#'   print(r)
#' }
#'
#' @export
convert_url_to_key <- function(url) {
  r <- gsub(" |-|:|/|\\.|&|\\?|=", "_", url)
  r <- gsub("https?___", "", r)
  r <- gsub("_$", "", r)
  return(r)
}



#' @export 
list_s3_buckets <- function(type, credentials) {
  logger::log_debug("[tube::list_s3_buckets] entering function")

  logger::log_debug("[tube::list_s3_buckets] instanciating s3 client")
  s3_client <- paws.storage::s3(
    config = c(
      credentials,
      close_connection = TRUE)
  )

  logger::log_debug("[tube::list_s3_buckets] listing buckets")
  r <- s3_client$list_buckets()

  #TODO: error management if no bucket is returned

  logger::log_debug("[tube::list_s3_buckets] wrangling result")
  list <- unlist(r$Buckets)
  bucket_list <- list[grep(type, list)]
  bucket_list <- as.list(bucket_list)
  names(bucket_list) <- ""
  bucket_list <- unlist(bucket_list)

  logger::log_debug("[tube::list_s3_buckets] returning results")
  return(bucket_list)
}



list_athena_staging_bucket <- function(credentials) {
  logger::log_debug("[tube::list_datalakes] entering function")

  datalake_list <- list_s3_buckets("athenaqueryresults", credentials)

  logger::log_debug("[tube::list_datalakes] returning results")
  return(datalake_list)
}



#' @export
list_glue_databases <- function(type, credentials) {
  logger::log_debug("[tube::list_glue_databases] entering function")

  logger::log_debug("[tube::list_glue_databases] instanciating glue client")
  glue_client <- paws.analytics::glue(
    credentials = credentials
  )

  logger::log_debug("[tube::list_glue_databases] listing databases")
  r <- glue_client$get_databases()

  logger::log_debug("[tube::list_s3_buckets] wrangling result")
  list <- unlist(r$DatabaseList)
  database_list <- list[grep(type, list)]
  database_list <- as.list(database_list)
  names(database_list) <- ""
  database_list <- unlist(database_list)

  return(database_list)
}