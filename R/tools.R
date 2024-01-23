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
list_buckets <- function(type, credentials) {
  logger::log_debug("[pumpr::list_buckets] entering function")

  logger::log_debug("[pumpr::list_buckets] instanciating s3 client")
  s3_client <- paws.storage::s3(
    config = c(
      credentials, 
      close_connection = TRUE)
  )

  logger::log_debug("[pumpr::list_buckets] listing buckets")
  r <- s3_client$list_buckets()

  #TODO: error management if no bucket is returned

  logger::log_debug("[pumpr::list_buckets] wrangling result")
  list <- unlist(r$Buckets)
  datalake_list <- list[grep(type, list)]
  datalake_list <- as.list(as.data.frame(datalake_list))

  logger::log_debug("[pumpr::list_buckets] returning results")
  return(datalake_list)
}


list_athena_staging_bucket <- function(credentials) {
  logger::log_debug("[pumpr::list_datalakes] entering function")

  datalake_list <- list_buckets("athenaqueryresults", credentials)

  logger::log_debug("[pumpr::list_datalakes] returning results")
  return(datalake_list)
}