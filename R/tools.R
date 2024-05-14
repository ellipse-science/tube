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


#' @export
list_athena_staging_bucket <- function(credentials) {
  logger::log_debug("[tube::list_athena_staging_bucket] entering function")

  datalake_list <- list_s3_buckets("athenaqueryresults", credentials)

  logger::log_debug("[tube::list_athena_staging_bucket] returning results")
  return(datalake_list)
}


list_glue_databases <- function(type, credentials) {
  logger::log_debug("[tube::list_glue_databases] entering function")

  logger::log_debug("[tube::list_glue_databases] instanciating glue client")
  glue_client <- paws.analytics::glue(
    credentials = credentials
  )

  logger::log_debug("[tube::list_glue_databases] listing databases")
  r <- glue_client$get_databases()

  logger::log_debug("[tube::list_glue_databases] wrangling result")
  list <- unlist(r$DatabaseList)
  database_list <- list[grep(type, list)]
  database_list <- as.list(database_list)
  if (length(database_list) == 0) {
    return(NULL)
  }
  names(database_list) <- ""
  database_list <- unlist(database_list)

  return(database_list)
}


list_glue_tables <- function(type, datamart = NULL, credentials) {  
  logger::log_debug("[tube::list_glue_tables] entering function")
  table_list <- list()

  logger::log_debug("[tube::list_glue_tables] instanciating glue client")
  glue_client <- paws.analytics::glue(
    credentials = credentials
  )

  if (is.null(datamart) && type == "datawarehouse") {
    logger::log_info("[tube::list_glue_tables] listing tables from the datawarehouse")
    dwh_db <- list_glue_databases("datawarehouse", credentials)
    if (is.null(dwh_db)) {
      logger::log_error("[tube::list_glue_tables] no datawarehouse database found")
      return(NULL)
    }
    r <- glue_client$get_tables("", dwh_db)
  } else {
    if (is.null(datamart) && type == "datamart") {
      logger::log_error("[tube::list_glue_tables] datamart type provided, but no datamart name provided")
      return(NULL)
    } else {
      logger::log_info("[tube::list_glue_tables] listing tables from the datamart")
      r <- glue_client$get_tables("", datamart)
    }
  }

  if (length(r) == 0) {
    return(NULL)
  }

  # For now just return the full unprocessed list
  # TODO: add some processing to make it easier for researchers
  #       to parse
  table_list <- r

  return(table_list)
}
