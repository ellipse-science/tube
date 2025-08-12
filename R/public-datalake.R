#' Returns the public datalake bucket name
#'
#' Technically the function returns all the buckets for which
#' the name contains the string `publicdatalakebucket`
#'
#' In our infrastructure, there is only one per AWS account (DEV/PROD)
#'
#' It is used mainly by the ellipse_connect function and related
#' discovery functions to interact with user-uploaded files
#' in the public datalake.
#'
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @returns A string containing the name of the public datalake bucket
list_public_datalake_bucket <- function(credentials) {
  logger::log_debug("[tube::list_public_datalake_bucket] entering function")

  public_datalake_list <- list_s3_buckets(credentials, "publicdatalakebucket")

  logger::log_debug("[tube::list_public_datalake_bucket] returning results")
  public_datalake_list
}

#' Returns the public datalake GLUE database name
#'
#' Technically the function returns all the databases of which
#' the name contains the string `publicdatalake` but in our
#' infrastructure, there is only one per AWS account (DEV/PROD)
#'
#' It is used mainly by the ellipse_connect function to retrieve the
#' schema with which to instantiate the DBI connection for the
#' public datalake containing user-uploaded files with metadata.
#'
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @returns A string containing the name of the public datalake database
list_public_datalake_database <- function(credentials) {
  logger::log_debug("[tube::list_public_datalake_database] entering function")

  public_datalake_database <- list_glue_databases(credentials, "publicdatalake")

  logger::log_debug("[tube::list_public_datalake_database] returning results")
  public_datalake_database
}
