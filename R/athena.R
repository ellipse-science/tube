#' Returns the Athena staging bucket name
#'
#' Technically the function returns all the buckets for which
#' the name contains the string `athenaqueryresults`
#' In our infrastructure, there is only one per AWS account (DEV/PROD)
#' 
#' This is used to get the name of the bucket where the results of the
#' athena queries are stored.  More specifically, it is used to create
#' the `s3_staging_dir` parameter in the `DBI::dbConnect` function call
#' in ellipse_connect
#'
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @returns A string containing the name of the Athena staging bucket
list_athena_staging_bucket <- function(credentials) {
  logger::log_debug("[tube::list_athena_staging_bucket] entering function")

  datalake_list <- list_s3_buckets(credentials, "athenaqueryresults")

  logger::log_debug("[tube::list_athena_staging_bucket] returning results")
  return(datalake_list)
}
