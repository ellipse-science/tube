#' Returns the datalake bucket name
#'
#' Technically the function returns all the buckets for which
#' the name contains the string `datalakebucket`
#' 
#' In our infrastructure, there is only one per AWS account (dev/prod)
#' 
#'
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @returns A string containing the name of the datalake bucket
list_datalake_bucket <- function(credentials) {
  logger::log_debug("[tube::list_datalakes] entering function")

  datalake_list <- list_s3_buckets(credentials, "datalakebucket")

  logger::log_debug("[tube::list_datalakes] returning results")
  return(datalake_list)
}
