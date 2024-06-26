# Returns the datalake bucket name
#
list_datalake_bucket <- function(credentials) {
  logger::log_debug("[tube::list_datalakes] entering function")

  datalake_list <- list_s3_buckets("datalakebucket", credentials)

  logger::log_debug("[tube::list_datalakes] returning results")
  return(datalake_list)
}
