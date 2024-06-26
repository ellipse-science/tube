list_athena_staging_bucket <- function(credentials) {
  logger::log_debug("[tube::list_athena_staging_bucket] entering function")

  datalake_list <- list_s3_buckets("athenaqueryresults", credentials)

  logger::log_debug("[tube::list_athena_staging_bucket] returning results")
  return(datalake_list)
}
