# Checks and returns AWS credentials to be used by other
# non-exported functions in this package
#
get_aws_credentials <- function() {
  logger::log_debug("[get_aws_credentials] entering function")

  aws_access_key_id <- Sys.getenv("AWS_ACCESS_KEY_ID")
  aws_secret_access_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY")

  if (aws_access_key_id == "" || aws_secret_access_key == "") {
    usage <-
      paste("Nous n'avons pas trouvé vos clés d'accès AWS\n\n",
            "N'oubliez pas de vous connecter avec tube::ellipse_connect()\n\n")
    cli::cli_alert_danger(usage)
    logger::log_error("[get_aws_credentials] missing aws credentials in env variables")
    return(NULL)
  }

  creds <- list(
    credentials = list(
      creds = list(
        access_key_id = aws_access_key_id,
        secret_access_key = aws_secret_access_key,
        session_token = Sys.getenv("AWS_SESSION_TOKEN")
      )
    )
  )

  tryCatch({
    paws.storage::s3(
      config = c(
        creds,
        close_connection = TRUE)
    )$list_buckets()
  }, error = function(e) {
    cli::cli_alert_danger("Oups, il semble que vos clés d'accès ne sont pas valides! 😅")
    logger::log_error("[get_aws_credentials] invalid aws credentials")
    return(NULL)
  })

  logger::log_info("[get_aws_credentials] successful connection to aws")
  return(creds)
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

list_athena_staging_bucket <- function(credentials) {
  logger::log_debug("[tube::list_athena_staging_bucket] entering function")

  datalake_list <- list_s3_buckets("athenaqueryresults", credentials)

  logger::log_debug("[tube::list_athena_staging_bucket] returning results")
  return(datalake_list)
}