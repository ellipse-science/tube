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

  logger::log_debug("[get_aws_credentials] successful connection to aws")
  return(creds)
}

