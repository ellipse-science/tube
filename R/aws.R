#' Returns the AWS credentials in a format that is
#' compliant with the paws functions calls
#' 
#' It is used to get the credentials from the environment variables
#' set in ~/.Renviron in the following format:
#' 
#' #dev
#' AWS_ACCESS_KEY_ID_DEV=<the access key id for the DEV account>
#' AWS_SECRET_ACCESS_KEY_DEV=<the secret access key for the DEV account >
#' #prod
#' AWS_ACCESS_KEY_ID_PROD=<the access key id for the PROD account>
#' AWS_SECRET_ACCESS_KEY_PROD=<the access key id for the PROD account>
#' 
#' The function checks if the credentials are valid by trying to list the buckets
#' in the account. If the credentials are not valid, the function returns NULL
#' and displays an error message.
#' 
#' The value returned must be passed to all functions that use paws functions
#'
#' @param env The environnement ("DEV" or "PROD")
#' @returns a list structure compliant with the paws functions calls containing the AWS credentials
#' for the specified environment
get_aws_credentials <- function(env) {
  logger::log_debug("[get_aws_credentials] entering function")

  # Check if the environment is valid
  if (!check_env(env)) {
    cli::cli_alert_danger("Oups, l'environnement que vous avez spécifié n'est pas valide! 😅")
    logger::log_error("[get_aws_credentials] invalid environment")
    return(NULL)
  }

  aws_access_key_id <- Sys.getenv(paste0("AWS_ACCESS_KEY_ID_", env))
  aws_secret_access_key <- Sys.getenv(paste0("AWS_SECRET_ACCESS_KEY_", env))

  if (aws_access_key_id == "" || aws_secret_access_key == "") {
    usage <-
      paste(
        "Nous n'avons pas trouvé vos clés d'accès AWS\n\n",
        "N'oubliez pas de vous connecter avec tube::ellipse_connect()\n\n")
    cli::cli_alert_danger(usage)
    logger::log_error("[get_aws_credentials] missing aws credentials in env variables")
    return(NULL)
  }

  creds <- list(
    credentials = list(
      creds = list(
        access_key_id = aws_access_key_id,
        secret_access_key = aws_secret_access_key
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
