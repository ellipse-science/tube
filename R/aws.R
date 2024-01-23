#' @export 
aws_session <- function(id, key) {
  creds <- list(
    credentials = list(
      creds = list(
        access_key_id = id,
        secret_access_key = key
      )
    )
  )

  r <- list()

  r$credentials <- creds
  r$datalake <- list_datalake_bucket(creds)
  r$datawarehouse <- list_datawarehouse_bucket(creds)
  r$datamarts <- list_datamarts_bucket(creds)
  r$athena_staging <- list_athena_staging_bucket(creds)

  return(r)
}
