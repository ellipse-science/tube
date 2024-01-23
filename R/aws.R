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

  r <- list(
    credentials = creds,
    datalake = list_datalake_bucket(creds),
    datawarehouse = list_datawarehouse_bucket(creds),
    datamarts = list_datamarts_bucket(creds),
    athena_staging = list_athena_staging_bucket(creds)
  )

  return(r)
}
