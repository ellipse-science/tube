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
    datalake_bucket = list_datalake_bucket(creds),
    datawarehouse_bucket = list_datawarehouse_bucket(creds),
    datawarehouse_database = list_datawarehouse_database(creds),
    datamarts_bucket = list_datamarts_bucket(creds),
    datamarts_databases = list_datamarts_databases(creds),
    athena_staging_bucket = list_athena_staging_bucket(creds)
  )

  return(r)
}
