#' Create an AWS session object
#'
#' @param id An AWS access key id
#' @param key An AWS secret access key
#'
#' @returns An AWS session object
#' @export
aws_session <- function(id = NULL, key = NULL) {
  if (is.null(id)) {
    id <- Sys.getenv("AWS_ACCESS_KEY_ID")
  }

  if (is.null(key)) {
    key <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
  }

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
    landing_zone_bucket = list_landing_zone_bucket(creds),
    datalake_bucket = list_datalake_bucket(creds),
    datawarehouse_bucket = list_datawarehouse_bucket(creds),
    datawarehouse_database = list_datawarehouse_database(creds),
    datamarts_bucket = list_datamarts_bucket(creds),
    datamarts_databases = list_datamarts_databases(creds),
    athena_staging_bucket = list_athena_staging_bucket(creds)
  )

  return(r)
}
