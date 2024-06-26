# Returns the datamart bucket name
#
list_datamarts_bucket <- function(credentials) {
  logger::log_debug("[tube::list_datalakes] entering function")

  datalake_list <- list_s3_buckets("datamartbucket", credentials)

  logger::log_debug("[tube::list_datalakes] returning results")
  return(datalake_list)
}

put_datamart_table <- function(credentials, datamart_name, table_name, dataframe) {
  logger::log_debug("[tube::put_datamart_table] entering function")

  # TODO: checkmate parameters validations and error handling

  logger::log_debug("[tube::put_datamart_table] instanciating s3 client")
  s3_client <- paws.storage::s3(
    config = c(
      credentials, 
      close_connection = TRUE)
  )

  filename <- tempfile()

  arrow::write_parquet(dataframe, filename)

  s3_client$put_object(
    Bucket = datamart_name,
    Body = filename,
    Key = paste(table_name, paste(table_name, format(Sys.time(), format="%Y-%m-%d-%H:%M"), ".parquet", sep=""), sep="/")
  )  
  
}