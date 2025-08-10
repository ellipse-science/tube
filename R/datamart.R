#' Returns the datamarts bucket name
#'
#' Technically the function returns all the buckets for which
#' the name contains the string `datamartbucket`
#'
#' In our infrastructure, there is only one per AWS account (DEV/PROD)
#'
#' It is used mainly by the ellipse_publish function to store R dataframes
#' in a datamart in the form of CSV files, which in turn, are converted into
#' GLUE tables by the glue job.
#'
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @returns A string containing the name of the datamart bucket
list_datamarts_bucket <- function(credentials) {
  logger::log_debug("[tube::list_datamarts_bucket] entering function")

  datamart_list <- list_s3_buckets(credentials, "datamartbucket")

  logger::log_debug("[tube::list_datamarts_bucket] returning results")
  return(datamart_list)
}

#' Returns the datamarts GLUE database name
#'
#' Technically the function returns all the databases of which
#' the name contains the string `datamart` but in our
#' infrastructure, there is only one per AWS account (DEV/PROD)
#'
#' It is used mainly by the ellipse_connect function to retrieve the
#' schema with wich to instanciate the DBI connection
#'
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @returns A string containing the name the datamarts database
list_datamarts_database <- function(credentials) {
  logger::log_debug("[tube::list_datamarts_database] entering function")

  datamart_database <- list_glue_databases(credentials, "datamart")

  logger::log_debug("[tube::list_datamarts_database] returning results")
  return(datamart_database)
}

#' Returns the datamarts GLUE tables names
#'
#' It is currently not used as the ellipse_discover function
#' does the job through the list_glue_tables function
#' We're keeping it for now just in case we need it later
#'
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @returns If simplify = true it returns a tibble with columns:
#'
#'   * `table_name` : Name of the table in the data warehouse
#'   * `col_name` : Name of the column
#'   * `col_type` : Data type of the column
#'   * `is_partition` : Logical indicating wether or not the column is
#'      partitionned
#' If simplify = false it returns the raw output from the AWS Glue API
list_datamart_tables <- function(credentials, datamart_name, simplify = TRUE) {
  logger::log_debug("[tube::list_datamart_tables] entering function")

  # datamart_tables <- list_glue_tables(type = "datamart", datamart = datamart_name,
  #                                           credentials = credentials)

  logger::log_debug("[tube::list_datamart_tables] returning results")

  # if (simplify) return(glue_table_list_to_tibble(datamart_tables))
  # return(datamart_tables)
  return(NULL)
}

upload_dataframe_to_datamart <- function(credentials, dataframe, bucket, prefix, partition) {
  logger::log_debug("[tube::upload_dataframe_to_datamart] entering function")

  # Convert the dataframe to a CSV file by appending the variable type to the column header
  # such as: `column_name:variable_type`
  # Convert the column names of the dataframe to include the variable type
  # supported types are: string, int and date
  col_names <- colnames(dataframe)
  for (i in 1:length(col_names)) {
    dataframe_col_type <- get_column_type(dataframe[[i]])
    glue_col_type <- switch(dataframe_col_type,
      "character" = "string",
      "integer" = "int",
      "decimal" = "double",
      "date" = "date",
      FALSE
    )

    if (glue_col_type == FALSE) {
      return("Unsupported column type")
    }

    col_names[i] <- paste(col_names[i], ":", glue_col_type, sep = "")
  }

  colnames(dataframe) <- tolower(col_names)
  csv_file <- tempfile(fileext = ".csv")
  write.csv(dataframe, csv_file, row.names = FALSE)

  # Instantiate the S3 client
  s3_client <- paws.storage::s3(
    config = c(
      credentials,
      close_connection = TRUE
    )
  )

  current_time <- Sys.time()
  current_time_utc <- as.POSIXct(current_time, tz = "UTC")
  formatted_time <- format(current_time_utc, "%Y-%m-%dT%H:%M:%OS3Z")

  # `prefix/partition/data-2024-07-02T15:10:52.645Z.csv`
  s3_key <- paste(prefix, "/", partition, "/unprocessed/data-", formatted_time, ".csv", sep = "")

  r <- upload_file_to_s3(credentials, csv_file, bucket, s3_key)

  logger::log_debug("[tube::upload_dataframe_to_datamart] returning results")
  return(r)
}
