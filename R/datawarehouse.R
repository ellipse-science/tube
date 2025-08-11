#' Returns the datawarehouse bucket name
#'
#' Technically the function returns all the buckets for which
#' the name contains the string `datawarehousebucket` but in our
#' infrastructure, there is only one per AWS account (DEV/PROD)
#'
#' Currently the function is not used in the package but it is
#' kept for future use
#'
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @returns A string containing the name of the datamart bucket
list_datawarehouse_bucket <- function(credentials) {
  logger::log_debug("[tube::list_datawarehouse_bucket] entering function")

  datalake_list <- list_s3_buckets(credentials, "datawarehousebucket")

  logger::log_debug("[tube::list_datawarehouse_bucket] returning results")
  return(datalake_list)
}

#' Returns the datawarehouse GLUE database name
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
#' @returns A string containing the name of the datawarehouse database
list_datawarehouse_database <- function(credentials) {
  logger::log_debug("[tube::list_datawarehouse_database] entering function")

  datawarehouse_database <- list_glue_databases(credentials, "datawarehouse")

  logger::log_debug("[tube::list_datawarehouse_database] returning results")
  return(datawarehouse_database)
}

#' Returns the datawarehouse GLUE tables names
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
list_datawarehouse_tables <- function(credentials, simplify = TRUE) {
  logger::log_debug("[tube::list_datawarehouse_tables] entering function")

  # Get the actual database name dynamically instead of hardcoding "datawarehouse"
  database_name <- list_datawarehouse_database(credentials)
  if (is.null(database_name) || length(database_name) == 0) {
    logger::log_warn("[tube::list_datawarehouse_tables] No datawarehouse database found")
    return(NULL)
  }
  
  # Use the first database if multiple are returned
  database_name <- database_name[1]
  
  # Get raw data (simplify = FALSE) so we can handle simplification here
  datawarehouse_database <- list_glue_tables(credentials, database_name, NULL, simplify = FALSE)

  logger::log_debug("[tube::list_datawarehouse_tables] returning results")

  if (simplify) {
    return(glue_table_list_to_tibble(datawarehouse_database))
  }
  return(datawarehouse_database)
}
