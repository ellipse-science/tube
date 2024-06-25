# Returns the datawarehouse bucket
#
list_datawarehouse_bucket <- function(credentials) {
  logger::log_debug("[tube::list_datalakes] entering function")

  datalake_list <- list_s3_buckets("datawarehousebucket", credentials)

  logger::log_debug("[tube::list_datalakes] returning results")
  return(datalake_list)
}

# Returns the database name of the datawarehouse
list_datawarehouse_database <- function(credentials) {
  logger::log_debug("[tube::list_datawarehouse_database] entering function")

  datawarehouse_database <- list_glue_databases("datawarehouse", credentials)

  logger::log_debug("[tube::list_datawarehouse_database] returning results")
  return(datawarehouse_database)
}

# Returns the list of tables in the datawarehouse
list_datawarehouse_tables <- function(credentials, simplify = TRUE) {
  logger::log_debug("[tube::list_datawarehouse_tables] entering function")

  datawarehouse_database <- list_glue_tables(type = "datawarehouse",
                                             credentials = credentials)

  logger::log_debug("[tube::list_datawarehouse_tables] returning results")

  if (simplify) return(glue_table_list_to_tibble(datawarehouse_database))
  return(datawarehouse_database)
}

#' Convert a list of tables from the AWS Glue API
#'
#' The Glue API returns a complex `JSON` response when the
#' `list_database_tables` method is called. There is simply too much nested
#' information for a data scientist to parse through to get to the information
#' they need.
#'
#' This function converts the response into a human readable `tibble` containing
#' only the most useful information, namely the table names, their respective
#' columns and column types, as well which columns are partitionned in the
#' data warehouse.
#'
#' @param glue_response A list of 2 elements, the first of which is named
#'   `TableList`. What is expected here is the output from
#'   `tube::list_datawarehouse_tables()`
#' @returns A tibble with columns:
#'
#'   * `table_name` : Name of the table in the data warehouse
#'   * `col_name` : Name of the column
#'   * `col_type` : Data type of the column
#'   * `is_partition` : Logical indicating wether or not the column is
#'      partitionned
glue_table_list_to_tibble <- function(glue_response) {
  df <- tibble::tibble(table_name = character(),
                       col_name = character(),
                       col_type = character(),
                       is_partition = logical())

  table_names <- purrr::map(glue_response[[1]], \(x) x$Name) |> unlist()

  for (i in seq_along(table_names)) {
    # Partitions and regulars columns are not together in the response
    partitions <- glue_response[[1]][[i]]$PartitionKeys
    col_names  <- purrr::map(partitions, \(x) x$Name) |> unlist()
    col_types  <- purrr::map(partitions, \(x) x$Type) |> unlist()

    parts <- tibble::tibble(table_name = table_names[i],
                            col_name = col_names,
                            col_type = col_types,
                            is_partition = TRUE)

    columns   <- glue_response[[1]][[i]]$StorageDescriptor$Columns
    col_names <- purrr::map(columns, \(x) x$Name) |> unlist()
    col_types <- purrr::map(columns, \(x) x$Type) |> unlist()

    cols <- tibble::tibble(table_name = table_names[i],
                           col_name = col_names,
                           col_type = col_types,
                           is_partition = FALSE)

    df <- dplyr::bind_rows(df, parts, cols)
  }

  return(df)
}