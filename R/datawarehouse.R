#' @export
list_datawarehouses <- function() {

}

#' @export
get_datawarehouse_table <- function(credentials, datawarehouse_name, table_name, columns = NULL, filter = NULL) {
  logger::log_debug("[pumpr::get_datawarehouse_content] entering function")

  # TODO: checkmate parameters validations and error handling

  # TODO: checkmate parameters validations and error handling
  logger::log_debug("[pumpr::get_datalake_inventory] opening noctua athena DBI connection")
  if (exists("credentials") && length(credentials) > 0 && !is.null(credentials) && !is.na(credentials)) {
    con <- DBI::dbConnect(
      noctua::athena(),
      aws_access_key_id=Sys.getenv("AWS_ACCESS_KEY_ID"),
      aws_secret_access_key=Sys.getenv("AWS_SECRET_ACCESS_KEY"),
      s3_staging_dir=paste("s3:/", datawarehouse_name, table_name, sep="/"),
      region_name='ca-central-1'
    )
  } else {
    con <- DBI::dbConnect(
      noctua::athena(),
      s3_staging_dir=paste("s3:/", datawarehouse_name, table_name, sep="/"),
      region_name='ca-central-1'
    )
  }


  logger::log_debug("[pumpr::get_datawarehouse_content] building query")

  columns_string <- if (!is.null(unlist(columns))) paste(columns, collapse = ",") else "*"
  filter_string <- if (!is.null(unlist(filter))) {
    paste(
      "WHERE ",
      trimws(
        paste(
          if (length(filter$metadata)) paste(paste(names(filter$metadata), paste("'", filter$metadata, "'", sep=""), sep="=", collapse=" AND ")) else "",
          if (length(filter$metadata) && length(filter$data) > 0) "AND" else "",
          if (length(filter$data)) paste(paste(names(filter$data), paste("'", filter$data, "'", sep=""), sep="=", collapse=" AND ")) else "",
          sep = " "
        )
      )
    )
  } else {
    ""
  }

  query_string <- paste(
    "SELECT ", columns_string, 
    " FROM \"", datalake_name, "\".\"", table_name, "\"",
    filter_string, ";", sep = "")

  logger::log_debug(paste("[pumpr::get_datalake_content] query string is", query_string))
  logger::log_debug("[pumpr::get_datalake_content] executing query")

  res <- NULL

  res <- tryCatch(
    expr = { DBI::dbExecute(con, query_string) },
    error = function(e) {
      if (grepl("TABLE_NOT_FOUND", e$message)) {
        msg <- paste("[pumpr::get_datawarehouse_content] The table specified",
          data_source,
          "does not exist...  the dataframe returned is NULL"
        )
        logger::log_error(msg)
      } else {
        msg <- paste("[pumpr::get_datawarehouse_content] an error occurred: ", e$message)
        logger::log_error(msg)
      }
      return(NULL)
    },
    finally = {}
  )

  if (!is.null(res)) {
    df <- DBI::dbFetch(res)
    DBI::dbClearResult(res)

    if (nrow(df) == 0) {
      logger::log_warn("[pumpr::get_datawarehouse_content] The query was successful but the dataframe returned is empty.  Check the columns or the filter you sent to the function")
    }
  } else {
    logger::log_debug("[pumpr::get_datalake_content] setting null dataframe")
    df <- NULL  
  }

  logger::log_debug("[pumpr::get_datawarehouse_content] exiting function")
  return(df)
}

#' @export
put_datawarehouse_table <- function() {

}

#' @export 
update_datawarehouse_table <- function() {
}

#' @export 
get_datawarehouse_inventory <- function() {

}

#' @export 
refresh_datawarehouse_inventory <- function() {

}
