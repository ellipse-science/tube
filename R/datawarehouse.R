#' @export
list_datawarehouse_bucket <- function(credentials) {
  logger::log_debug("[tube::list_datalakes] entering function")

  datalake_list <- list_s3_buckets("datawarehouse", credentials)

  logger::log_debug("[tube::list_datalakes] returning results")
  return(datalake_list)
}


#' @export
list_datawarehouse_database <- function(credentials) {
  logger::log_debug("[tube::list_datawarehouse_database] entering function")

  datawarehouse_database <- list_glue_databases("datawarehouse", credentials)

  logger::log_debug("[tube::list_datawarehouse_database] returning results")
  return(datawarehouse_database)
}


#' @export
list_datawarehouse_tables <- function(credentials) {
  logger::log_debug("[tube::list_datawarehouse_tables] entering function")

  datawarehouse_database <- list_glue_tables(type = "datawarehouse", credentials = credentials)

  logger::log_debug("[tube::list_datawarehouse_tables] returning results")
  return(datawarehouse_database)
}



#' @export
get_datawarehouse_table <- function(session, table_name, columns = NULL, filter = NULL) {
  logger::log_debug("[tube::get_datawarehouse_table] entering function")

  # TODO: checkmate parameters validations and error handling

  # TODO: checkmate parameters validations and error handling
  logger::log_debug("[tube::get_datawarehouse_table] opening noctua athena DBI connection")
  if (exists("credentials") && length(session$credentials) > 0 && !is.null(session$credentials) && !is.na(session$credentials)) {
    con <- DBI::dbConnect(
      noctua::athena(),
      aws_access_key_id=Sys.getenv("AWS_ACCESS_KEY_ID"),
      aws_secret_access_key=Sys.getenv("AWS_SECRET_ACCESS_KEY"),
      s3_staging_dir=paste("s3:/", session$athena_staging_bucket, table_name, sep="/"),
      region_name='ca-central-1'
    )
  } else {
    con <- DBI::dbConnect(
      noctua::athena(),
      s3_staging_dir=paste("s3:/", session$athena_staging_bucket, table_name, sep="/"),
      region_name='ca-central-1'
    )
  }


  logger::log_debug("[tube::get_datawarehouse_table] building query")

  columns_string <- if (!is.null(unlist(columns))) paste(columns, collapse = ",") else "*"

  if (typeof(filter) == "list") {
    filter_string <- if (!is.null(unlist(filter))) {
      paste(
        "WHERE ",
        trimws(
          paste(
            if (length(filter$metadata))
              paste(paste(names(filter$metadata), paste("'", filter$metadata, "'", sep = ""), sep = "=", collapse = " AND ")) else "",
            if (length(filter$metadata) && length(filter$data) > 0)
              "AND" else "",
            if (length(filter$data))
              paste(paste(names(filter$data), paste("'", filter$data, "'", sep=""), sep="=", collapse=" AND ")) else "",
            sep = " "
          )
        )
      )
    } else {
      ""
    }
  } else {
    if (typeof(filter) == "character") {
      filter_string <- if (nchar(filter) > 0) paste("WHERE", filter) else ""
    } else {
      msg <- "[tube::get_datawarehouse_table] please use a list with variable = 'value' format or a string with SQL syntax for the filter"
      rlang::abort(msg)
    }
  }

  query_string <- paste(
    "SELECT ", columns_string,
    " FROM \"", session$datawarehouse_database, "\".\"", table_name, "\"",
    filter_string, ";", sep = "")

  logger::log_debug(paste("[tube::get_datawarehouse_table] query string is", query_string))
  logger::log_debug("[tube::get_datawarehouse_table] executing query")

  res <- NULL

  res <- tryCatch(
    expr = { DBI::dbExecute(con, query_string) },
    error = function(e) {
      if (grepl("TABLE_NOT_FOUND", e$message)) {
        msg <- paste("[tube::get_datawarehouse_table] The table specified",
          table_name,
          "does not exist...  the dataframe returned is NULL"
        )
        logger::log_error(msg)
      } else {
        msg <- paste("[tube::get_datawarehouse_table] an error occurred: ", e$message)
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
      logger::log_warn(
        "[tube::get_datawarehouse_table] The query was successful but the dataframe returned is empty.\
         Check the columns or the filter you sent to the function."
      )
    }
  } else {
    logger::log_debug("[tube::get_datawarehouse_table] setting null dataframe")
    df <- NULL
  }

  logger::log_debug("[tube::get_datawarehouse_table] exiting function")
  return(df)
}


#' @export
put_datawarehouse_table <- function(session, table_name, dataframe) {
  logger::log_debug("[tube::put_datawarehouse_table] entering function")

  # TODO: checkmate parameters validations and error handling

  logger::log_debug("[tube::put_datawarehouse_table] instanciating s3 client")
  s3_client <- paws.storage::s3(
    config = c(
      session$credentials,
      close_connection = TRUE)
  )

  filename <- tempfile()

  arrow::write_parquet(dataframe, filename)

  s3_client$put_object(
    Bucket = session$datawarehouse,
    Body = filename,
    Key = paste(table_name, paste(table_name, format(Sys.time(), format="%Y-%m-%d-%H:%M"), ".parquet", sep = ""), sep = "/")
  )

}





#' @export 
update_datawarehouse_table <- function(session, table_name, dataframe) {
  logger::log_debug("[tube::update_datawarehouse_table] entering function")

  # TODO: checkmate parameters validations and error handling

  logger::log_debug("[tube::update_datawarehouse_table] instanciating s3 client")
  s3_client <- paws.storage::s3(
    config = c(
      session$credentials, 
      close_connection = TRUE)
  )

  filename <- tempfile()

  arrow::write_parquet(dataframe, filename)

  s3_client$put_object(
    Bucket = session$datawarehouse,
    Body = filename,
    Key = paste(table_name, paste(table_name, format(Sys.time(), format="%Y-%m-%d-%H:%M"), ".parquet", sep = ""), sep = "/")
  )
}





#' @export
get_datawarehouse_inventory <- function() {

}





#' @export
refresh_datawarehouse_inventory <- function(session, table_name) {
  logger::log_debug("[tube::refresh_datawarehouse_inventory] entering function")

  # TODO: checkmate parameters validations and error handling
  logger::log_debug("[tube::refresh_datawarehouse_inventory] checking input parameters")

  logger::log_debug("[tube::refresh_datawarehouse_inventory] instanciating s3 client")
  glue_client <- paws.analytics::glue(
    config = c(
      session$credentials, 
      close_connection = TRUE)
  )

  glue_client$start_crawler(Name = paste(session$datawarehouse, table_name, "crawler", sep = "-"))

}
