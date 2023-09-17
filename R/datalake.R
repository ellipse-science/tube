#' @export
list_datalakes <- function(credentials) {
  logger::log_debug("[pumpr::list_datalakes] entering function")

  logger::log_debug("[pumpr::list_datalakes] instanciating s3 client")
  s3_client <- paws.storage::s3(
    config = c(
      credentials, 
      close_connection = TRUE)
  )

  logger::log_debug("[pumpr::list_datalakes] listing buckets")
  r <- s3_client$list_buckets()

  #TODO: error management if no bucket is returned

  logger::log_debug("[pumpr::list_datalakes] wrangling result")
  list <- unlist(r$Buckets)
  datalake_list <- list[grep("datalake", list)]
  datalake_list <- as.list(as.data.frame(datalake_list))

  logger::log_debug("[pumpr::list_datalakes] returning results")
  return(datalake_list)
}



#' @export 
list_datalake_tables <- function(credentials, datalake_name) {
  logger::log_debug("[pumpr::get_datalake_inventory] entering function")

  # TODO: checkmate parameters validations and error handling
  logger::log_debug("[pumpr::put_datalake_object] instanciating s3 client")
  glue_client <- paws.analytics::glue(
    config = c(
      credentials, 
      close_connection = TRUE)
  )

  logger::log_debug("[pumpr::get_datalake_content] getting list of tables and their properties")

  tables <- glue_client$get_tables(DatabaseName = datalake_name)
  df <- as.data.frame(do.call(rbind, tables$TableList))

  return(df)
}



#' @export 
get_datalake_table_info <- function(credentials, datalake_name, table_name) {
  logger::log_debug("[pumpr::get_datalake_inventory] entering function")

  # TODO: checkmate parameters validations and error handling
  logger::log_debug("[pumpr::put_datalake_object] instanciating s3 client")
  glue_client <- paws.analytics::glue(
    config = c(
      credentials, 
      close_connection = TRUE)
  )

  logger::log_debug("[pumpr::get_datalake_content] getting list of tables and their properties")

  table <- glue_client$get_table(DatabaseName = datalake_name, Name = table_name)
  df <- as.data.frame(do.call(rbind, table))

  return(df)
}



#' @export
get_datalake_object <- function(credentials, datalake_name, prefix, partition, key) {
  logger::log_debug("[pumpr::get_datalake_object] entering function")

  # TODO: checkmate parameters validations and error handling

  logger::log_debug("[pumpr::list_datalakes] instanciating s3 client")
  s3_client <- paws.storage::s3(
    config = c(
      credentials, 
      close_connection = TRUE)
  )

  logger::log_info(
    paste(
      "[pumpr::get_datalake_object] retrieving object",
      paste(prefix, key, sep="/"),
      "from bucket",
      datalake_name
    )
  )

  # TODO: mettre la gestion des erreurs autour de ce code
  object <- s3_client$get_object( 
    Bucket = datalake_name,
    Key = paste(prefix, partition, paste(key, ".json", sep=""), sep="/")
  )

  logger::log_debug("[pumpr::get_r_object_from_datalake] exiting function and returning rawToChar object")

  raw_to_char <- object$Body %>%  rawToChar
  json_to_list <- jsonlite::fromJSON(raw_to_char)

  #TODO : Error management before returning
  return(json_to_list)
}




#' @export
put_datalake_object <- function(credentials, datalake_name, prefix, partition_schema = NULL, key, metadata, data, refresh_data = FALSE) {
  logger::log_debug("[pumpr::put_datalake_object] entering function")

  # TODO: checkmate parameters validations and error handling
  logger::log_debug("[pumpr::put_datalake_object] checking input parameters")
  checkmate::assertChoice(
    tolower(metadata$content_type), 
    tolower(c(
      "text/html; charset=utf-8",
      "application/rss\\+xml",
      "application/json"
    ))
  )

  logger::log_debug("[pumpr::put_datalake_object] instanciating s3 client")
  s3_client <- paws.storage::s3(
    config = c(
      credentials, 
      close_connection = TRUE)
  )

  # compute the partition 
  partition <- dplyr::case_when(
    length(partition_schema) == 0 ~ NA_character_,
    partition_schema == "YYYY" ~ format(Sys.Date(), format="%Y"),
    partition_schema == "YYYY/MM" ~ format(Sys.Date(), format="%Y/%m"),
    partition_schema == "YYYY/MM/DD" ~ format(Sys.Date(), format="%Y/%m/%d"),
    partition_schema == "YYYY/MM/DD/HH" ~ format(Sys.time(), format="%Y/%m/%d/%H"),
    partition_schema == "YYYY/WEEKNUM" ~ format(Sys.time(), format="%Y/%W"),  
    TRUE ~ NA_character_
  )

  # add partition info in metadata
  if (length(partition) > 0 && !is.null(partition) && !is.na(partition)) {
    metadata$partitionned = "TRUE"
    metadata$partition_schema = history_schema
    metadata$partition = partition_suffix
    prefix <- paste(prefix, partition, sep="/") 
  } else {
    metadata$partitionned = "FALSE"
    metadata$partition_schema <- NA_character_
    metadata$partition <- NA_character_
  }

  names(metadata) <- paste("metadata", names(metadata), sep = "_")

  # we're in lambda so we'll use a temporary fildsystem
  logger::log_info("[pumpr::put_datalake_object] writing temporary file to disk")

  td <- tempdir()
  filename <- paste(key, "json", sep = ".")

  # build json object
  json_object <- jsonlite::toJSON(
    c(
      list(key = key),
      metadata,
      data
    ),
    auto_unbox = T
  )

  # todo : detect object type (df, list, character etc) and write accordingly
  write(json_object, file.path(td, filename))

  # put the object in s3 bucket 
  logger::log_info("[pumpr::put_datalake_object] committing object to datalake")
  s3_client$put_object(
    Bucket = datalake_name,
    Body = file.path(td, filename),
    Key = paste(prefix, filename, sep="/"),
    ContentType = "application/json; charset=utf-8"
  )  

  # remove tmp file
  logger::log_info("[pumpr::put_datalake_object] removing temporary file from disk")
  file.remove(file.path(td, filename))


  #TODO : Error management

  logger::log_debug("[pumpr::put_datalake_object] exiting function")
}




#' @export 
refresh_datalake_inventory <- function(credentials, datalake_name, table_name) {

}




#' @export 
get_datalake_inventory <- function(credentials, datalake_name, table_name, filter = NULL, download_content = FALSE) {
  logger::log_debug("[pumpr::get_datalake_inventory] entering function")

  # TODO: checkmate parameters validations and error handling
  logger::log_debug("[pumpr::get_datalake_inventory] opening noctua athena DBI connection")
  if (exists("credentials") && length(credentials) > 0 && !is.null(credentials) && !is.na(credentials)) {
    con <- DBI::dbConnect(
      noctua::athena(),
      aws_access_key_id=Sys.getenv("AWS_ACCESS_KEY_ID"),
      aws_secret_access_key=Sys.getenv("AWS_SECRET_ACCESS_KEY"),
      s3_staging_dir=paste("s3:/", datalake_name, table_name, sep="/"),
      region_name='ca-central-1'
    )
  } else {
    con <- DBI::dbConnect(
      noctua::athena(),
      s3_staging_dir=paste("s3:/", datalake_name, table_name, sep="/"),
      region_name='ca-central-1'
    )
  }

  logger::log_debug("[pumpr::get_datalake_content] building query")

  filter_string <- if (!is.null(unlist(filter))) {
    paste(
      " WHERE ",
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

  if (!download_content) {
    table_properties <- get_datalake_table_info(credentials, datalake_name, table_name)
    columns_string <- strsplit(table_properties$StorageDescriptor[[1]]$SerdeInfo$Parameters$paths, ",")
    columns_string <- paste(columns_string[[1]][grep("metadata", columns_string[[1]])], collapse=",")
    columns_string <- paste("key", columns_string, sep=",")
  } else {
    columns_string <- if (!is.null(unlist(columns))) paste(columns, collapse = ",") else "*"
  }

  query_string <- paste(
    "SELECT ", columns_string,
    " FROM \"", datalake_name, "\".\"", table_name, "\"",
    if (nchar(filter_string) > 0) filter_string else NULL,";",
    sep = ""

  )

  logger::log_debug(paste("[pumpr::get_datalake_content] query string is", query_string))
  logger::log_debug("[pumpr::get_datalake_content] executing query")

  res <- NULL

  res <- tryCatch(
    expr = {DBI::dbExecute(con, query_string)},
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
      logger::log_warn("[pumpr::get_datalake_content] The query was successful but the dataframe returned is empty.  Check the columns or the filter you sent to the function")
    }
  } else {
    logger::log_debug("[pumpr::get_datalake_content] setting null dataframe")
    df <- NULL
  }

  logger::log_debug("[pumpr::get_datalake_content] exiting function")
  return(df)
}



