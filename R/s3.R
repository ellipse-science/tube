#' commits an R object to an S3 bucket
#' @param PARM1
#' @param PARM2
#' @param PARM3
#' @param PARM4
#'
#' @examples
#' \dontrun{
#'   # put some sample code here as an example
#' }
#'
#' @export
commit_r_object_to_datalake <- function(
  s3_client,
  bucket, 
  key, 
  metadata, 
  data, 
  prefix, 
  keep_history, 
  history_schema, 
  refresh_data) {

  logger::log_debug("[pumpr::commit_r_object_to_datalake] entering function")
  logger::log_info("[pumpr::commit_r_object_to_datalake] committing object to datalake")

  # TODO: checkmate parameters validations and error handling
  checkmate::assertChoice(metadata$format, c("pdf", "html", "xml", "json", "docx", "xlsx", "csv"))
  
  # figure our path (s3 prefix) based on whether we have to keep_history or not
  if (keep_history) {
    # we'll handle the history_granularity later
    # for now if keey history, then we partition the path based on partition schema
    # Partition schemas:
    # YYYY
    # YYYY/MM
    # YYYY/MM/DD
    # YYYY/MM/DD/HH
    # YYYY/WEEKNUM 

    # compute the partition prefix
    partition_suffix <- dplyr::case_when(
      history_schema == "YYYY" ~ format(Sys.Date(), format="%Y"),
      history_schema == "YYYY/MM" ~ format(Sys.Date(), format="%Y/%m"),
      history_schema == "YYYY/MM/DD" ~ format(Sys.Date(), format="%Y/%m/%d"),
      history_schema == "YYYY/MM/DD/HH" ~ format(Sys.time(), format="%Y/%m/%d/%H"),
      history_schema == "YYYY/WEEKNUM" ~ format(Sys.time(), format="%Y/%W"),      
    )

    partition <- paste(prefix, partition_suffix, sep="/")

    metadata$partitionned = "TRUE"
    metadata$partition_schema = history_schema
    metadata$partition = partition_suffix
  } else {
    partition <- ""
    metadata$partitionned = "FALSE"
    metadata$partition_schema <- NA_character_
    metadata$partition <- NA_character_
  }

  #split_metadata <- paste(paste("metadata", names(metadata), sep="."), metadata, collapse = ",", sep = ":")
  #ist(split_metadata)

  names(metadata) <- paste("metadata", names(metadata), sep = "_")

  # we're in lambda so we'll use a temporary fildsystem
  td <- tempdir()
  filename <- paste(key, "json", sep = ".")

  # build json object
  json_object <- jsonlite::toJSON(
    c(
      #key = gsub(".json$", "", paste(base_path,filename,sep="/")),
      key = gsub(".json$", "", filename),
      metadata,
      data
    ),
    auto_unbox = T
  )

  # todo : detect object type (df, list, character etc) and write accordingly
  write(json_object, file.path(td, filename))

  # put the object in s3 bucket 
  s3_client$put_object(
    Bucket = bucket,
    Body = file.path(td, filename),
    Key = paste(partition, filename, sep="/"),
    ContentType = "application/json; charset=utf-8"
  )  

  #TODO : Error management

  logger::log_debug("[pumpr::commit_r_object_to_datalake] exiting function")
}





#' retreieves and returns an R object to an S3 bucket
#' @param PARM1
#' @param PARM2
#' @param PARM3
#' @param PARM4
#'
#' @examples
#' \dontrun{
#'   # put some sample code here as an example
#' }
#'
#' @export
get_r_object_from_datalake <- function(aws_client, bucket, base_path, objectname, history_version = "") {
  logger::log_debug("[pumpr::get_r_object_from_datalake] entering function")
  logger::log_info(
    paste(
      "[pumpr::get_r_object_from_datalake] retrieving object",
      paste(base_path, objectname, sep="/"),
      "from bucket",
      bucket
    )
  )

  # TODO: checkmate parameters validations and error handling

  # put the object in s3 bucket 
  # TODO: mettre la gestion des erreur autour de ce code
  object <- aws_client$get_object( 
    Bucket = bucket,
    Key = paste(base_path, objectname, sep="/")
  )

  
  logger::log_debug("[pumpr::get_r_object_from_datalake] exiting function and returning rawToChar object")

  return(object$Body %>%  rawToChar)

  #TODO : Error management

  # aws.s3::put_object(
  #   file = file.path(td, filename), 
  #   object = paste(path,objectname,sep="/"),
  #   bucket = bucket,
  #   headers = metadata
  # )

}




#' retrieves a list and returns a dataframe of R objects from an S3 bucket through GLUE
#' @param PARM1
#' @param PARM2
#' @param PARM3
#' @param PARM4
#'
#' @examples
#' \dontrun{
#'   # put some sample code here as an example
#' }
#'
#' @export
get_datalake_content <- function(
                              data_source, 
                              columns = list(), 
                              filter = list(),
                              download_data = FALSE,
                              pipeline_handler = "lambda") {

logger::log_debug("[pumpr::get_datalake_content] entering function")

  datalake_name <- strsplit(data_source, "\\.")[[1]][1]
  database_name <- strsplit(data_source, "\\.")[[1]][2]
  table_name <- strsplit(data_source, "\\.")[[1]][3]

  # TODO: checkmate parameters validations and error handling

  if (pipeline_handler == "lambda") {
    con <- DBI::dbConnect(
      noctua::athena(),
      s3_staging_dir=paste("s3:/", datalake_name, table_name, sep="/"),
      region_name='ca-central-1'
    )
  } else {
    con <- DBI::dbConnect(
      noctua::athena(),
      aws_access_key_id=Sys.getenv("AWS_ACCESS_KEY_ID"),
      aws_secret_access_key=Sys.getenv("AWS_SECRET_ACCESS_KEY"),
      s3_staging_dir=paste("s3:/", datalake_name, table_name, sep="/"),
      region_name='ca-central-1'
    )
  }

  #logger::log_debug("[pumpr::get_datalake_content] listing tables")
  #DBI::dbListTables(con)

  logger::log_debug("[pumpr::get_datalake_content] executing query")

  columns_string <- if (typeof(columns) == "list") paste(columns, collapse = ",")
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

  logger::log_debug( 
    paste(
      "[pumpr::get_datalake_content] query string is",
      paste(
        "SELECT ", 
        columns_string,
        " FROM \"",
        database_name,
        "\".\"",
        table_name,
        filter_string,
        ";",
        sep = ""
      )
    )
  )
  res <- NULL

  res <- res <- tryCatch(
    expr = {
      DBI::dbExecute(
        con, 
        paste(
          "SELECT ", 
          columns_string,
          " FROM \"",
          database_name,
          "\".\"",
          table_name,
          "\"",
          filter_string,
          ";",
          sep = ""
        )
      )
    },
    error = function(e) {
      if (grepl("TABLE_NOT_FOUND", e$message)) {
        msg <- paste(
          "[pumpr::get_datawarehouse_content]",
          "The table specified",
          data_source,
          "does not exist...  the dataframe returned is NULL"
        )
        logger::log_error(msg)
      } else {
        msg <- paste(
          "[pumpr::get_datawarehouse_content]",
          "an error occurred",
          e$message
        )
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



#' retrieves a list and returns a dataframe of R objects from an S3 bucket through GLUE
#' @param PARM1
#' @param PARM2
#' @param PARM3
#' @param PARM4
#'
#' @examples
#' \dontrun{
#'   # put some sample code here as an example
#' }
#'
#' @export
get_datawarehouse_content <- function(
                              data_source, 
                              columns = list(), 
                              filter = list(),
                              download_data = FALSE,
                              pipeline_handler = "lambda") {

  logger::log_debug("[pumpr::get_datawarehouse_content] entering function")

  datawarehouse_name <- strsplit(data_source, "\\.")[[1]][1]
  database_name <- strsplit(data_source, "\\.")[[1]][2]
  table_name <- strsplit(data_source, "\\.")[[1]][3]

  # TODO: checkmate parameters validations and error handling

  if (pipeline_handler == "lambda") {
    con <- DBI::dbConnect(
      noctua::athena(),
      s3_staging_dir=paste("s3:/", datawarehouse_name, table_name, sep="/"),
      region_name='ca-central-1'
    )
  } else {
    con <- DBI::dbConnect(
      noctua::athena(),
      aws_access_key_id=Sys.getenv("AWS_ACCESS_KEY_ID"),
      aws_secret_access_key=Sys.getenv("AWS_SECRET_ACCESS_KEY"),
      s3_staging_dir=paste("s3:/", datawarehouse_name, table_name, sep="/"),
      region_name='ca-central-1'
    )
  }

  #logger::log_debug("[pumpr::get_datawarehouse_content] listing tables")
  #DBI::dbListTables(con)

  logger::log_debug("[pumpr::get_datawarehouse_content] executing query")

  columns_string <- if (typeof(columns) == "list") paste(columns, collapse = ",")
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

  logger::log_debug( 
    paste(
      "[pumpr::get_datalake_content] query string is",
      paste(
        "SELECT ", 
        columns_string,
        " FROM \"",
        database_name,
        "\".\"",
        table_name,
        filter_string,
        ";",
        sep = ""
      )
    )
  )
  res <- NULL

  res <- tryCatch(
    expr = {
      DBI::dbExecute(
        con, 
        paste(
          "SELECT ", 
          columns_string,
          " FROM \"",
          database_name,
          "\".\"",
          table_name,
          "\"",
          filter_string,
          ";",
          sep = ""
        )
      )
    },
    error = function(e) {
      if (grepl("TABLE_NOT_FOUND", e$message)) {
        msg <- paste(
          "[pumpr::get_datawarehouse_content]",
          "The table specified",
          data_source,
          "does not exist...  the dataframe returned is NULL"
        )
        logger::log_error(msg)
      } else {
        msg <- paste(
          "[pumpr::get_datawarehouse_content]",
          "an error occurred",
          e$message
        )
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