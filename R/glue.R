
#' List all the databases in the AWS Glue Data Catalog
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @param type A string to filter the databases by (e.g. "datawarehouse" or "datamart")
#' @returns A list of databases
list_glue_databases <- function(credentials, type) {
  logger::log_debug(
    paste("[tube::list_glue_databases] entering function",
    "with type", type)
  )

  logger::log_debug("[tube::list_glue_databases] instanciating glue client")
  glue_client <- paws.analytics::glue(
    credentials = credentials
  )

  logger::log_debug("[tube::list_glue_databases] listing databases")
  r <- glue_client$get_databases()

  logger::log_debug("[tube::list_glue_databases] wrangling result")
  list <- unlist(r$DatabaseList)
  database_list <- list[grep(type, list)]
  database_list <- as.list(database_list)
  if (length(database_list) == 0) {
    return(NULL)
  }
  names(database_list) <- ""
  database_list <- unlist(database_list)

  logger::log_debug("[tube::list_glue_databases] wrangling result")
  return(database_list)
}

#' Lists all the tables in a GLUE database
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @param schema The DBI schema to list the tables from
#' @param tablename_filter A string to filter the tables by
#' @param simplify A boolean indicating wether or not to simplify the output
#' @returns A list of tables
list_glue_tables <- function(credentials, schema, tablename_filter = NULL, simplify = TRUE) {
  logger::log_debug("[tube::list_glue_tables] entering function")
  table_list <- list()

  logger::log_debug("[tube::list_glue_tables] instanciating glue client")
  glue_client <- paws.analytics::glue(
    credentials = credentials
  )

  logger::log_debug("[tube::list_glue_tables] listing tables")
  tables <- glue_client$get_tables("", schema)

  if (!is.null(tablename_filter)) {
    tables$TableList <- tables$TableList[grep(tablename_filter, sapply(tables$TableList, function(x) x$Name))]
  }

  if (length(tables) == 0) {
    logger::log_error("[tube::list_glue_tables] no table found")
    return(NULL)
  } else {
    logger::log_debug("[tube::list_glue_tables] returning results")
    if (simplify) return(glue_table_list_to_tibble(tables))
    return(tables)
  }
}

#' Delete glue table
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @param table_name The name of the table to delete
#' @param database_name The name of the database to delete the table from
#' @returns A boolean indicating wether or not the table was deleted
#' successfully
delete_glue_table <- function(credentials, database_name, table_name) {
  logger::log_debug(
    paste("[tube::delete_glue_table] entering function with database_name", 
    database_name, 
    "and table_name", 
    table_name))

  # ensure the database name is the datamart
  if (!grepl("datamart", database_name)) {
    logger::log_error("[tube::delete_glue_table] only datamarts tables can be deleted")
    return(FALSE)
  }

  logger::log_debug("[tube::delete_glue_table] instanciating glue client")
  glue_client <- paws.analytics::glue(
    credentials = credentials
  )

  result <- tryCatch({
    logger::log_debug("[tube::delete_glue_table] deleting table")
    suppress_console_output({glue_client$delete_table(
      DatabaseName = database_name,
      Name = table_name
    )})
    logger::log_debug("[tube::delete_glue_table] table deleted")
    TRUE
  }, error = function(e) {
    FALSE
  })

  result
}

#' Lists all the GLUE jobs in the account
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @returns A list of jobs
list_glue_jobs <- function(credentials) {
  logger::log_debug("[tube::list_glue_jobs] entering function")
  job_list <- list()

  logger::log_debug("[tube::list_glue_jobs] instanciating glue client")
  glue_client <- paws.analytics::glue(
    credentials = credentials
  )

  logger::log_debug("[tube::list_glue_jobs] listing jobs")
  r <- glue_client$get_jobs()

  if (length(r) == 0) {
    return(NULL)
  }

  # Should the glue client be closed
  # glue_client$close()

  # For now just return the full unprocessed list
  job_names <- sapply(r$Jobs, function(x) x$Name)

  return(job_names)
}

#' Run a GLUE job manually specifically for ingesting CSV in to
#' the datawarehouse or datamart
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @param job_name The name of the job to run
#' @param database The name of the database to run the job for 
#' it must be either "datamart", "datamarts" or "datawarehouse"
#' @param prefix The prefix to run the job for.  This is the first
#' level partition in the datawarehouse or datamart bucket
#' in the datawarehouse, it represents the pipeline name
#' in the datamart, it represents the datamart and table name separates with a /
#' @returns A boolean indicating wether or not the job was started
run_glue_job <- function(credentials, job_name, database, prefix, table_tags = NULL, table_description = NULL) {
  logger::log_debug(
    paste("[tube::run_glue_job] entering function",
    "with job_name", job_name,
    "database", database,
    "and prefix", prefix)
  )

  if (is.null(database) || !database %in% c("datamart", "datamarts", "datawarehouse")) {
    logger::log_error("[tube::run_glue_job] invalid database name.  must be datamart, datamarts or datawarehouse")
    return(FALSE)
  }

  logger::log_debug("[tube::run_glue_job] instanciating glue client")
  glue_client <- paws.analytics::glue(
    credentials = credentials
  )

 switch(database,
    "datamarts" = {
      bucket <- list_datamarts_bucket(credentials)
      glue_db <- list_glue_databases(credentials, "datamart")
      table_name <- paste0(strsplit(prefix, "/")[[1]][1], "-", strsplit(prefix, "/")[[1]][2])
    },
    "datawarehouse" = {
      bucket <- list_datawarehouse_bucket(credentials)
      glue_db <- list_glue_databases(credentials, "datawarehouse")
      table_name <- strsplit(prefix, "/")[[1]][1]
    },
    {
      stop("Invalid database type")
    }
  )

  job_names <- list_glue_jobs(credentials)
  if (length(job_names) == 0) {
    logger::log_error("[tube::run_glue_job] no job found")
    return(FALSE)
  }

  if (length(job_names) > 1) {
    logger::log_error("[tube::run_glue_job] more than one job found")
    return(FALSE)
  }

  if (!job_name %in% job_names) {
    logger::log_error("[tube::run_glue_job] job not found")
    return(FALSE)
  }

  # list all the unprocessed folders across the pipeline partitions
  # instanciate s3 client
  s3_client <- paws.storage::s3(
    config = c(
      credentials,
      close_connection = TRUE)
  )

  # list all the unprocessed folders across the pipeline prefix in the datawarehouse bucket
  r <- s3_client$list_objects_v2(
    Bucket = bucket,
    Prefix = ifelse(substr(prefix, nchar(prefix), nchar(prefix)) != "/", paste0(prefix, "/"), prefix),
    Delimiter = "/"
  )

  partitions <- sapply(r$CommonPrefixes, function(x) {
    prefix <- x$Prefix
    parts <- unlist(strsplit(prefix, "/"))
    return(parts[length(parts)])
  })

  has_unprocessed <- function(prefix_list) {
    grepl("unprocessed", prefix_list$Prefix)
  }

  # for each partition, list the ones containing an "unprocessed" folder
  for (partition in partitions) {
    r <- s3_client$list_objects_v2(
      Bucket = bucket,
      Prefix = paste0(prefix,"/",partition),
      Delimiter = "/"
    )

    unprocessed_prefixes <- Filter(has_unprocessed, r$CommonPrefixes)
    unprocessed_prefixes <- lapply(unprocessed_prefixes, function(x) x$Prefix)

    if (length(unprocessed_prefixes) > 0) {
      s3_input_path = paste0("s3://",bucket, "/", unprocessed_prefixes)
      s3_output_path = paste0("s3://",bucket, "/", prefix, "-output/")

      arguments_list <- list(
          '--s3_input_path' = s3_input_path,
          '--s3_output_path' = s3_output_path,
          '--glue_db_name' = glue_db,
          '--glue_table_name' = table_name
      )

      if (!is.null(table_tags)) {
        arguments_list <- c(arguments_list, list('--custom_table_properties' = jsonlite::toJSON(table_tags, auto_unbox = TRUE, null = "null")))
      } 

      if (!is.null(table_description)) {
        arguments_list <- c(arguments_list, list('--table_description' = table_description))
      }
      
      logger::log_debug(paste(
        "[tube::run_glue_job] starting job",
        job_name,
        "with arguments",
        paste(arguments_list, collapse = ", ")
      ))

      # start the glue job
      glue_client$start_job_run(
        JobName = job_name,
        Arguments = arguments_list
      )
    }
  }
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
#'   `tube::list_glue_tables()` 
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
