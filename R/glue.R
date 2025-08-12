#' List all the databases in the AWS Glue Data Catalog
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @param type A string to filter the databases by (e.g. "datawarehouse" or "datamart")
#' @returns A list of databases
list_glue_databases <- function(credentials, type) {
  logger::log_debug(
    paste(
      "[tube::list_glue_databases] entering function",
      "with type", type
    )
  )

  logger::log_debug("[tube::list_glue_databases] instanciating glue client")
  glue_client <- paws.analytics::glue(
    config = credentials
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
  database_list
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

  logger::log_debug("[tube::list_glue_tables] instanciating glue client")
  glue_client <- paws.analytics::glue(
    config = credentials
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
    if (simplify) {
      return(glue_table_list_to_tibble(tables))
    }
    return(tables)
  }
}

#' List the properties of a GLUE table
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @param schema The DBI schema to list the tables from
#' @param table The name of the table to list the properties from
#' @returns A tibble with the properties of the table
#' @examples
#' list_glue_table_properties(credentials, DBI::dbGetInfo(con)$dbms.name, "my_table")
list_glue_table_properties <- function(credentials, schema, table) {
  logger::log_debug("[tube::list_glue_table_properties] entering function")

  props <- list()

  logger::log_debug("[tube::list_glue_table_properties] instanciating glue client")

  glue_client <- paws.analytics::glue(
    config = credentials,
  )

  logger::log_debug(paste("[tube::list_glue_table_properties] getting table", table, "in schema", schema))
  props <- glue_client$get_table(
    DatabaseName = schema,
    Name = table
  )

  if (length(props) == 0) {
    logger::log_error("[tube::list_glue_table_properties] no table found")
    NULL
  } else {
    logger::log_debug("[tube::list_glue_table_properties] returning results")

    # only get the Parameters starting with "x-amz-meta-" as they are custom properties
    custom_parameters <- props$Table$Parameters[grepl("^x-amz-meta-", names(props$Table$Parameters))]
    # custom_parameters <- if (length(custom_parameters) == 0) NA else custom_parameters

    properties_tibble <- tibble::tibble(
      table_name = props$Table$Name,
      description = ifelse(
        is.null(props$Table$Description) || length(props$Table$Description) == 0,
        NA,
        props$Table$Description
      ),
      create_time = props$Table$CreateTime,
      update_time = props$Table$UpdateTime,
      location = props$Table$StorageDescriptor$Location,
      table_tags = if (length(custom_parameters) == 0) NULL else list(custom_parameters)
    )
    properties_tibble
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
    paste(
      "[tube::delete_glue_table] entering function with database_name",
      database_name,
      "and table_name",
      table_name
    )
  )

  # ensure the database name is the datamart
  if (!grepl("datamart", database_name)) {
    logger::log_error("[tube::delete_glue_table] only datamarts tables can be deleted")
    FALSE
  }

  logger::log_debug("[tube::delete_glue_table] instanciating glue client")
  glue_client <- paws.analytics::glue(
    config = credentials
  )

  result <- tryCatch(
    {
      logger::log_debug("[tube::delete_glue_table] deleting table")
      suppress_console_output({
        glue_client$delete_table(
          DatabaseName = database_name,
          Name = table_name
        )
      })
      logger::log_debug("[tube::delete_glue_table] table deleted")
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )

  result
}

#' Lists all the GLUE jobs in the account
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @returns A list of jobs
list_glue_jobs <- function(credentials) {
  logger::log_debug("[tube::list_glue_jobs] entering function")

  logger::log_debug("[tube::list_glue_jobs] instanciating glue client")
  glue_client <- paws.analytics::glue(
    config = credentials
  )

  logger::log_debug("[tube::list_glue_jobs] listing jobs")
  r <- glue_client$get_jobs()

  if (length(r) == 0) {
    logger::log_debug("[tube::list_glue_jobs] listing jobs")
    return(NULL)
  }

  # Should the glue client be closed
  # glue_client$close()

  # For now just return the full unprocessed list
  job_names <- sapply(r$Jobs, function(x) x$Name)

  logger::log_debug(paste("[tube::list_glue_jobs] returning results", paste(job_names, collapse = " | ")))

  job_names
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
    paste(
      "[tube::run_glue_job] entering function",
      "with job_name", job_name,
      "database", database,
      "and prefix", prefix
    )
  )

  if (is.null(database) || !database %in% c("datamart", "datamarts", "datawarehouse")) {
    logger::log_error("[tube::run_glue_job] invalid database name.  must be datamart, datamarts or datawarehouse")
    FALSE
  }

  logger::log_debug("[tube::run_glue_job] instanciating glue client")
  glue_client <- paws.analytics::glue(
    config = credentials
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

  logger::log_debug(paste(
    "[tube::run_glue_job] database", database,
    "bucket", bucket,
    "glue_db", glue_db,
    "table_name", table_name
  ))

  job_names <- list_glue_jobs(credentials)

  if (length(job_names) == 0) {
    logger::log_error("[tube::run_glue_job] no job found")
    FALSE
  }

  if (length(job_names) > 1) {
    logger::log_error("[tube::run_glue_job] more than one job found")
    FALSE
  }

  if (!job_name %in% job_names) {
    logger::log_error("[tube::run_glue_job] job not found")
    FALSE
  }


  # list all the unprocessed folders across the pipeline partitions
  # instanciate s3 client
  logger::log_debug("[tube::run_glue_job] instanciating s3 client")
  s3_client <- paws.storage::s3(
    config = c(
      credentials,
      close_connection = TRUE
    )
  )

  # list all the unprocessed folders across the pipeline prefix in the bucket
  logger::log_debug("[tube::run_glue_job] listing unprocessed folders")

  tmp_prefix <- gsub("^/", "", prefix)
  tmp_prefix <- gsub("/$", "", tmp_prefix)
  tmp_prefix_split <- strsplit(tmp_prefix, "/")

  if (length(tmp_prefix_split[[1]]) == 2) {
    r <- s3_client$list_objects_v2(
      Bucket = bucket,
      Prefix = ifelse(substr(prefix, nchar(prefix), nchar(prefix)) != "/", paste0(prefix, "/"), prefix),
      Delimiter = "/"
    )
  }

  if (length(tmp_prefix_split[[1]]) == 1) {
    first_level <- s3_client$list_objects_v2(
      Bucket = bucket,
      Prefix = ifelse(substr(prefix, nchar(prefix), nchar(prefix)) != "/", paste0(prefix, "/"), prefix),
      Delimiter = "/"
    )

    # only get the commonprefixes$Prefix values
    r <- lapply(first_level$CommonPrefixes, \(x) {
      s3_client$list_objects_v2(
        Bucket = bucket,
        Prefix = x$Prefix,
        Delimiter = "/"
      )
    })

    r <- r[[1]]
  }

  logger::log_debug("[tube::run_glue_job] wrangling partitions")
  partitions <- sapply(r$CommonPrefixes, function(x) {
    ret <- gsub(prefix, "", x$Prefix)
    ret <- gsub("^/", "", ret)
    ret <- gsub("/$", "", ret)
    ret
  })

  has_unprocessed <- function(prefix_list) {
    grepl("unprocessed", prefix_list$Prefix)
  }

  # for each partition, list the ones containing an "unprocessed" folder
  logger::log_debug("[tube::run_glue_job] looping through partitions")

  job_count <- 0

  for (partition in partitions) {
    logger::log_debug(paste(
      "[tube::run_glue_job] processing partition",
      partition,
      "in prefix",
      prefix
    ))

    r <- s3_client$list_objects_v2(
      Bucket = bucket,
      Prefix = paste0(prefix, "/", partition),
      Delimiter = "/"
    )

    unprocessed_prefixes <- Filter(has_unprocessed, r$CommonPrefixes)
    unprocessed_prefixes <- lapply(unprocessed_prefixes, function(x) x$Prefix)

    logger::log_debug(paste(
      "[tube::run_glue_job] found",
      length(unprocessed_prefixes),
      "unprocessed folders in partition",
      partition
    ))

    if (length(unprocessed_prefixes) > 0) {
      s3_input_path <- paste0("s3://", bucket, "/", unprocessed_prefixes)
      s3_output_path <- paste0("s3://", bucket, "/", prefix, "-output/")

      logger::log_debug(paste(
        "[tube::run_glue_job] wrangling glue job arguments",
        job_name,
        "with arguments",
        "s3_input_path", s3_input_path,
        "s3_output_path", s3_output_path,
        "glue_db_name", glue_db,
        "glue_table_name", table_name
      ))

      arguments_list <- list(
        "--s3_input_path" = s3_input_path,
        "--s3_output_path" = s3_output_path,
        "--glue_db_name" = glue_db,
        "--glue_table_name" = table_name
      )

      if (!is.null(table_tags)) {
        # change all the names of the named list
        # to x-amz-meta-<name> for every item in the list
        # that is not null and not already prefixed with x-amz-meta-
        table_tags <-
          setNames(
            table_tags,
            ifelse(!sapply(table_tags, is.null) &
                !sapply(grepl("x-amz-meta-", names(table_tags)), \(x) x),
              paste0("x-amz-meta-", names(table_tags)),
              names(table_tags)
            )
          )

        arguments_list <- c(arguments_list, list(
          "--custom_table_properties" =
            jsonlite::toJSON(table_tags,
              auto_unbox = TRUE,
              null = "null"
            )
        ))
      }

      if (!is.null(table_description)) {
        arguments_list <- c(arguments_list, list("--table_description" = table_description))
      }

      logger::log_debug(paste(
        "[tube::run_glue_job] starting glue job",
        job_name
      ))

      # start the glue job
      glue_client$start_job_run(
        JobName = job_name,
        Arguments = arguments_list
      )

      job_count <- job_count + 1
    } else {
      logger::log_debug("[tube::run_glue_job] no unprocessed folders found")
    }
  }

  if (job_count == 0) {
    logger::log_debug("[tube::run_glue_job] no job started as there was no unprocessed data")
    return(-1)
  }

  logger::log_debug("[tube::run_glue_job] job started successfully")
  TRUE
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
  df <- tibble::tibble(
    table_name = character(),
    col_name = character(),
    col_type = character(),
    is_partition = logical()
  )

  table_names <- purrr::map(glue_response[[1]], \(x) x$Name) |> unlist()

  for (i in seq_along(table_names)) {
    # Partitions and regulars columns are not together in the response
    partitions <- glue_response[[1]][[i]]$PartitionKeys
    col_names <- purrr::map(partitions, \(x) x$Name) |> unlist()
    col_types <- purrr::map(partitions, \(x) x$Type) |> unlist()

    parts <- tibble::tibble(
      table_name = table_names[i],
      col_name = col_names,
      col_type = col_types,
      is_partition = TRUE
    )

    columns <- glue_response[[1]][[i]]$StorageDescriptor$Columns
    col_names <- purrr::map(columns, \(x) x$Name) |> unlist()
    col_types <- purrr::map(columns, \(x) x$Type) |> unlist()

    cols <- tibble::tibble(
      table_name = table_names[i],
      col_name = col_names,
      col_type = col_types,
      is_partition = FALSE
    )

    df <- dplyr::bind_rows(df, parts, cols)
  }

  df
}


#' Update the custom tags (advanced properties) of a glue table
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @param schema The DBI schema to list the tables from
#' @param table The name of the table to update the tags of
#' @param table_tags A list of custom tags to update the table with
#' @returns A boolean indicating wether or not the tags were updated
#' successfully
#' @examples
#' update_glue_table_tags(
#'   credentials,
#'   DBI::dbGetInfo(con)$dbms.name,
#'   "my_table",
#'   list(tag1 = "value1", tag2 = "value2")
#' )
update_glue_table_tags <- function(creds, schema, table, new_table_tags) {
  logger::log_debug(paste(
    "[tube::update_glue_table_tags] entering function",
    "with schema", schema,
    "table", table,
    "and new_table_tags", paste(new_table_tags, collapse = ", ")
  ))

  # instanciate clue client
  logger::log_debug("[tube::update_glue_table_tags] instanciating glue client")
  glue_client <- paws.analytics::glue(
    config = creds
  )

  # Get the table details
  logger::log_debug("[tube::update_glue_table_tags] Getting table details")
  table_details <- glue_client$get_table(
    DatabaseName = schema,
    Name = table
  )

  # add x-amz-meta- prefix to the tags if not already present
  new_table_tags <-
    setNames(
      new_table_tags,
      ifelse(!sapply(grepl("x-amz-meta-", names(new_table_tags)), \(x) x),
        paste0("x-amz-meta-", names(new_table_tags)),
        names(new_table_tags)
      )
    )

  properties_to_remove <- list()
  custom_table_properties <- new_table_tags
  if (is.list(custom_table_properties) && !is.null(custom_table_properties) && length(custom_table_properties) > 0) {
    # custom_table_properties <- jsonlite::fromJSON(custom_table_properties)
    # Identify properties to remove (those explicitly set to NULL)
    properties_to_remove <- names(custom_table_properties)[sapply(custom_table_properties, is.null)]
    # Remove properties that are set to NULL from custom_table_properties
    custom_table_properties <- custom_table_properties[!sapply(custom_table_properties, is.null)]
  }

  custom_table_properties <-
    setNames(
      custom_table_properties,
      ifelse(!sapply(custom_table_properties, is.null) &
          !sapply(grepl("x-amz-meta-", names(custom_table_properties)), \(x) x),
        paste0("x-amz-meta-", names(custom_table_properties)),
        names(custom_table_properties)
      )
    )

  existing_parameters <- table_details$Table$Parameters

  if (!is.null(custom_table_properties) && length(custom_table_properties) > 0) {
    existing_parameters <- modifyList(existing_parameters, custom_table_properties)
  }

  if (length(properties_to_remove) > 0) {
    existing_parameters <- existing_parameters[!names(existing_parameters) %in% properties_to_remove]
  }

  # Update the glue table
  logger::log_debug("[tube::update_glue_table_tags] Updating glue table")

  # Define the table input list
  table_input <- list(
    Name = table_details$Table$Name,
    Retention = table_details$Table$Retention,
    StorageDescriptor = table_details$Table$StorageDescriptor,
    PartitionKeys = table_details$Table$PartitionKeys,
    TableType = table_details$Table$TableType,
    Description = ifelse(!is.null(table_details$Table$Description), table_details$Table$Description, "")
  )

  # Only include Parameters if it's not empty
  if (!is.null(existing_parameters)) {
    table_input$Parameters <- existing_parameters
  }

  glue_client$update_table(
    DatabaseName = schema,
    TableInput = table_input
  )

  # Return TRUE indicating successful update
  logger::log_debug("[tube::update_glue_table_tags] Tags updated successfully")
  TRUE
}


#' Update the description of a glue table
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @param schema The DBI schema to list the tables from
#' @param table The name of the table to change the description of
#' @param desc A string contaning the new description of the table
#' @returns A boolean indicating wether or not the description was updated
#' successfully
#' @examples
#' update_glue_table_desc(credentials, DBI::dbGetInfo(con)$dbms.name, "my_table", "new description of my_table")
update_glue_table_desc <- function(creds, schema, table, desc) {
  logger::log_debug(paste(
    "[tube::update_glue_table_desc] entering function",
    "with schema", schema,
    "table", table,
    "and desc", desc
  ))

  # instanciate clue client
  logger::log_debug("[tube::update_glue_table_desc] instanciating glue client")
  glue_client <- paws.analytics::glue(
    config = creds
  )

  # Get the table details
  logger::log_debug("[tube::update_glue_table_desc] Getting table details")
  table_details <- glue_client$get_table(
    DatabaseName = schema,
    Name = table
  )

  # Define the table input list
  table_input <- list(
    Name = table_details$Table$Name,
    Retention = table_details$Table$Retention,
    StorageDescriptor = table_details$Table$StorageDescriptor,
    PartitionKeys = table_details$Table$PartitionKeys,
    TableType = table_details$Table$TableType,
    Parameters = table_details$Table$Parameters,
    Description = desc
  )

  r <- glue_client$update_table(
    DatabaseName = schema,
    TableInput = table_input
  )

  # Return TRUE indicating successful update
  if (length(r) != 0) {
    logger::log_error("[tube::update_glue_table_desc] Description not updated")
    FALSE
  }

  logger::log_debug("[tube::update_glue_table_desc] Description updated successfully")
  TRUE
}
