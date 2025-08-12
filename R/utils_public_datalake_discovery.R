#' Format public datalake discovery results for all datasets
#'
#' @param con Database connection object
#' @return Formatted tibble with all public datalake datasets
#' @keywords internal
format_public_datalake_all_datasets <- function(con) {
  # Query the public datalake table for all datasets
  query <- 'SELECT name as table_name, tag, file_count, creation_date, consent_expiry_date, 
            data_destruction_date, sensitivity_level, ethical_stamp, 
            substr(user_metadata_json, 1, 50) || \'...\' as user_metadata_preview
            FROM "public-data-lake-content" ORDER BY name, tag'

  result <- DBI::dbGetQuery(con, query)

  # Convert to the expected format
  if (nrow(result) > 0) {
    result
  } else {
    data.frame()
  }
}

#' Format public datalake discovery results for pattern search
#'
#' @param con Database connection object
#' @param pattern Search pattern for dataset names
#' @return List with search results and summary
#' @keywords internal
format_public_datalake_pattern_search <- function(con, pattern) {
  # First, get count of matching datasets
  count_query <- paste0('SELECT COUNT(DISTINCT name) as count FROM "public-data-lake-content" 
                        WHERE name LIKE \'%', pattern, '%\'')
  count_result <- DBI::dbGetQuery(con, count_query)

  if (count_result$count == 0) {
    return(list(
      search_pattern = pattern,
      matching_tables = character(0),
      tables_summary = data.frame(),
      note = "No matching datasets found."
    ))
  }

  # Get matching table names
  tables_query <- paste0('SELECT DISTINCT name FROM "public-data-lake-content" 
                         WHERE name LIKE \'%', pattern, '%\' ORDER BY name')
  tables_result <- DBI::dbGetQuery(con, tables_query)

  # Get summary with tag counts for each matching table
  summary_query <- paste0('SELECT name as table_name, COUNT(DISTINCT tag) as tags_count,
                          array_join(array_agg(DISTINCT tag), \', \') as tags_list
                          FROM "public-data-lake-content" 
                          WHERE name LIKE \'%', pattern, '%\'
                          GROUP BY name ORDER BY name')
  summary_result <- DBI::dbGetQuery(con, summary_query)

  cli::cli_alert_info(paste("Found", count_result$count, "dataset(s) matching pattern:", pattern))
  cli::cli_alert_info("Use ellipse_discover(con, 'exact_dataset_name') to view details for a specific dataset")

  list(
    search_pattern = pattern,
    matching_tables = tables_result$name,
    tables_summary = summary_result,
    note = paste("Found", count_result$count, "table(s). Use exact table name to get detailed information.")
  )
}

#' Format public datalake discovery results for specific dataset
#'
#' @param con Database connection object
#' @param dataset_name Exact dataset name
#' @return List with detailed dataset information
#' @keywords internal
format_public_datalake_dataset_details <- function(con, dataset_name) {
  # Check if dataset exists
  check_query <- paste0('SELECT COUNT(*) as count FROM "public-data-lake-content" 
                        WHERE name = \'', dataset_name, '\'')
  check_result <- DBI::dbGetQuery(con, check_query)

  if (check_result$count == 0) {
    cli::cli_alert_danger("La table demandée est inconnue.")
    return(invisible(NULL))
  }

  # Get basic dataset info
  basic_query <- paste0('SELECT name as table_name, COUNT(DISTINCT tag) as tags_count,
                        array_join(array_agg(DISTINCT tag), \', \') as tags,
                        SUM(CAST(file_count AS INTEGER)) as total_files
                        FROM "public-data-lake-content" 
                        WHERE name = \'', dataset_name, '\'
                        GROUP BY name')
  basic_result <- DBI::dbGetQuery(con, basic_query)

  # Get detailed tag summary
  tags_query <- paste0('SELECT tag, file_count, creation_date, consent_expiry_date,
                       data_destruction_date, sensitivity_level, ethical_stamp,
                       regexp_extract(user_metadata_json, \'"([^"]+)"\', 1) as user_metadata_fields,
                       regexp_extract(user_metadata_json, \':\\\\s*"([^"]+)"\', 1) as user_metadata_values
                       FROM "public-data-lake-content" 
                       WHERE name = \'', dataset_name, '\'
                       ORDER BY tag')
  tags_result <- DBI::dbGetQuery(con, tags_query)

  # Get all files information from the arrays
  files_query <- paste0('SELECT tag, 
                        unnest(file_names) as file_name,
                        unnest(file_paths) as file_path,
                        unnest(file_extensions) as file_extension,
                        unnest(CAST(file_sizes_bytes AS ARRAY<INTEGER>)) as file_size_bytes
                        FROM "public-data-lake-content" 
                        WHERE name = \'', dataset_name, '\'
                        ORDER BY tag, file_name')
  files_result <- tryCatch(
    DBI::dbGetQuery(con, files_query),
    error = function(e) data.frame()
  )

  list(
    name = basic_result$table_name,
    tags_count = basic_result$tags_count,
    tags = if (!is.na(basic_result$tags)) strsplit(basic_result$tags, ", ")[[1]] else character(0),
    total_files = basic_result$total_files,
    tags_summary = tags_result,
    all_files = files_result,
    note = paste(
      "Table contains", basic_result$tags_count,
      "tag(s). Use ellipse_discover(con, object, tag) to view specific tag details."
    )
  )
}

#' Format public datalake discovery results for specific dataset and tag
#'
#' @param con Database connection object
#' @param dataset_name Exact dataset name
#' @param tag_name Exact tag name
#' @return List with specific tag information
#' @keywords internal
format_public_datalake_tag_details <- function(con, dataset_name, tag_name) {
  # Check if dataset and tag combination exists
  check_query <- paste0('SELECT COUNT(*) as count FROM "public-data-lake-content" 
                        WHERE name = \'', dataset_name, '\' AND tag = \'', tag_name, '\'')
  check_result <- DBI::dbGetQuery(con, check_query)

  if (check_result$count == 0) {
    cli::cli_alert_danger("La combinaison table/tag demandée est inconnue.")
    return(invisible(NULL))
  }

  # Get tag details
  tag_query <- paste0('SELECT name, tag, file_count, creation_date, consent_expiry_date,
                      data_destruction_date, sensitivity_level, ethical_stamp,
                      user_metadata_json
                      FROM "public-data-lake-content" 
                      WHERE name = \'', dataset_name, '\' AND tag = \'', tag_name, '\'')
  tag_result <- DBI::dbGetQuery(con, tag_query)

  # Get files for this specific tag
  files_query <- paste0('SELECT 
                        unnest(file_names) as file_name,
                        unnest(file_paths) as file_path,
                        unnest(file_extensions) as file_extension,
                        unnest(CAST(file_sizes_bytes AS ARRAY<INTEGER>)) as file_size_bytes
                        FROM "public-data-lake-content" 
                        WHERE name = \'', dataset_name, '\' AND tag = \'', tag_name, '\'
                        ORDER BY file_name')
  files_result <- tryCatch(
    DBI::dbGetQuery(con, files_query),
    error = function(e) data.frame()
  )

  # Parse user metadata JSON if it exists
  user_metadata <- NULL
  if (!is.null(tag_result$user_metadata_json) &&
    nzchar(tag_result$user_metadata_json)) {
    tryCatch(
      {
        user_metadata <- jsonlite::fromJSON(tag_result$user_metadata_json)
      },
      error = function(e) {
        user_metadata <<- list(error = "Could not parse user metadata JSON")
      }
    )
  }

  # Prepare result
  result <- list(
    name = tag_result$name,
    tag = tag_result$tag,
    file_count = tag_result$file_count,
    creation_date = tag_result$creation_date,
    consent_expiry_date = tag_result$consent_expiry_date,
    data_destruction_date = tag_result$data_destruction_date,
    sensitivity_level = tag_result$sensitivity_level,
    ethical_stamp = tag_result$ethical_stamp,
    files = files_result
  )

  if (!is.null(user_metadata)) {
    result$user_metadata <- user_metadata
  }

  result
}
