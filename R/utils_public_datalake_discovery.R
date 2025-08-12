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

  if (nrow(result) == 0) {
    cli::cli_alert_info("📊 No datasets found in public datalake")
    return(invisible(NULL))
  }

  # Create nice output with icons
  cli::cli_h2("🗂️  Public Datalake - All Datasets")
  cli::cli_text("")

  # Group by dataset name for better display
  datasets <- unique(result$table_name)

  for (dataset in datasets) {
    dataset_rows <- result[result$table_name == dataset, ]
    cli::cli_h3(glue::glue("📁 {dataset}"))
    cli::cli_ul()
    for (i in seq_len(nrow(dataset_rows))) {
      tag <- dataset_rows[i, "tag"]
      file_count <- dataset_rows[i, "file_count"]
      creation_date <- dataset_rows[i, "creation_date"]
      cli::cli_li(glue::glue("🏷️  Tag: {tag} | 📄 Files: {file_count} | 📅 Created: {creation_date}"))
    }
    cli::cli_end()
    cli::cli_text("")
  }

  cli::cli_alert_info("💡 Use ellipse_discover(con, 'dataset_name') for detailed information")
  invisible(result)
}

#' Format public datalake discovery results for pattern search
#'
#' @param con Database connection object
#' @param pattern Search pattern for dataset names
#' @return List with search results and summary
#' @keywords internal
format_public_datalake_pattern_search <- function(con, pattern) {
  # Single query to get all matching dataset information
  comprehensive_query <- paste0('
    SELECT name as table_name, COUNT(DISTINCT tag) as tags_count,
           array_join(array_agg(DISTINCT tag), \', \') as tags_list,
           SUM(CAST(file_count AS INTEGER)) as total_files
    FROM "public-data-lake-content"
    WHERE name LIKE \'%', pattern, "%'
    GROUP BY name ORDER BY name")

  result <- DBI::dbGetQuery(con, comprehensive_query)

  if (nrow(result) == 0) {
    cli::cli_alert_warning(glue::glue("🔍 No datasets found matching pattern: {pattern}"))
    return(invisible(NULL))
  }

  # Create nice output with icons
  cli::cli_h2(glue::glue("🔍 Search Results for: {pattern}"))
  cli::cli_text("")
  cli::cli_alert_success(glue::glue("Found {nrow(result)} dataset(s) matching pattern"))
  cli::cli_text("")

  for (i in seq_len(nrow(result))) {
    table_name <- result[i, "table_name"]
    tags_count <- result[i, "tags_count"]
    tags_list <- result[i, "tags_list"]
    total_files <- result[i, "total_files"]

    cli::cli_h3(glue::glue("📁 {table_name}"))
    cli::cli_ul()
    cli::cli_li(glue::glue("🏷️  Tags: {tags_count} ({tags_list})"))
    cli::cli_li(glue::glue("📄 Total files: {total_files}"))
    cli::cli_end()
    cli::cli_text("")
  }

  cli::cli_alert_info("💡 Use ellipse_discover(con, 'exact_dataset_name') for detailed information")
  invisible(result)
}

#' Format public datalake discovery results for specific dataset
#'
#' @param con Database connection object
#' @param dataset_name Exact dataset name
#' @return List with detailed dataset information
#' @keywords internal
format_public_datalake_dataset_details <- function(con, dataset_name) {
  # Single comprehensive query to get all dataset information
  comprehensive_query <- paste0('
    SELECT
      name as table_name,
      tag,
      file_count,
      creation_date,
      consent_expiry_date,
      data_destruction_date,
      sensitivity_level,
      ethical_stamp,
      regexp_extract(user_metadata_json, \'"([^"]+)"\', 1) as user_metadata_fields,
      regexp_extract(user_metadata_json, \':\\\\s*"([^"]+)"\', 1) as user_metadata_values,
      regexp_replace(file_paths, \'s3://[^/]+/\', \'\') as file_paths,
      COUNT(*) OVER () as total_rows,
      SUM(CAST(file_count AS INTEGER)) OVER () as total_files
    FROM "public-data-lake-content"
    WHERE name = \'', dataset_name, "'
    ORDER BY tag")

  result <- DBI::dbGetQuery(con, comprehensive_query)

  # Check if dataset exists
  if (nrow(result) == 0) {
    cli::cli_alert_danger(glue::glue("❌ Dataset not found: {dataset_name}"))
    return(invisible(NULL))
  }

  # Create nice output with icons
  cli::cli_h2(glue::glue("📊 Dataset Details: {dataset_name}"))
  cli::cli_text("")

  # Basic information
  cli::cli_h3("📋 Overview")
  table_name <- result[1, "table_name"]
  unique_tags <- unique(result$tag)
  tags_count <- length(unique_tags)
  tags_list <- paste(unique_tags, collapse = ", ")
  total_files <- as.integer(result[1, "total_files"])

  cli::cli_ul()
  cli::cli_li(glue::glue("� Dataset: {table_name}"))
  cli::cli_li(glue::glue("🏷️  Tags: {tags_count} ({tags_list})"))
  cli::cli_li(glue::glue("📄 Total files: {total_files}"))
  cli::cli_end()
  cli::cli_text("")

  # Tags details
  cli::cli_h3("🏷️  Tag Details")
  for (i in seq_len(nrow(result))) {
    tag <- result[i, "tag"]
    file_count <- result[i, "file_count"]
    creation_date <- result[i, "creation_date"]
    sensitivity_level <- result[i, "sensitivity_level"]
    consent_expiry_date <- result[i, "consent_expiry_date"]
    data_destruction_date <- result[i, "data_destruction_date"]
    ethical_stamp <- result[i, "ethical_stamp"]
    file_paths <- result[i, "file_paths"]

    cli::cli_h4(glue::glue("Tag: {tag}"))
    cli::cli_ul()
    cli::cli_li(glue::glue("📄 Files: {file_count}"))
    cli::cli_li(glue::glue("📅 Created: {creation_date}"))
    cli::cli_li(glue::glue("⚠️  Sensitivity: Level {sensitivity_level}"))
    cli::cli_li(glue::glue("🔒 Consent expires: {consent_expiry_date}"))
    cli::cli_li(glue::glue("🗑️  Data destruction: {data_destruction_date}"))
    cli::cli_li(glue::glue("✅ Ethical stamp: {ethical_stamp}"))
    if (!is.na(file_paths) && nzchar(file_paths)) {
      cli::cli_li(glue::glue("📂 Files: {file_paths}"))
    }
    cli::cli_end()
    cli::cli_text("")
  }

  cli::cli_alert_info(glue::glue("💡 Use ellipse_discover(con, '{dataset_name}', 'tag_name') for specific tag details"))
  invisible(result)
}

#' Format public datalake discovery results for specific dataset and tag
#'
#' @param con Database connection object
#' @param dataset_name Exact dataset name
#' @param tag_name Exact tag name
#' @return List with specific tag information
#' @keywords internal
format_public_datalake_tag_details <- function(con, dataset_name, tag_name) {
  # Single comprehensive query to get all tag information
  comprehensive_query <- paste0('
    SELECT
      name,
      tag,
      file_count,
      creation_date,
      consent_expiry_date,
      data_destruction_date,
      sensitivity_level,
      ethical_stamp,
      user_metadata_json,
      regexp_replace(file_paths, \'s3://[^/]+/\', \'\') as file_paths,
      file_names,
      file_extensions,
      file_sizes_bytes
    FROM "public-data-lake-content"
    WHERE name = \'', dataset_name, "' AND tag = '", tag_name, "'")

  result <- DBI::dbGetQuery(con, comprehensive_query)

  # Check if dataset and tag combination exists
  if (nrow(result) == 0) {
    cli::cli_alert_danger("La combinaison table/tag demandée est inconnue.")
    return(invisible(NULL))
  }

  tag_result <- result[1, ]

  # Parse user metadata JSON to extract only the custom fields
  user_metadata <- NULL
  if (!is.null(tag_result$user_metadata_json) && nzchar(tag_result$user_metadata_json)) {
    tryCatch(
      {
        full_metadata <- jsonlite::fromJSON(tag_result$user_metadata_json)
        # Extract only the user_metadata_json content (the nested JSON)
        if ("user_metadata_json" %in% names(full_metadata)) {
          user_metadata <- jsonlite::fromJSON(full_metadata$user_metadata_json)
        } else {
          # If no nested structure, look for custom fields (not system fields)
          system_fields <- c(
            "data_destruction_date", "creation_date", "ethical_stamp",
            "consent_expiry_date", "sensitivity_level"
          )
          user_metadata <- full_metadata[!names(full_metadata) %in% system_fields]
          if (length(user_metadata) == 0) user_metadata <- NULL
        }
      },
      error = function(e) {
        user_metadata <<- NULL
      }
    )
  }

  # Create a simple files summary from the JSON arrays
  files_summary <- data.frame(
    dataset = tag_result$name,
    tag = tag_result$tag,
    file_paths_json = tag_result$file_paths,
    file_names_json = tag_result$file_names,
    total_files = as.integer(tag_result$file_count),
    stringsAsFactors = FALSE
  )

  # Enhanced CLI output instead of raw list
  name <- tag_result$name
  tag <- tag_result$tag
  file_count <- as.integer(tag_result$file_count)
  creation_date <- tag_result$creation_date
  sensitivity_level <- as.integer(tag_result$sensitivity_level)
  ethical_stamp <- as.logical(tag_result$ethical_stamp)
  consent_expiry_date <- tag_result$consent_expiry_date
  data_destruction_date <- tag_result$data_destruction_date

  cli::cli_h2(glue::glue("🏷️  Tag Details: {name} / {tag}"))

  # Basic information
  cli::cli_h3("📋 Overview")
  cli::cli_ul()
  cli::cli_li(glue::glue("📄 Total files: {file_count}"))
  cli::cli_li(glue::glue("📅 Creation date: {creation_date}"))
  cli::cli_li(glue::glue("🔒 Sensitivity level: {sensitivity_level}"))
  cli::cli_li(glue::glue("✅ Ethical stamp: {ethical_stamp}"))
  cli::cli_end()
  cli::cli_text("")

  # Dates information
  cli::cli_h3("📅 Important Dates")
  cli::cli_ul()
  if (!is.na(consent_expiry_date) && nzchar(consent_expiry_date)) {
    cli::cli_li(glue::glue("⏰ Consent expiry: {consent_expiry_date}"))
  }
  if (!is.na(data_destruction_date) && nzchar(data_destruction_date)) {
    cli::cli_li(glue::glue("🗑️  Data destruction: {data_destruction_date}"))
  }
  cli::cli_end()
  cli::cli_text("")

  # Custom metadata (if any)
  if (!is.null(user_metadata) && length(user_metadata) > 0) {
    cli::cli_h3("🏷️  Custom Metadata")
    cli::cli_ul()
    for (field_name in names(user_metadata)) {
      field_value <- user_metadata[[field_name]]
      cli::cli_li(glue::glue("{field_name}: {field_value}"))
    }
    cli::cli_end()
    cli::cli_text("")
  }

  # File information
  cli::cli_h3("📁 Files Overview")
  cli::cli_alert_info("Files located in cleaned paths (S3 bucket prefix removed)")

  # Return invisible file summary for programmatic use if needed
  invisible(files_summary)
}
