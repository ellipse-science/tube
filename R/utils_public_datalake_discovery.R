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

  # Create tabular output with proper formatting
  cli::cli_h2("🗂️  Public Datalake - All Datasets")
  cli::cli_text("")

  # Create a summary table first
  dataset_names <- unique(result$table_name)
  dataset_summary <- data.frame(
    table_name = character(0),
    tags_count = integer(0),
    total_files = numeric(0),
    first_created = character(0),
    stringsAsFactors = FALSE
  )

  for (dataset in dataset_names) {
    dataset_rows <- result[result$table_name == dataset, ]
    dataset_summary <- rbind(dataset_summary, data.frame(
      table_name = dataset,
      tags_count = nrow(dataset_rows),
      total_files = sum(as.numeric(dataset_rows$file_count), na.rm = TRUE),
      first_created = min(dataset_rows$creation_date, na.rm = TRUE),
      stringsAsFactors = FALSE
    ))
  }

  cli::cli_h3("📋 Dataset Summary")
  cli::cli_text("")

  # Create a clean data frame with icons integrated into values
  display_summary <- data.frame(
    Dataset = paste("📁", dataset_summary$table_name),
    Tags = dataset_summary$tags_count,
    Files = dataset_summary$total_files,
    `First Created` = dataset_summary$first_created,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Print the data frame as a simple table
  print(display_summary, row.names = FALSE, col.names = FALSE, right = FALSE)

  cli::cli_text("")
  cli::cli_rule()

  # Detailed table with all tags
  cli::cli_h3("🏷️  Detailed Tag Information")
  cli::cli_text("")

  # Create a clean data frame with icons integrated into values
  display_details <- data.frame(
    Dataset = paste("📁", result$table_name),
    Tag = paste("🏷️", result$tag),
    Files = result$file_count,
    Created = result$creation_date,
    Sensitivity = paste("Level", result$sensitivity_level),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Print the data frame as a simple table
  print(display_details, row.names = FALSE, col.names = FALSE, right = FALSE)

  cli::cli_text("")
  cli::cli_rule()
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

  # Create tabular output with proper formatting
  cli::cli_h2(glue::glue("🔍 Search Results for: {pattern}"))
  cli::cli_text("")
  cli::cli_alert_success(glue::glue("Found {nrow(result)} dataset(s) matching pattern"))
  cli::cli_text("")

  # Create a clean data frame with icons integrated into values
  # Truncate long tag lists for better display
  truncated_tags <- sapply(result$tags_list, function(x) {
    if (nchar(x) > 50) paste0(substr(x, 1, 47), "...") else x
  })

  display_pattern <- data.frame(
    Dataset = paste("📁", result$table_name),
    Tags = result$tags_count,
    `Total Files` = result$total_files,
    `Available Tags` = truncated_tags,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Print the data frame as a simple table
  print(display_pattern, row.names = FALSE, right = FALSE)

  cli::cli_text("")
  cli::cli_rule()

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
      user_metadata_json,
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
  # Basic information
  cli::cli_h3("📋 Overview")
  cli::cli_text("")

  table_name <- result[1, "table_name"]
  unique_tags <- unique(result$tag)
  tags_count <- length(unique_tags)
  tags_list <- paste(unique_tags, collapse = ", ")
  total_files <- as.integer(result[1, "total_files"])

  # Create overview data frame
  overview_data <- data.frame(
    Property = c("📋 Dataset", "🏷️ Tags", "📄 Total files"),
    Value = c(table_name, paste(tags_count, "(", tags_list, ")"), total_files),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    col.names = FALSE
  )

  # Print the overview table
  print(overview_data, row.names = FALSE, col.names = FALSE, right = FALSE)
  cli::cli_text("")

  # Tags details
  cli::cli_h3("🏷️  Tag Details")
  cli::cli_text("")

  # Create a clean data frame with icons integrated into values for tag details
  display_tag_details <- data.frame(
    Tag = paste("🏷️", result$tag),
    Files = paste("📄", result$file_count),
    Created = paste("📅", result$creation_date),
    Sensitivity = paste("⚠️ Level", result$sensitivity_level),
    `Consent Expires` = paste("🔒", result$consent_expiry_date),
    `Data Destruction` = paste("🗑️", result$data_destruction_date),
    `Ethical Stamp` = paste("✅", result$ethical_stamp),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Add file paths column if any exist
  if (any(!is.na(result$file_paths) & nzchar(result$file_paths))) {
    display_tag_details$`File Paths` <- ifelse(
      !is.na(result$file_paths) & nzchar(result$file_paths),
      paste("📂", result$file_paths),
      ""
    )
  }

  # Print the data frame as a simple table
  print(display_tag_details, row.names = FALSE, right = FALSE)

  cli::cli_text("")

  # User Metadata Details section
  metadata_by_tag <- list()

  for (i in seq_len(nrow(result))) {
    row <- result[i, ]
    if (!is.null(row$user_metadata_json) && nzchar(row$user_metadata_json)) {
      tryCatch({
        # Parse the main JSON
        full_metadata <- jsonlite::fromJSON(row$user_metadata_json)

        # Only extract the nested user_metadata_json content
        if ("user_metadata_json" %in% names(full_metadata) &&
            !is.null(full_metadata$user_metadata_json) &&
            nzchar(full_metadata$user_metadata_json)) {

          # Parse the nested JSON string to get the actual custom metadata
          custom_metadata <- jsonlite::fromJSON(full_metadata$user_metadata_json)

          # Store metadata fields for this tag
          tag_name <- row$tag
          if (!tag_name %in% names(metadata_by_tag)) {
            metadata_by_tag[[tag_name]] <- list()
          }

          for (field_name in names(custom_metadata)) {
            field_value <- custom_metadata[[field_name]]
            if (is.list(field_value) || length(field_value) > 1) {
              field_value <- paste(as.character(field_value), collapse = ", ")
            }
            metadata_by_tag[[tag_name]][[field_name]] <- as.character(field_value)
          }
        }
      }, error = function(e) {
        # Skip this row if JSON parsing fails
        cat("JSON parsing error for tag", row$tag, ":", e$message, "\n")
      })
    }
  }

  if (length(metadata_by_tag) > 0) {
    cli::cli_h3("🏷️  User Metadata Details")
    cli::cli_text("")

    # Build grouped display data frame
    metadata_display_list <- list()

    for (tag_name in names(metadata_by_tag)) {
      tag_metadata <- metadata_by_tag[[tag_name]]
      field_names <- names(tag_metadata)

      # First row shows the tag name
      metadata_display_list <- append(metadata_display_list, list(data.frame(
        Tag = paste("🏷️", tag_name),
        Field = field_names[1],
        Value = tag_metadata[[field_names[1]]],
        stringsAsFactors = FALSE,
        check.names = FALSE
      )))

      # Subsequent rows for this tag have empty tag column
      if (length(field_names) > 1) {
        for (i in 2:length(field_names)) {
          metadata_display_list <- append(metadata_display_list, list(data.frame(
            Tag = "",
            Field = field_names[i],
            Value = tag_metadata[[field_names[i]]],
            stringsAsFactors = FALSE,
            check.names = FALSE
          )))
        }
      }
    }

    # Combine all metadata rows and print
    metadata_display <- do.call(rbind, metadata_display_list)
    print(metadata_display, row.names = FALSE, col.names = FALSE, right = FALSE)
    cli::cli_text("")
  }

  cli::cli_rule()

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

  # Create overview data frame
  overview_data <- data.frame(
    Property = c("📄 Total files", "📅 Creation date", "🔒 Sensitivity level", "✅ Ethical stamp"),
    Value = c(file_count, creation_date, paste("Level", sensitivity_level), ethical_stamp),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Print the overview table
  print(overview_data, row.names = FALSE, col.names = FALSE, right = FALSE)
  cli::cli_text("")

  # Dates information
  dates_info <- data.frame(
    Property = character(0),
    Value = character(0),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (!is.na(consent_expiry_date) && nzchar(consent_expiry_date)) {
    dates_info <- rbind(dates_info, data.frame(
      Property = "⏰ Consent expiry",
      Value = consent_expiry_date,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  if (!is.na(data_destruction_date) && nzchar(data_destruction_date)) {
    dates_info <- rbind(dates_info, data.frame(
      Property = "🗑️  Data destruction",
      Value = data_destruction_date,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  if (nrow(dates_info) > 0) {
    cli::cli_h3("📅 Important Dates")
    print(dates_info, row.names = FALSE, col.names = FALSE, right = FALSE)
    cli::cli_text("")
  }

  # Custom metadata (if any)
  if (!is.null(user_metadata) && length(user_metadata) > 0) {
    cli::cli_h3("🏷️  Custom Metadata")

    # Safely handle nested or complex metadata structures
    tryCatch({
      metadata_fields <- names(user_metadata)
      metadata_values <- character(length(metadata_fields))

      for (i in seq_along(metadata_fields)) {
        field_value <- user_metadata[[metadata_fields[i]]]
        # Convert complex structures to string safely
        if (is.list(field_value) || length(field_value) > 1) {
          metadata_values[i] <- paste(as.character(field_value), collapse = ", ")
        } else {
          metadata_values[i] <- as.character(field_value)
        }
      }

      metadata_data <- data.frame(
        Field = paste("🏷️", metadata_fields),
        Value = metadata_values,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      print(metadata_data, row.names = FALSE, col.names = FALSE, right = FALSE)
      cli::cli_text("")
    }, error = function(e) {
      # Fallback to simple display if data frame creation fails
      cli::cli_ul()
      for (field_name in names(user_metadata)) {
        field_value <- user_metadata[[field_name]]
        if (is.list(field_value) || length(field_value) > 1) {
          field_value <- paste(as.character(field_value), collapse = ", ")
        }
        cli::cli_li(glue::glue("🏷️ {field_name}: {field_value}"))
      }
      cli::cli_end()
      cli::cli_text("")
    })
  }

  # File information
  cli::cli_h3("📁 Files Overview")
  cli::cli_alert_info("Files located in cleaned paths (S3 bucket prefix removed)")

  # Return invisible file summary for programmatic use if needed
  invisible(files_summary)
}
