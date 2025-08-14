#' Format public datalake discovery results for all datasets
#'
#' @param con Database connection object
#' @return Formatted tibble with all public datalake datasets
#' @keywords internal
format_public_datalake_all_datasets <- function(con) {
  # Query the public datalake table for all datasets
  query <- 'SELECT name as table_name, tag, file_count, creation_date, consent_expiry_date,
            data_destruction_date, sensitivity_level, ethical_stamp,
            user_metadata_json
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

  # Create overview data frame with proper formatting
  overview_data <- data.frame(
    Property = c("📋Dataset", "🏷️ Tags", "📄Total files"),
    Value = c(table_name, paste(tags_count, "(", tags_list, ")"), total_files),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Format columns with proper alignment and print
  formatted_output <- paste(
    format(overview_data$Property, width = max(nchar(overview_data$Property)), justify = "left"),
    format(overview_data$Value, width = max(nchar(overview_data$Value)), justify = "left"),
    sep = "  "
  )
  cat(formatted_output, sep = "\n")
  cli::cli_text("")

  # Tags details
  cli::cli_h3("🏷️  Tag Details")
  cli::cli_text("")

  # Create a clean data frame with icons integrated into values for tag details
  display_tag_details <- data.frame(
    `🏷️ Tag` = result$tag,
    `📄Files` = result$file_count,
    `📅Created` = result$creation_date,
    `⚠️ Sensitivity Level` = result$sensitivity_level,
    `🔒Consent Expires` = result$consent_expiry_date,
    `🗑️ Data Destruction` = result$data_destruction_date,
    `✅Ethical Stamp` = result$ethical_stamp,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Add file paths column if any exist
  if (any(!is.na(result$file_paths) & nzchar(result$file_paths))) {
    display_tag_details$`📂File Paths` <- ifelse(
      !is.na(result$file_paths) & nzchar(result$file_paths),
      result$file_paths,
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
        # Parse the user metadata JSON directly (no nested structure after lambda fix)
        custom_metadata <- jsonlite::fromJSON(row$user_metadata_json)

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
        Tag = paste("🏷️ Tag", tag_name),
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

    # Combine all metadata rows and format with proper alignment
    metadata_display <- do.call(rbind, metadata_display_list)

    # Format columns with proper alignment and print
    formatted_metadata <- paste(
      format(metadata_display$Tag, width = max(nchar(metadata_display$Tag)), justify = "left"),
      format(metadata_display$Field, width = max(nchar(metadata_display$Field)), justify = "left"),
      format(metadata_display$Value, width = max(nchar(metadata_display$Value)), justify = "left"),
      sep = "  "
    )
    cat(formatted_metadata, sep = "\n")
    cli::cli_text("")
  }

  cli::cli_rule()

  cli::cli_alert_info(glue::glue("💡 Use ellipse_discover(con, '{dataset_name}', 'tag_name') for specific tag details"))
  invisible(result)
}

#' Format public datalake discovery results for specific dataset and tag (BACKUP - DETAILED VERSION)
#'
#' @param con Database connection object
#' @param dataset_name Exact dataset name
#' @param tag_name Exact tag name
#' @return List with specific tag information
#' @keywords internal
format_public_datalake_tag_details_detailed <- function(con, dataset_name, tag_name) {
  # Query to get all individual file records for this dataset/tag combination
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

  # Display header
  cli::cli_h2(glue::glue("🏷️  Tag Details: {dataset_name} / {tag_name}"))
  cli::cli_text("")

  # Parse all file arrays from all matching records
  all_files_data <- list()

  for (row_idx in seq_len(nrow(result))) {
    tag_result <- result[row_idx, ]

    tryCatch({
      # Parse file arrays for this record
      file_paths <- if (!is.null(tag_result$file_paths) && nzchar(tag_result$file_paths)) {
        jsonlite::fromJSON(tag_result$file_paths)
      } else {
        character(0)
      }

      file_names <- if (!is.null(tag_result$file_names) && nzchar(tag_result$file_names)) {
        jsonlite::fromJSON(tag_result$file_names)
      } else {
        character(0)
      }

      file_extensions <- if (!is.null(tag_result$file_extensions) && nzchar(tag_result$file_extensions)) {
        jsonlite::fromJSON(tag_result$file_extensions)
      } else {
        character(0)
      }

      file_sizes <- if (!is.null(tag_result$file_sizes_bytes) && nzchar(tag_result$file_sizes_bytes)) {
        jsonlite::fromJSON(tag_result$file_sizes_bytes)
      } else {
        numeric(0)
      }

      # Parse user metadata for this record
      user_metadata <- NULL
      if (!is.null(tag_result$user_metadata_json) && nzchar(tag_result$user_metadata_json)) {
        tryCatch({
          # After lambda fix, user_metadata_json contains only custom metadata directly
          user_metadata <- jsonlite::fromJSON(tag_result$user_metadata_json)
          if (length(user_metadata) == 0) user_metadata <- NULL
        }, error = function(e) {
          user_metadata <<- NULL
        })
      }

      # Store file data with metadata for each file in this record
      num_files <- max(length(file_paths), length(file_names), length(file_extensions), length(file_sizes))

      if (num_files > 0) {
        for (file_idx in seq_len(num_files)) {
          file_data <- list(
            path = if (file_idx <= length(file_paths)) file_paths[file_idx] else "",
            name = if (file_idx <= length(file_names)) file_names[file_idx] else "",
            extension = if (file_idx <= length(file_extensions)) file_extensions[file_idx] else "",
            size_bytes = if (file_idx <= length(file_sizes)) file_sizes[file_idx] else 0,
            creation_date = tag_result$creation_date,
            sensitivity_level = tag_result$sensitivity_level,
            ethical_stamp = tag_result$ethical_stamp,
            consent_expiry_date = tag_result$consent_expiry_date,
            data_destruction_date = tag_result$data_destruction_date,
            user_metadata = user_metadata
          )
          all_files_data <- append(all_files_data, list(file_data))
        }
      }
    }, error = function(e) {
      cli::cli_alert_warning(paste("Error processing record", row_idx, ":", e$message))
    })
  }

  if (length(all_files_data) == 0) {
    cli::cli_alert_info("No files found in this tag")
    return(invisible(NULL))
  }

  # Display summary
  cli::cli_h3("� Tag Summary")
  total_size <- sum(sapply(all_files_data, function(f) as.numeric(f$size_bytes)), na.rm = TRUE)
  if (total_size >= 1024^3) {
    total_size_display <- paste(round(total_size / 1024^3, 2), "GB")
  } else if (total_size >= 1024^2) {
    total_size_display <- paste(round(total_size / 1024^2, 2), "MB")
  } else if (total_size >= 1024) {
    total_size_display <- paste(round(total_size / 1024, 2), "KB")
  } else {
    total_size_display <- paste(total_size, "bytes")
  }

  summary_data <- data.frame(
    Property = c("📄Total files", "�Total size"),
    Value = c(length(all_files_data), total_size_display),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  formatted_summary <- paste(
    format(summary_data$Property, width = max(nchar(summary_data$Property)), justify = "left"),
    format(summary_data$Value, width = max(nchar(summary_data$Value)), justify = "left"),
    sep = "  "
  )
  cat(formatted_summary, sep = "\n")
  cli::cli_text("")
  cli::cli_rule()

  # Display detailed information for each file
  cli::cli_h3("📁 Individual File Details")
  cli::cli_text("")

  for (i in seq_along(all_files_data)) {
    file_data <- all_files_data[[i]]

    # File header using cli_text with formatting
    cli::cli_text(paste("📄", cli::style_bold(paste("File", i))))
    cli::cli_text("")

    # Basic file information
    size_bytes <- as.numeric(file_data$size_bytes)
    if (size_bytes >= 1024^3) {
      size_display <- paste(round(size_bytes / 1024^3, 2), "GB")
    } else if (size_bytes >= 1024^2) {
      size_display <- paste(round(size_bytes / 1024^2, 2), "MB")
    } else if (size_bytes >= 1024) {
      size_display <- paste(round(size_bytes / 1024, 2), "KB")
    } else {
      size_display <- paste(size_bytes, "bytes")
    }

    # File basic info
    file_info <- data.frame(
      Property = c("📂Path", "📝Name", "🏷️ Extension", "📏Size"),
      Value = c(
        ifelse(nzchar(file_data$path), file_data$path, "N/A"),
        ifelse(nzchar(file_data$name), file_data$name, "N/A"),
        ifelse(nzchar(file_data$extension), file_data$extension, "N/A"),
        size_display
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    formatted_file_info <- paste(
      format(file_info$Property, width = max(nchar(file_info$Property)), justify = "left"),
      format(file_info$Value, width = max(nchar(file_info$Value)), justify = "left"),
      sep = "  "
    )
    cat(formatted_file_info, sep = "\n")
    cli::cli_text("")

    # File metadata
    metadata_info <- data.frame(
      Property = character(0),
      Value = character(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    if (!is.na(file_data$creation_date) && nzchar(file_data$creation_date)) {
      metadata_info <- rbind(metadata_info, data.frame(
        Property = "📅Creation date",
        Value = file_data$creation_date,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }

    if (!is.na(file_data$sensitivity_level)) {
      metadata_info <- rbind(metadata_info, data.frame(
        Property = "🔒Sensitivity level",
        Value = paste("Level", file_data$sensitivity_level),
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }

    if (!is.na(file_data$ethical_stamp)) {
      metadata_info <- rbind(metadata_info, data.frame(
        Property = "✅Ethical stamp",
        Value = as.logical(file_data$ethical_stamp),
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }

    if (!is.na(file_data$consent_expiry_date) && nzchar(file_data$consent_expiry_date)) {
      metadata_info <- rbind(metadata_info, data.frame(
        Property = "⏰Consent expiry",
        Value = file_data$consent_expiry_date,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }

    if (!is.na(file_data$data_destruction_date) && nzchar(file_data$data_destruction_date)) {
      metadata_info <- rbind(metadata_info, data.frame(
        Property = "🗑️ Data destruction",
        Value = file_data$data_destruction_date,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }

    if (nrow(metadata_info) > 0) {
      formatted_metadata_info <- paste(
        format(metadata_info$Property, width = max(nchar(metadata_info$Property)), justify = "left"),
        format(metadata_info$Value, width = max(nchar(metadata_info$Value)), justify = "left"),
        sep = "  "
      )
      cat(formatted_metadata_info, sep = "\n")
      cli::cli_text("")
    }

    # Custom metadata for this file
    if (!is.null(file_data$user_metadata) && length(file_data$user_metadata) > 0) {
      cli::cli_text("🏷️  Custom Metadata:")

      tryCatch({
        metadata_fields <- names(file_data$user_metadata)
        metadata_values <- character(length(metadata_fields))

        for (j in seq_along(metadata_fields)) {
          field_value <- file_data$user_metadata[[metadata_fields[j]]]
          if (is.list(field_value) || length(field_value) > 1) {
            metadata_values[j] <- paste(as.character(field_value), collapse = ", ")
          } else {
            metadata_values[j] <- as.character(field_value)
          }
        }

        custom_metadata <- data.frame(
          Field = paste("  ", metadata_fields),  # Indent custom fields
          Value = metadata_values,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )

        formatted_custom <- paste(
          format(custom_metadata$Field, width = max(nchar(custom_metadata$Field)), justify = "left"),
          format(custom_metadata$Value, width = max(nchar(custom_metadata$Value)), justify = "left"),
          sep = "  "
        )
        cat(formatted_custom, sep = "\n")
        cli::cli_text("")
      }, error = function(e) {
        cli::cli_alert_warning("Could not display custom metadata for this file")
      })
    }

    # Separator between files (except for last file)
    if (i < length(all_files_data)) {
      cli::cli_rule()
      cli::cli_text("")
    }
  }

  # Create file summary for programmatic use
  files_summary <- data.frame(
    dataset = rep(dataset_name, length(all_files_data)),
    tag = rep(tag_name, length(all_files_data)),
    file_path = sapply(all_files_data, function(f) f$path),
    file_name = sapply(all_files_data, function(f) f$name),
    file_size_bytes = sapply(all_files_data, function(f) as.numeric(f$size_bytes)),
    stringsAsFactors = FALSE
  )

  invisible(files_summary)
}

#' Format public datalake discovery results for specific dataset and tag (CONDENSED VERSION)
#'
#' @param con Database connection object
#' @param dataset_name Exact dataset name
#' @param tag_name Exact tag name
#' @return List with specific tag information
#' @keywords internal
format_public_datalake_tag_details <- function(con, dataset_name, tag_name) {
  # Query to get all individual file records for this dataset/tag combination
  comprehensive_query <- paste0('
    SELECT
      name, tag, file_count, creation_date, consent_expiry_date, data_destruction_date,
      sensitivity_level, ethical_stamp, user_metadata_json,
      regexp_replace(file_paths, \'s3://[^/]+/\', \'\') as file_paths,
      file_names, file_extensions, file_sizes_bytes
    FROM "public-data-lake-content"
    WHERE name = \'', dataset_name, "' AND tag = '", tag_name, "'")

  result <- DBI::dbGetQuery(con, comprehensive_query)

  if (nrow(result) == 0) {
    cli::cli_alert_danger("La combinaison table/tag demandée est inconnue.")
    return(invisible(NULL))
  }

  cli::cli_h2(glue::glue("🏷️  Tag Details: {dataset_name} / {tag_name}"))

  # Parse all file arrays from all matching records
  all_files_data <- list()

  for (row_idx in seq_len(nrow(result))) {
    tag_result <- result[row_idx, ]

    tryCatch({
      # Parse file arrays
      file_paths <- if (!is.null(tag_result$file_paths) && nzchar(tag_result$file_paths)) {
        jsonlite::fromJSON(tag_result$file_paths)
      } else {
        character(0)
      }

      file_names <- if (!is.null(tag_result$file_names) && nzchar(tag_result$file_names)) {
        jsonlite::fromJSON(tag_result$file_names)
      } else {
        character(0)
      }

      file_extensions <- if (!is.null(tag_result$file_extensions) && nzchar(tag_result$file_extensions)) {
        jsonlite::fromJSON(tag_result$file_extensions)
      } else {
        character(0)
      }

      file_sizes <- if (!is.null(tag_result$file_sizes_bytes) && nzchar(tag_result$file_sizes_bytes)) {
        jsonlite::fromJSON(tag_result$file_sizes_bytes)
      } else {
        numeric(0)
      }

      # Parse user metadata
      user_metadata <- NULL
      if (!is.null(tag_result$user_metadata_json) && nzchar(tag_result$user_metadata_json)) {
        tryCatch({
          # After lambda fix, user_metadata_json contains only custom metadata directly
          user_metadata <- jsonlite::fromJSON(tag_result$user_metadata_json)
          if (length(user_metadata) == 0) user_metadata <- NULL
        }, error = function(e) {
          user_metadata <<- NULL
        })
      }

      # Store file data
      num_files <- max(length(file_paths), length(file_names), length(file_extensions), length(file_sizes))

      if (num_files > 0) {
        for (file_idx in seq_len(num_files)) {
          file_data <- list(
            path = if (file_idx <= length(file_paths)) file_paths[file_idx] else "",
            name = if (file_idx <= length(file_names)) file_names[file_idx] else "",
            extension = if (file_idx <= length(file_extensions)) file_extensions[file_idx] else "",
            size_bytes = if (file_idx <= length(file_sizes)) file_sizes[file_idx] else 0,
            creation_date = tag_result$creation_date,
            sensitivity_level = tag_result$sensitivity_level,
            ethical_stamp = tag_result$ethical_stamp,
            consent_expiry_date = tag_result$consent_expiry_date,
            data_destruction_date = tag_result$data_destruction_date,
            user_metadata = user_metadata
          )
          all_files_data <- append(all_files_data, list(file_data))
        }
      }
    }, error = function(e) {
      cli::cli_alert_warning(paste("Error processing record", row_idx, ":", e$message))
    })
  }

  if (length(all_files_data) == 0) {
    cli::cli_alert_info("No files found in this tag")
    return(invisible(NULL))
  }

  # Display compact summary
  total_size <- sum(sapply(all_files_data, function(f) as.numeric(f$size_bytes)), na.rm = TRUE)
  if (total_size >= 1024^3) {
    total_size_display <- paste(round(total_size / 1024^3, 2), "GB")
  } else if (total_size >= 1024^2) {
    total_size_display <- paste(round(total_size / 1024^2, 2), "MB") 
  } else if (total_size >= 1024) {
    total_size_display <- paste(round(total_size / 1024, 2), "KB")
  } else {
    total_size_display <- paste(total_size, "bytes")
  }

  cli::cli_alert_info(paste("📊", length(all_files_data), "files, Total size:", total_size_display))
  cli::cli_text("")

  # Display compact file information as a table with more details
  files_table_data <- data.frame(
    `📄File` = paste("📄", seq_along(all_files_data)),
    `📝Name` = sapply(all_files_data, function(f) {
      name <- ifelse(nzchar(f$name), f$name, basename(f$path))
      if (nchar(name) > 20) {
        paste0(substr(name, 1, 17), "...")
      } else {
        name
      }
    }),
    `📂Path` = sapply(all_files_data, function(f) {
      path <- ifelse(nzchar(f$path), f$path, "N/A")
      if (nchar(path) > 30) {
        paste0(substr(path, 1, 27), "...")
      } else {
        path
      }
    }),
    `📏Size` = sapply(all_files_data, function(f) {
      size_bytes <- as.numeric(f$size_bytes)
      if (size_bytes >= 1024^2) {
        paste(round(size_bytes / 1024^2, 1), "MB")
      } else if (size_bytes >= 1024) {
        paste(round(size_bytes / 1024, 1), "KB")
      } else {
        paste(size_bytes, "B")
      }
    }),
    `📅Creation Date` = sapply(all_files_data, function(f) {
      if (!is.na(f$creation_date) && nzchar(f$creation_date)) {
        substr(f$creation_date, 1, 10)  # Just date, no time
      } else {
        "N/A"
      }
    }),
    `🔒Sensitivity Level` = sapply(all_files_data, function(f) {
      if (!is.na(f$sensitivity_level)) paste("Level", f$sensitivity_level) else "N/A"
    }),
    `✅Ethical Stamp` = sapply(all_files_data, function(f) {
      if (!is.na(f$ethical_stamp)) {
        ifelse(as.logical(f$ethical_stamp), "✅ Yes", "❌ No")
      } else {
        "N/A"
      }
    }),
    `⏰Consent Expiry` = sapply(all_files_data, function(f) {
      if (!is.na(f$consent_expiry_date) && nzchar(f$consent_expiry_date)) {
        substr(f$consent_expiry_date, 1, 10)
      } else {
        "N/A"
      }
    }),
    `🗑️ Data Destruction` = sapply(all_files_data, function(f) {
      if (!is.na(f$data_destruction_date) && nzchar(f$data_destruction_date)) {
        substr(f$data_destruction_date, 1, 10)
      } else {
        "N/A"
      }
    }),
    `🏷️Custom Metadata` = sapply(all_files_data, function(f) {
      if (!is.null(f$user_metadata) && length(f$user_metadata) > 0) {
        metadata_summary <- paste(names(f$user_metadata)[1:min(2, length(f$user_metadata))], collapse = ",")
        if (length(f$user_metadata) > 2) {
          paste0(metadata_summary, "...")
        } else {
          metadata_summary
        }
      } else {
        "None"
      }
    }),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Print compact table with all details
  print(files_table_data, row.names = FALSE, right = FALSE)
  cli::cli_text("")

  # Show files with custom metadata and their details
  files_with_metadata <- which(sapply(
    all_files_data, function(f) !is.null(f$user_metadata) && length(f$user_metadata) > 0
  ))

  if (length(files_with_metadata) > 0) {
    cli::cli_text("🏷️  Custom Metadata Details:")
    for (file_idx in files_with_metadata) {
      file_data <- all_files_data[[file_idx]]
      metadata_items <- sapply(names(file_data$user_metadata), function(key) {
        value <- file_data$user_metadata[[key]]
        if (is.list(value) || length(value) > 1) {
          value <- paste(as.character(value), collapse = ", ")
        }
        # Truncate long values
        if (nchar(value) > 30) value <- paste0(substr(value, 1, 27), "...")
        paste(key, "=", value)
      })
      cli::cli_text(paste("   File", file_idx, ":", paste(metadata_items, collapse = " | ")))
    }
    cli::cli_text("")
  }

  cli::cli_text("💡 Use format_public_datalake_tag_details_detailed() for full individual file cards")

  # Create file summary for programmatic use
  files_summary <- data.frame(
    dataset = rep(dataset_name, length(all_files_data)),
    tag = rep(tag_name, length(all_files_data)),
    file_path = sapply(all_files_data, function(f) f$path),
    file_name = sapply(all_files_data, function(f) f$name),
    file_size_bytes = sapply(all_files_data, function(f) as.numeric(f$size_bytes)),
    stringsAsFactors = FALSE
  )

  invisible(files_summary)
}
