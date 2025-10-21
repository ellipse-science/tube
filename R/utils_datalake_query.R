# Utility functions for public datalake query operations
# These functions support the ellipse_query() file aggregator functionality

#' File aggregator mode for public datalake connections
#' @param con Database connection to public datalake
#' @param dataset Dataset name to aggregate
#' @param tag Optional tag to filter files
#' @param file Optional specific filename to download/display
#' @keywords internal
ellipse_query_datalake_aggregator <- function(con, dataset, tag = NULL, file = NULL) {
  logger::log_debug(paste("[ellipse_query_datalake_aggregator] entering with dataset =", dataset, ", tag =", tag, ", file =", file))

  tryCatch(
    {
      # Get AWS credentials from connection
      env <- DBI::dbGetInfo(con)$profile_name
      creds <- get_aws_credentials(env)

      # Step 1: Get file metadata from public datalake
      files_metadata <- get_datalake_files_metadata(con, dataset, tag)

      if (nrow(files_metadata) == 0) {
        if (!is.null(tag)) {
          cli::cli_alert_warning("Aucun fichier trouvé pour le dataset '{dataset}' avec le tag '{tag}'.")
        } else {
          cli::cli_alert_warning("Aucun fichier trouvé pour le dataset '{dataset}'.")
        }
        return(tibble::tibble())
      }
      
      # Filter to specific file if requested
      if (!is.null(file)) {
        files_metadata <- files_metadata[files_metadata$file_name == file, ]
        if (nrow(files_metadata) == 0) {
          cli::cli_alert_warning("Fichier '{file}' non trouvé dans le dataset '{dataset}'.")
          return(tibble::tibble())
        }
        cli::cli_alert_info("Fichier spécifique sélectionné: {file}")
      }

      # Check if dataset contains images or HTML files
      image_extensions <- c("png", "jpg", "jpeg")
      html_extensions <- c("html", "htm")
      has_images <- any(files_metadata$file_extension %in% image_extensions)
      has_html <- any(files_metadata$file_extension %in% html_extensions)
      
      # Handle mixed media datasets (images + HTML)
      if (has_images && has_html) {
        # Handle both image and HTML files
        handle_image_dataset(files_metadata, creds, dataset, tag)
        handle_html_dataset(files_metadata, creds, dataset, tag)
        return(invisible(files_metadata))
      }
      
      if (has_images) {
        # Handle image files only
        return(handle_image_dataset(files_metadata, creds, dataset, tag))
      }
      
      if (has_html) {
        # Handle HTML files only
        return(handle_html_dataset(files_metadata, creds, dataset, tag))
      }

      # Step 2: Download and read files (for non-image datasets)
      if (!is.null(tag)) {
        cli::cli_alert_info(
          "Agrégation de {nrow(files_metadata)} fichier(s) du dataset '{dataset}' avec le tag '{tag}'..."
        )
      } else {
        cli::cli_alert_info("Agrégation de {nrow(files_metadata)} fichier(s) du dataset '{dataset}' (tous les tags)...")
      }

      result <- download_and_aggregate_files(files_metadata, creds)

      # Step 3: Add metadata info
      if (!is.null(tag)) {
        cli::cli_alert_success("Agrégation terminée: {nrow(result)} lignes depuis le tag '{tag}'.")
      } else {
        cli::cli_alert_success("Agrégation terminée: {nrow(result)} lignes depuis tous les tags.")
      }

      return(result)
    },
    error = function(e) {
      logger::log_error(paste("[ellipse_query_datalake_aggregator] error:", e$message))
      cli::cli_alert_danger("Erreur lors de l'agrégation des fichiers: {e$message}")
      NULL
    }
  )
}

#' Get file metadata from public datalake for a specific dataset
#' @param con Database connection
#' @param dataset Dataset name
#' @param tag Optional tag filter
#' @keywords internal
get_datalake_files_metadata <- function(con, dataset, tag = NULL) {
  # Build the query based on tag parameter
  if (is.null(tag)) {
    query <- sprintf('SELECT name, tag, file_paths, file_names, file_extensions, file_sizes_bytes
              FROM "public-data-lake-content"
              WHERE name = \'%s\'
              ORDER BY tag', dataset)
    result <- DBI::dbGetQuery(con, query)
  } else {
    query <- sprintf('SELECT name, tag, file_paths, file_names, file_extensions, file_sizes_bytes
              FROM "public-data-lake-content"
              WHERE name = \'%s\' AND tag = \'%s\'
              ORDER BY tag', dataset, tag)
    result <- DBI::dbGetQuery(con, query)
  }

  if (nrow(result) == 0) {
    return(tibble::tibble())
  }

  # Expand file arrays into individual file records
  expanded_files <- purrr::map_dfr(seq_len(nrow(result)), function(i) {
    row <- result[i, ]

    file_paths <- jsonlite::fromJSON(row$file_paths)
    file_names <- jsonlite::fromJSON(row$file_names)
    file_extensions <- jsonlite::fromJSON(row$file_extensions)
    file_sizes <- jsonlite::fromJSON(row$file_sizes_bytes)

    tibble::tibble(
      dataset = row$name,
      tag = row$tag,
      file_path = file_paths,
      file_name = file_names,
      file_extension = file_extensions,
      file_size_bytes = file_sizes
    )
  })

  return(expanded_files)
}

#' Download files from S3 and aggregate into single dataframe
#' @param files_metadata Tibble with file metadata
#' @param credentials AWS credentials from get_aws_credentials()
#' @keywords internal
download_and_aggregate_files <- function(files_metadata, credentials) {
  # Calculate total size and show warning if > 1GB
  total_size_bytes <- sum(files_metadata$file_size_bytes, na.rm = TRUE)
  total_size_gb <- round(total_size_bytes / (1024^3), 2)

  if (total_size_gb >= 1) {
    cli::cli_alert_warning(
      "Attention: Taille totale des données = {total_size_gb} GB. Le traitement peut prendre du temps."
    )
  } else {
    total_size_mb <- total_size_bytes / (1024^2)
    if (total_size_mb >= 0.1) {
      # Show MB with 2 decimals if >= 0.1 MB
      cli::cli_alert_info("Taille totale des données: {round(total_size_mb, 2)} MB")
    } else {
      # Show KB for smaller files
      total_size_kb <- round(total_size_bytes / 1024, 1)
      cli::cli_alert_info("Taille totale des données: {total_size_kb} KB")
    }
  }

  # Download and read files
  dataframes <- list()
  failed_files <- character()

  # Initialize progress bar
  total_files <- nrow(files_metadata)
  cli::cli_progress_bar(
    name = "Lecture des fichiers",
    total = total_files,
    format = "{cli::pb_name} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
    clear = TRUE
  )

  for (i in seq_len(nrow(files_metadata))) {
    file_info <- files_metadata[i, ]

    # Process file based on its extension
    tryCatch(
      {
        # Download file to temp location
        temp_file <- download_s3_file_to_temp(file_info$file_path, credentials)

        # Read file based on extension
        df <- read_file_by_extension(temp_file, file_info$file_extension)

        Sys.sleep(0.2)

        # Add metadata columns
        df$..dataset.. <- file_info$dataset
        df$..tag.. <- file_info$tag
        df$..file_name.. <- file_info$file_name

        dataframes[[i]] <- df

        # Cleanup temp file
        unlink(temp_file)
      },
      error = function(e) {
        logger::log_warn(paste(
          "[download_and_aggregate_files] failed to read file:", file_info$file_name, "- error:", e$message
        ))
        failed_files <<- c(failed_files, file_info$file_name)
      }
    )

    cli::cli_progress_update()
  }

  # cli progress bar is closed automatically

  # Report on failed files
  if (length(failed_files) > 0) {
    cli::cli_alert_warning(
      "Impossible de lire {length(failed_files)} fichier(s): {paste(failed_files, collapse = ', ')}"
    )
  }

  # Filter out NULL dataframes (failed reads)
  dataframes <- dataframes[!sapply(dataframes, is.null)]

  if (length(dataframes) == 0) {
    cli::cli_alert_danger("Aucun fichier n'a pu être lu avec succès.")
    return(tibble::tibble())
  }

  # Merge dataframes with schema union
  cli::cli_alert_info("Fusion des schémas de {length(dataframes)} fichier(s)...")
  merged_df <- merge_dataframes_with_schema_union(dataframes)

  merged_df
}

#' Traditional table query mode for datawarehouse/datamarts
#' @param con Database connection
#' @param table Table name to query
#' @keywords internal
ellipse_query_table_mode <- function(con, table) {
  logger::log_debug(paste("[ellipse_query_table_mode] entering function with table = ", table))
  schema_name <- DBI::dbGetInfo(con)$dbms.name

  logger::log_debug(paste("[ellipse_query_table_mode] about to dbGetQuery on schema_name = ", schema_name))
  tables <- DBI::dbGetQuery(
    con, paste0("SHOW TABLES IN ", schema_name)
  )$tab_name

  logger::log_debug("[ellipse_query_table_mode] got tables")

  if (!table %in% tables) {
    logger::log_debug("[ellipse_query_table_mode] table not in tables")
    cli::cli_alert_danger("La table demandée est inconnue.")
    return(NULL)
  }
  logger::log_debug("[ellipse_query_table_mode] returning results")

  r <- tryCatch(
    {
      dplyr::tbl(con, table)
    },
    error = function(e) {
      cli::cli_alert_danger("Oups, il semble que la table n'a pas pu être lue! 😅")
      logger::log_error(paste("[ellipse_query_table_mode] error in dplyr::tbl", e$message))
      NULL
    }
  )

  r
}

#' Handle image dataset queries with interactive selection
#' @param files_metadata Dataframe with file metadata
#' @param credentials AWS credentials
#' @param dataset Dataset name
#' @param tag Tag name (optional)
#' @return NULL (images are displayed directly)
#' @keywords internal
handle_image_dataset <- function(files_metadata, credentials, dataset, tag = NULL) {
  # Filter to only image files
  image_extensions <- c("png", "jpg", "jpeg")
  image_files <- files_metadata[files_metadata$file_extension %in% image_extensions, ]
  
  if (nrow(image_files) == 0) {
    cli::cli_alert_warning("Aucun fichier image trouvé.")
    return(invisible(files_metadata))
  }
  
  # Display available images
  if (!is.null(tag)) {
    cli::cli_h2("🖼️ Images dans le dataset '{dataset}' (tag: '{tag}')")
  } else {
    cli::cli_h2("🖼️ Images dans le dataset '{dataset}' (tous les tags)")
  }
  
  # Create a display table
  display_data <- data.frame(
    `#` = seq_len(nrow(image_files)),
    `Nom` = image_files$file_name,
    `Tag` = image_files$tag,
    `Taille` = sapply(image_files$file_size_bytes, function(x) {
      if (is.na(x) || x == 0) return("N/A")
      if (x < 1024) return(paste(x, "B"))
      if (x < 1024^2) return(paste(round(x / 1024, 1), "KB"))
      if (x < 1024^3) return(paste(round(x / 1024^2, 1), "MB"))
      paste(round(x / 1024^3, 1), "GB")
    }),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # Display the table
  print(display_data, row.names = FALSE)
  cli::cli_text("")
  
  # Auto-display all images (no interactive selection needed)
  cli::cli_alert_info("Téléchargement et affichage de {nrow(image_files)} image(s)...")
  
  for (i in seq_len(nrow(image_files))) {
    selected_file <- image_files[i, ]
    display_image_from_s3(selected_file, credentials)
  }
  
  cli::cli_alert_success("✅ Toutes les images ont été affichées.")
  return(invisible(files_metadata))
}

#' Display an image from S3 using R's built-in viewer
#' @param file_info Single row from files_metadata
#' @param credentials AWS credentials
#' @keywords internal
display_image_from_s3 <- function(file_info, credentials) {
  cli::cli_alert_info("Téléchargement et affichage de: {file_info$file_name}")
  
  tryCatch({
    # Download to temporary file
    temp_file <- download_s3_file_to_temp(file_info$file_path, credentials)
    
    # Load and display image using appropriate method
    display_image_file(temp_file)
    
    # Note: Don't delete temp file immediately as xdg-open runs asynchronously
    # Temp files will be cleaned up when R session ends
    
    cli::cli_alert_success("✅ Image affichée: {file_info$file_name}")
  }, error = function(e) {
    cli::cli_alert_danger("Erreur lors de l'affichage de l'image: {e$message}")
  })
}

#' Handle HTML dataset queries with automatic display
#'
#' Processes HTML datasets by automatically downloading and displaying all HTML files
#' in a web browser. Similar to image handling, this provides a streamlined workflow
#' for informational products that don't require data aggregation.
#'
#' @param files_metadata Dataframe with file metadata (from get_datalake_files_metadata)
#' @param credentials AWS credentials (from get_aws_credentials)
#' @param dataset Dataset name
#' @param tag Tag name (optional). If NULL, processes all tags
#' @return Invisibly returns files_metadata tibble
#' @keywords internal
#' @seealso \code{\link{handle_image_dataset}} for similar image handling
handle_html_dataset <- function(files_metadata, credentials, dataset, tag = NULL) {
  # Filter to only HTML files
  html_extensions <- c("html", "htm")
  html_files <- files_metadata[files_metadata$file_extension %in% html_extensions, ]
  
  if (nrow(html_files) == 0) {
    cli::cli_alert_warning("Aucun fichier HTML trouvé.")
    return(invisible(files_metadata))
  }
  
  # Display available HTML files
  if (!is.null(tag)) {
    cli::cli_h2("📄 Fichiers HTML dans le dataset '{dataset}' (tag: '{tag}')")
  } else {
    cli::cli_h2("📄 Fichiers HTML dans le dataset '{dataset}' (tous les tags)")
  }
  
  # Create a display table
  display_data <- data.frame(
    `#` = seq_len(nrow(html_files)),
    `Nom` = html_files$file_name,
    `Tag` = html_files$tag,
    `Taille` = sapply(html_files$file_size_bytes, function(x) {
      if (is.na(x) || x == 0) return("N/A")
      if (x < 1024) return(paste(x, "B"))
      if (x < 1024^2) return(paste(round(x / 1024, 1), "KB"))
      if (x < 1024^3) return(paste(round(x / 1024^2, 1), "MB"))
      paste(round(x / 1024^3, 1), "GB")
    }),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # Display the table
  print(display_data, row.names = FALSE)
  cli::cli_text("")
  
  # Auto-display all HTML files (no interactive selection needed)
  cli::cli_alert_info("Téléchargement et affichage de {nrow(html_files)} fichier(s) HTML...")
  
  for (i in seq_len(nrow(html_files))) {
    selected_file <- html_files[i, ]
    display_html_from_s3(selected_file, credentials)
  }
  
  cli::cli_alert_success("✅ Tous les fichiers HTML ont été affichés.")
  return(invisible(files_metadata))
}

#' Display an HTML file from S3 using browser
#'
#' Downloads an HTML file from S3 to a temporary location and opens it in the default
#' browser. Automatically cleans up temporary file after display. Similar to
#' display_image_from_s3() but uses browser instead of image viewer.
#'
#' @param file_info Single-row dataframe containing S3 file metadata with columns:
#'   s3_key, filename, and other metadata from Glue catalog
#' @param credentials List containing AWS credentials (access_key_id, secret_access_key,
#'   session_token, region)
#' @return NULL (invisibly). Function called for side effect of displaying HTML in browser
#' @seealso [display_image_from_s3()], [display_html_file()], [handle_html_dataset()]
#' @keywords internal
display_html_from_s3 <- function(file_info, credentials) {
  cli::cli_alert_info("Téléchargement et affichage de: {file_info$file_name}")
  
  tryCatch({
    # Download to temporary file
    temp_file <- download_s3_file_to_temp(file_info$file_path, credentials)
    
    # Load and display HTML using appropriate method
    display_html_file(temp_file)
    
    # Note: Don't delete temp file immediately as browser may need time to load
    # Temp files will be cleaned up when R session ends
    
    cli::cli_alert_success("✅ Fichier HTML affiché: {file_info$file_name}")
  }, error = function(e) {
    cli::cli_alert_danger("Erreur lors de l'affichage du fichier HTML: {e$message}")
  })
}
