# Utility functions for public datalake query operations
# These functions support the ellipse_query() file aggregator functionality

#' File aggregator mode for public datalake connections
#' @param con Database connection to public datalake
#' @param dataset Dataset name to aggregate
#' @param tag Optional tag to filter files
#' @keywords internal
ellipse_query_datalake_aggregator <- function(con, dataset, tag = NULL) {
  logger::log_debug(paste("[ellipse_query_datalake_aggregator] entering with dataset =", dataset, ", tag =", tag))

  tryCatch({
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

    # Step 2: Download and read files
    if (!is.null(tag)) {
      cli::cli_alert_info("Agrégation de {nrow(files_metadata)} fichier(s) du dataset '{dataset}' avec le tag '{tag}'...")
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

  }, error = function(e) {
    logger::log_error(paste("[ellipse_query_datalake_aggregator] error:", e$message))
    cli::cli_alert_danger("Erreur lors de l'agrégation des fichiers: {e$message}")
    NULL
  })
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
    cli::cli_alert_warning("Attention: Taille totale des données = {total_size_gb} GB. Le traitement peut prendre du temps.")
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

  # Download and read files with progress bar
  dataframes <- list()
  failed_files <- character()

  # Create CLI progress bar
  cli::cli_progress_bar(
    "Lecture des fichiers",
    total = nrow(files_metadata),
    format = "{cli::pb_spin} Lecture: {cli::pb_current}/{cli::pb_total} fichiers [{cli::pb_bar}] {cli::pb_percent} ETA: {cli::pb_eta}"
  )

  for (i in seq_len(nrow(files_metadata))) {
    file_info <- files_metadata[i, ]
    
    # Update progress
    cli::cli_progress_update()

    tryCatch({
      # Download file to temp location (suppress any AWS output)
      temp_file <- suppressMessages(suppressWarnings(
        download_s3_file_to_temp(file_info$file_path, credentials)
      ))

      # Read file based on extension (suppress any file reading output)
      df <- suppressMessages(suppressWarnings(
        read_file_by_extension(temp_file, file_info$file_extension)
      ))

      # Add metadata columns
      df$..dataset.. <- file_info$dataset
      df$..tag.. <- file_info$tag
      df$..file_name.. <- file_info$file_name

      dataframes[[i]] <- df

      # Cleanup temp file
      unlink(temp_file)

    }, error = function(e) {
      logger::log_warn(paste("[download_and_aggregate_files] failed to read file:", file_info$file_name, "- error:", e$message))
      failed_files <<- c(failed_files, file_info$file_name)
    })
  }

  # Complete progress bar
  cli::cli_progress_done()

  # Report on failed files
  if (length(failed_files) > 0) {
    cli::cli_alert_warning("Impossible de lire {length(failed_files)} fichier(s): {paste(failed_files, collapse = ', ')}")
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
