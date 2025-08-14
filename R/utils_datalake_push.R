# Utility functions for public datalake push operations
# These functions support the ellipse_push() public datalake upload functionality

#' Upload files to public datalake with metadata
#' @param con Database connection to public datalake
#' @param file_or_folder File path or folder path to upload
#' @param dataset_name Dataset name for the upload
#' @param tag Tag name for the dataset version
#' @param metadata Named list of custom metadata
#' @param interactive Whether to use interactive mode
#' @keywords internal
ellipse_push_datalake_mode <- function(con, file_or_folder, dataset_name = NULL, tag = NULL, 
                                      metadata = NULL, interactive = TRUE) {
  logger::log_debug("[ellipse_push_datalake_mode] entering function")
  
  tryCatch({
    # Get AWS credentials from connection
    env <- DBI::dbGetInfo(con)$profile_name
    creds <- get_aws_credentials(env)
    
    # Interactive mode for missing parameters
    if (interactive) {
      result <- interactive_datalake_push_flow(file_or_folder, dataset_name, tag, metadata)
      file_or_folder <- result$file_or_folder
      dataset_name <- result$dataset_name
      tag <- result$tag
      metadata <- result$metadata
    }
    
    # Validate all parameters
    if (!validate_datalake_push_params(file_or_folder, dataset_name, tag, metadata)) {
      return(invisible(NULL))
    }
    
    # Process files
    files_to_upload <- prepare_files_for_upload(file_or_folder)
    
    # Check total file size
    total_size <- sum(sapply(files_to_upload, function(f) file.info(f)$size), na.rm = TRUE)
    if (total_size > 1024^3) { # 1GB
      cli::cli_alert_warning("⚠️ Taille totale des fichiers: {round(total_size / 1024^3, 2)} GB - c'est volumineux!")
    }
    
    # Upload files to S3
    success <- upload_files_to_public_datalake(creds, files_to_upload, dataset_name, tag, metadata)
    
    if (success) {
      # Trigger lambda indexing
      lambda_success <- invoke_datalake_indexing_lambda(creds)
      
      if (lambda_success) {
        cli::cli_alert_success("✅ Fichiers uploadés et indexation déclenchée avec succès!")
        cli::cli_alert_info("Les données seront disponibles dans ellipse_discover() dans quelques minutes.")
      } else {
        cli::cli_alert_warning("⚠️ Fichiers uploadés mais l'indexation a échoué. Contactez votre ingénieur de données.")
      }
      
      cli::cli_alert_info("N'oubliez pas de vous déconnecter avec ellipse_disconnect(con) 👋")
      return(invisible(files_to_upload))
    } else {
      cli::cli_alert_danger("❌ Erreur lors de l'upload des fichiers.")
      return(invisible(NULL))
    }
    
  }, error = function(e) {
    logger::log_error(paste("[ellipse_push_datalake_mode] error:", e$message))
    cli::cli_alert_danger("Erreur lors de l'upload: {e$message}")
    return(invisible(NULL))
  })
}

#' Interactive flow for collecting push parameters
#' @param file_or_folder Initial file/folder (may be NULL)
#' @param dataset_name Initial dataset name (may be NULL) 
#' @param tag Initial tag (may be NULL)
#' @param metadata Initial metadata (may be NULL)
#' @keywords internal
interactive_datalake_push_flow <- function(file_or_folder, dataset_name, tag, metadata) {
  cli::cli_h1("🚀 Upload vers le Datalake Public")
  cli::cli_text("")
  
  # File/folder selection
  if (is.null(file_or_folder)) {
    cli::cli_h2("📁 Sélection des fichiers")
    cli::cli_text("Formats supportés: CSV, DTA, SAV, RDS, RDA, XLSX, XLS, DAT")
    cli::cli_text("")
    
    file_or_folder <- readline(prompt = "📂 Chemin vers fichier ou dossier: ")
    
    if (!file.exists(file_or_folder)) {
      cli::cli_alert_danger("Le fichier ou dossier spécifié n'existe pas!")
      stop("Fichier introuvable")
    }
  }
  
  # Dataset name
  if (is.null(dataset_name)) {
    cli::cli_h2("🏷️ Nom du dataset")
    cli::cli_text("Choisissez un nom descriptif pour votre dataset")
    cli::cli_text("")
    
    dataset_name <- readline(prompt = "📋 Nom du dataset: ")
    
    if (nchar(dataset_name) == 0) {
      cli::cli_alert_danger("Le nom du dataset est requis!")
      stop("Dataset name manquant")
    }
  }
  
  # Tag
  if (is.null(tag)) {
    cli::cli_h2("🏷️ Tag de version")
    cli::cli_text("Ex: v1.0, 2025-prod, pilot-test")
    cli::cli_text("")
    
    tag <- readline(prompt = "🔖 Tag: ")
    
    if (nchar(tag) == 0) {
      cli::cli_alert_danger("Le tag est requis!")
      stop("Tag manquant")
    }
  }
  
  # Metadata collection
  if (is.null(metadata)) {
    cli::cli_h2("📊 Métadonnées personnalisées")
    metadata <- collect_custom_metadata_interactive()
  }
  
  # Confirmation
  display_upload_summary(file_or_folder, dataset_name, tag, metadata)
  
  if (!ask_yes_no("Confirmer l'upload?")) {
    cli::cli_alert_info("Upload annulé.")
    stop("Upload annulé par l'utilisateur")
  }
  
  list(
    file_or_folder = file_or_folder,
    dataset_name = dataset_name,
    tag = tag,
    metadata = metadata
  )
}

#' Collect custom metadata interactively
#' @keywords internal
collect_custom_metadata_interactive <- function() {
  cli::cli_text("Ajoutez des métadonnées personnalisées (optionnel)")
  cli::cli_text("Exemples: title, authors, year, description, etc.")
  cli::cli_text("Tapez 'done' pour terminer")
  cli::cli_text("")
  
  metadata <- list()
  
  repeat {
    field_name <- readline(prompt = "🏷️ Nom du champ (ou 'done'): ")
    
    if (tolower(field_name) == "done" || nchar(field_name) == 0) {
      break
    }
    
    field_value <- readline(prompt = paste0("📝 Valeur pour '", field_name, "': "))
    
    if (nchar(field_value) > 0) {
      metadata[[field_name]] <- field_value
      cli::cli_alert_success("✅ Ajouté: {field_name} = {field_value}")
    }
  }
  
  if (length(metadata) == 0) {
    cli::cli_text("💡 Aucune métadonnée personnalisée ajoutée")
  }
  
  return(metadata)
}

#' Display upload summary before confirmation
#' @keywords internal
display_upload_summary <- function(file_or_folder, dataset_name, tag, metadata) {
  cli::cli_rule("📋 Résumé de l'upload")
  
  # File info
  if (file.info(file_or_folder)$isdir) {
    files <- list.files(file_or_folder, recursive = TRUE, full.names = TRUE)
    cli::cli_text("📂 Dossier: {file_or_folder}")
    cli::cli_text("📄 Nombre de fichiers: {length(files)}")
  } else {
    cli::cli_text("📄 Fichier: {basename(file_or_folder)}")
  }
  
  cli::cli_text("🏷️ Dataset: {dataset_name}")
  cli::cli_text("🔖 Tag: {tag}")
  
  if (length(metadata) > 0) {
    cli::cli_text("📊 Métadonnées personnalisées:")
    for (name in names(metadata)) {
      cli::cli_text("   • {name}: {metadata[[name]]}")
    }
  } else {
    cli::cli_text("📊 Aucune métadonnée personnalisée")
  }
  
  cli::cli_rule()
}

#' Validate parameters for datalake push
#' @keywords internal
validate_datalake_push_params <- function(file_or_folder, dataset_name, tag, metadata) {
  # Check file/folder exists
  if (is.null(file_or_folder) || !file.exists(file_or_folder)) {
    cli::cli_alert_danger("Le fichier ou dossier spécifié n'existe pas!")
    return(FALSE)
  }
  
  # Check dataset name
  if (is.null(dataset_name) || nchar(dataset_name) == 0) {
    cli::cli_alert_danger("Le nom du dataset est requis!")
    return(FALSE)
  }
  
  # Check tag
  if (is.null(tag) || nchar(tag) == 0) {
    cli::cli_alert_danger("Le tag est requis!")
    return(FALSE)
  }
  
  # Validate metadata structure
  if (!is.null(metadata) && !is.list(metadata)) {
    cli::cli_alert_danger("Les métadonnées doivent être une liste nommée!")
    return(FALSE)
  }
  
  return(TRUE)
}

#' Prepare files for upload (handle both files and folders)
#' @keywords internal
prepare_files_for_upload <- function(file_or_folder) {
  if (file.info(file_or_folder)$isdir) {
    # Get all files in directory
    files <- list.files(file_or_folder, recursive = TRUE, full.names = TRUE)
    
    # Filter for supported formats
    supported_extensions <- c("csv", "dta", "sav", "rds", "rda", "xlsx", "xls", "dat")
    files <- files[tools::file_ext(tolower(files)) %in% supported_extensions]
    
    if (length(files) == 0) {
      cli::cli_alert_danger("Aucun fichier de format supporté trouvé dans le dossier!")
      return(character(0))
    }
    
    cli::cli_alert_info("📁 {length(files)} fichier(s) trouvé(s) dans le dossier")
    return(files)
  } else {
    # Single file
    extension <- tools::file_ext(tolower(file_or_folder))
    supported_extensions <- c("csv", "dta", "sav", "rds", "rda", "xlsx", "xls", "dat")
    
    if (!extension %in% supported_extensions) {
      cli::cli_alert_danger("Format de fichier non supporté: {extension}")
      return(character(0))
    }
    
    return(file_or_folder)
  }
}

#' Upload files to public datalake S3 bucket
#' @keywords internal
upload_files_to_public_datalake <- function(creds, files, dataset_name, tag, metadata) {
  logger::log_debug("[upload_files_to_public_datalake] entering function")
  
  # Get public datalake bucket
  bucket <- list_public_datalake_bucket(creds)
  
  if (is.null(bucket)) {
    cli::cli_alert_danger("Impossible de trouver le bucket du datalake public!")
    return(FALSE)
  }
  
  # Setup S3 client
  s3_client <- paws.storage::s3(config = c(creds, close_connection = TRUE))
  
  cli::cli_alert_info("📤 Upload en cours vers s3://{bucket}")
  
  # Create progress bar
  pb <- progress::progress_bar$new(
    format = "  uploading [:bar] :percent (:current/:total) eta: :eta",
    total = length(files),
    clear = FALSE,
    width = 70
  )
  
  success_count <- 0
  
  for (file_path in files) {
    tryCatch({
      # Generate S3 key: dataset_name/tag/filename
      filename <- basename(file_path)
      s3_key <- paste0(dataset_name, "/", tag, "/", filename)
      
      # Prepare metadata for S3
      s3_metadata <- prepare_s3_metadata(metadata)
      
      # Upload file
      s3_client$put_object(
        Bucket = bucket,
        Key = s3_key,
        Body = file_path,
        Metadata = s3_metadata,
        ContentType = get_content_type(file_path)
      )
      
      success_count <- success_count + 1
      logger::log_debug(paste("[upload_files_to_public_datalake] uploaded:", s3_key))
      
    }, error = function(e) {
      logger::log_error(paste("[upload_files_to_public_datalake] failed to upload:", file_path, "-", e$message))
      cli::cli_alert_danger("❌ Erreur upload: {basename(file_path)}")
      return(FALSE)  # Stop on first error
    })
    
    pb$tick()
  }
  
  if (success_count == length(files)) {
    cli::cli_alert_success("✅ {success_count} fichier(s) uploadé(s) avec succès!")
    return(TRUE)
  } else {
    cli::cli_alert_danger("❌ Échec upload: {success_count}/{length(files)} fichiers uploadés")
    return(FALSE)
  }
}

#' Prepare metadata for S3 upload
#' @keywords internal
prepare_s3_metadata <- function(custom_metadata) {
  # System metadata (required by the platform)
  s3_metadata <- list(
    "creation-date" = format(Sys.Date(), "%Y-%m-%d"),
    "consent-expiry-date" = format(Sys.Date() + 365, "%Y-%m-%d"), # Default 1 year
    "data-destruction-date" = format(Sys.Date() + 365*10, "%Y-%m-%d"), # Default 10 years
    "sensitivity-level" = "2", # Default medium sensitivity
    "ethical-stamp" = "true" # Default approved
  )
  
  # Add custom metadata
  if (!is.null(custom_metadata) && length(custom_metadata) > 0) {
    # Convert custom metadata to JSON string
    custom_json <- jsonlite::toJSON(custom_metadata, auto_unbox = TRUE)
    s3_metadata[["user-metadata-json"]] <- custom_json
  }
  
  return(s3_metadata)
}

#' Get appropriate content type for file
#' @keywords internal
get_content_type <- function(file_path) {
  extension <- tools::file_ext(tolower(file_path))
  
  content_types <- list(
    "csv" = "text/csv",
    "dta" = "application/x-stata-data",
    "sav" = "application/x-spss-data", 
    "rds" = "application/x-r-data",
    "rda" = "application/x-r-data",
    "xlsx" = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    "xls" = "application/vnd.ms-excel",
    "dat" = "application/octet-stream"
  )
  
  return(content_types[[extension]] %||% "application/octet-stream")
}

#' Invoke AWS Lambda function to index public datalake content
#' @keywords internal
invoke_datalake_indexing_lambda <- function(creds) {
  logger::log_debug("[invoke_datalake_indexing_lambda] entering function")
  
  tryCatch({
    # Setup Lambda client
    lambda_client <- paws.compute::lambda(config = creds)
    
    # Invoke the lambda function (no payload needed)
    result <- lambda_client$invoke(
      FunctionName = "public-data-lake-content-lambda",
      InvocationType = "Event"  # Async invocation
    )
    
    success <- result$StatusCode == 202  # Accepted for async
    
    if (success) {
      logger::log_debug("[invoke_datalake_indexing_lambda] lambda invoked successfully")
      cli::cli_alert_info("🔄 Indexation déclenchée...")
    } else {
      logger::log_error(paste("[invoke_datalake_indexing_lambda] lambda invocation failed, status:", result$StatusCode))
    }
    
    return(success)
    
  }, error = function(e) {
    logger::log_error(paste("[invoke_datalake_indexing_lambda] error:", e$message))
    cli::cli_alert_warning("⚠️ Erreur lors du déclenchement de l'indexation: {e$message}")
    return(FALSE)
  })
}
