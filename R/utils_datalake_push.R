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
    file_or_folder <- interactive_file_folder_selector()
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

#' Interactive file and folder selector with current directory browsing
#' @keywords internal
interactive_file_folder_selector <- function() {
  cli::cli_h2("📁 Sélection intelligente des fichiers")
  cli::cli_text("Formats supportés: CSV, DTA, SAV, RDS, RDA, XLSX, XLS, DAT")
  cli::cli_text("")
  
  current_dir <- getwd()
  cli::cli_text("📂 Répertoire actuel: {cli::col_blue(current_dir)}")
  
  repeat {
    # Show options for selection
    display_file_selection_menu(current_dir)
    
    choice <- readline(prompt = "👆 Votre choix: ")
    
    # Handle the choice
    result <- process_file_selection_choice(choice, current_dir)
    
    if (!is.null(result)) {
      if (result == "navigate") {
        # User chose to navigate - continue the loop
        current_dir <- getwd()  # Update current directory
        next
      } else {
        # User made a valid selection
        return(result)
      }
    }
  }
}

#' Display file selection menu with current directory contents
#' @keywords internal
display_file_selection_menu <- function(current_dir) {
  cli::cli_rule("📋 Options de sélection")
  
  # Show current directory contents
  items <- list.files(current_dir, include.dirs = TRUE, no.. = TRUE)
  
  if (length(items) == 0) {
    cli::cli_text("💡 Répertoire vide")
  } else {
    # Separate files and directories
    full_paths <- file.path(current_dir, items)
    is_dir <- file.info(full_paths)$isdir
    
    dirs <- items[is_dir]
    files <- items[!is_dir]
    
    # Show supported files first
    supported_exts <- c("\\.csv$", "\\.dta$", "\\.sav$", "\\.rds$", "\\.rda$", 
                       "\\.xlsx$", "\\.xls$", "\\.dat$")
    supported_pattern <- paste(supported_exts, collapse = "|")
    
    supported_files <- files[grepl(supported_pattern, files, ignore.case = TRUE)]
    other_files <- files[!grepl(supported_pattern, files, ignore.case = TRUE)]
    
    # Display supported files
    if (length(supported_files) > 0) {
      cli::cli_h3("📄 Fichiers supportés")
      for (i in seq_along(supported_files)) {
        file_path <- file.path(current_dir, supported_files[i])
        file_size <- format_file_size(file.info(file_path)$size)
        cli::cli_text("  {cli::col_green(i)}. {supported_files[i]} {cli::col_silver('({file_size})')}")
      }
      cli::cli_text("")
    }
    
    # Display directories
    if (length(dirs) > 0) {
      cli::cli_h3("📁 Dossiers")
      dir_start <- length(supported_files)
      for (i in seq_along(dirs)) {
        dir_num <- dir_start + i
        dir_path <- file.path(current_dir, dirs[i])
        file_count <- length(list.files(dir_path, recursive = TRUE))
        cli::cli_text("  {cli::col_blue(dir_num)}. {dirs[i]}/ {cli::col_silver('({file_count} fichiers)')}")
      }
      cli::cli_text("")
    }
    
    # Display other files (collapsed)
    if (length(other_files) > 0) {
      cli::cli_h3("📎 Autres fichiers")
      cli::cli_text("  {cli::col_silver(paste(length(other_files), 'fichier(s) non-supporté(s)'))}")
      cli::cli_text("")
    }
  }
  
  # Show navigation and action options
  cli::cli_h3("🧭 Navigation et actions")
  cli::cli_text("  {cli::col_yellow('p')}. 📁 Naviguer vers le dossier parent")
  cli::cli_text("  {cli::col_yellow('cd')}. 📂 Changer de répertoire (tapez le chemin)")
  cli::cli_text("  {cli::col_yellow('.')}. 📂 Sélectionner le dossier actuel")
  cli::cli_text("  {cli::col_yellow('r')}. 🔄 Rafraîchir la liste")
  cli::cli_text("  {cli::col_yellow('q')}. ❌ Annuler")
  cli::cli_text("")
}

#' Process user's file selection choice
#' @keywords internal
process_file_selection_choice <- function(choice, current_dir) {
  # Handle special commands
  if (choice == "q") {
    cli::cli_alert_info("Sélection annulée.")
    stop("Sélection annulée par l'utilisateur")
  }
  
  if (choice == "r") {
    return("navigate")  # Signal to refresh
  }
  
  if (choice == ".") {
    if (confirm_directory_selection(current_dir)) {
      return(current_dir)
    } else {
      return("navigate")
    }
  }
  
  if (choice == "p") {
    parent_dir <- dirname(current_dir)
    if (parent_dir != current_dir) {  # Not root
      setwd(parent_dir)
      cli::cli_alert_info("📁 Navigé vers: {parent_dir}")
    } else {
      cli::cli_alert_warning("⚠️ Déjà au répertoire racine")
    }
    return("navigate")
  }
  
  if (choice == "cd") {
    new_path <- readline(prompt = "📂 Nouveau chemin: ")
    if (nchar(new_path) > 0 && dir.exists(new_path)) {
      setwd(new_path)
      cli::cli_alert_success("📁 Navigé vers: {new_path}")
    } else {
      cli::cli_alert_danger("❌ Chemin invalide ou inexistant")
    }
    return("navigate")
  }
  
  # Handle numeric selection
  choice_num <- suppressWarnings(as.integer(choice))
  if (!is.na(choice_num)) {
    return(handle_numeric_selection(choice_num, current_dir))
  }
  
  # Handle direct path input
  if (nchar(choice) > 0) {
    # Check if it's a relative or absolute path
    test_path <- if (file.exists(choice)) choice else file.path(current_dir, choice)
    
    if (file.exists(test_path)) {
      if (file.info(test_path)$isdir) {
        if (confirm_directory_selection(test_path)) {
          return(test_path)
        } else {
          return("navigate")
        }
      } else {
        return(test_path)
      }
    }
  }
  
  cli::cli_alert_danger("❌ Choix invalide. Essayez à nouveau.")
  return("navigate")
}

#' Handle numeric selection from the menu
#' @keywords internal
handle_numeric_selection <- function(choice_num, current_dir) {
  items <- list.files(current_dir, include.dirs = TRUE, no.. = TRUE)
  
  if (length(items) == 0) {
    cli::cli_alert_warning("⚠️ Aucun élément à sélectionner")
    return("navigate")
  }
  
  # Separate files and directories as in display function
  full_paths <- file.path(current_dir, items)
  is_dir <- file.info(full_paths)$isdir
  
  dirs <- items[is_dir]
  files <- items[!is_dir]
  
  # Get supported files
  supported_exts <- c("\\.csv$", "\\.dta$", "\\.sav$", "\\.rds$", "\\.rda$", 
                     "\\.xlsx$", "\\.xls$", "\\.dat$")
  supported_pattern <- paste(supported_exts, collapse = "|")
  supported_files <- files[grepl(supported_pattern, files, ignore.case = TRUE)]
  
  # Calculate selection
  if (choice_num <= length(supported_files)) {
    # Selected a supported file
    selected_file <- file.path(current_dir, supported_files[choice_num])
    cli::cli_alert_success("✅ Fichier sélectionné: {basename(selected_file)}")
    return(selected_file)
  }
  
  dir_choice <- choice_num - length(supported_files)
  if (dir_choice > 0 && dir_choice <= length(dirs)) {
    # Selected a directory
    selected_dir <- file.path(current_dir, dirs[dir_choice])
    
    # Ask if they want to navigate into it or select it
    cli::cli_text("📁 Dossier sélectionné: {dirs[dir_choice]}")
    action <- readline(prompt = "Voulez-vous (n)aviguer dedans ou le (s)électionner? [n/s]: ")
    
    if (tolower(action) == "s") {
      if (confirm_directory_selection(selected_dir)) {
        return(selected_dir)
      } else {
        return("navigate")
      }
    } else {
      # Navigate into directory
      setwd(selected_dir)
      cli::cli_alert_info("📁 Navigé dans: {dirs[dir_choice]}")
      return("navigate")
    }
  }
  
  cli::cli_alert_danger("❌ Numéro de sélection invalide")
  return("navigate")
}

#' Confirm directory selection and show preview
#' @keywords internal
confirm_directory_selection <- function(dir_path) {
  files <- list.files(dir_path, recursive = TRUE, full.names = TRUE)
  supported_exts <- c("\\.csv$", "\\.dta$", "\\.sav$", "\\.rds$", "\\.rda$", 
                     "\\.xlsx$", "\\.xls$", "\\.dat$")
  supported_pattern <- paste(supported_exts, collapse = "|")
  supported_files <- files[grepl(supported_pattern, files, ignore.case = TRUE)]
  
  cli::cli_rule("📁 Aperçu du dossier")
  cli::cli_text("📂 Dossier: {dir_path}")
  cli::cli_text("📄 Total fichiers: {length(files)}")
  cli::cli_text("✅ Fichiers supportés: {length(supported_files)}")
  
  if (length(supported_files) == 0) {
    cli::cli_alert_warning("⚠️ Aucun fichier supporté trouvé dans ce dossier")
    return(ask_yes_no("Voulez-vous quand même sélectionner ce dossier?"))
  }
  
  # Show first few supported files as preview
  if (length(supported_files) > 0) {
    cli::cli_text("📋 Aperçu des fichiers supportés:")
    preview_count <- min(5, length(supported_files))
    for (i in 1:preview_count) {
      rel_path <- sub(paste0("^", dir_path, "/"), "", supported_files[i])
      file_size <- format_file_size(file.info(supported_files[i])$size)
      cli::cli_text("  • {rel_path} {cli::col_silver('({file_size})')}")
    }
    if (length(supported_files) > preview_count) {
      cli::cli_text("  ... et {length(supported_files) - preview_count} autre(s)")
    }
  }
  
  return(ask_yes_no("Confirmer la sélection de ce dossier?"))
}

#' Format file size for display
#' @keywords internal
format_file_size <- function(size_bytes) {
  if (is.na(size_bytes)) return("N/A")
  
  if (size_bytes < 1024) {
    return(paste(size_bytes, "B"))
  } else if (size_bytes < 1024^2) {
    return(paste(round(size_bytes / 1024, 1), "KB"))
  } else if (size_bytes < 1024^3) {
    return(paste(round(size_bytes / 1024^2, 1), "MB"))
  } else {
    return(paste(round(size_bytes / 1024^3, 2), "GB"))
  }
}
