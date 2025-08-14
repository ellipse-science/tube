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

#' Interactive flow for collecting push parameters (SIMPLIFIED)
#' @param file_or_folder Initial file/folder (may be NULL)
#' @param dataset_name Initial dataset name (may be NULL) 
#' @param tag Initial tag (may be NULL)
#' @param metadata Initial metadata (may be NULL)
#' @keywords internal
interactive_datalake_push_flow <- function(file_or_folder, dataset_name, tag, metadata) {
  cli::cli_h1("🚀 Upload vers le Datalake Public")
  cli::cli_text("")
  
  # SIMPLIFIED FILE SELECTION - with clear numbered choices
  if (is.null(file_or_folder)) {
    file_or_folder <- simple_file_folder_selector()
  }
  
  # Dataset name
  if (is.null(dataset_name)) {
    cli::cli_h2("📦 Nom du dataset")
    cli::cli_text("Choisissez un nom descriptif pour votre dataset")
    cli::cli_text("")
    
    dataset_name <- readline(prompt = "📦 Nom du dataset: ")
    
    if (nchar(dataset_name) == 0) {
      cli::cli_alert_danger("Le nom du dataset est requis!")
      stop("Dataset name manquant")
    }
  }
  
  # Tag
  if (is.null(tag)) {
    cli::cli_h2("🏷️ Tag")
    cli::cli_text("Ex: v1.0, 2025-prod, pilot-test")
    cli::cli_text("")
    
    tag <- readline(prompt = "🏷️ Tag: ")
    
    if (nchar(tag) == 0) {
      cli::cli_alert_danger("Le tag est requis!")
      stop("Tag manquant")
    }
  }
  
  # ENHANCED METADATA COLLECTION - now includes required system fields
  if (is.null(metadata)) {
    metadata <- collect_all_metadata_interactive()
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

#' Collect ALL metadata including required system fields
#' @keywords internal
collect_all_metadata_interactive <- function() {
  cli::cli_h2("📊 Métadonnées du dataset")
  cli::cli_text("Ces informations sont importantes pour la gouvernance des données")
  cli::cli_text("")
  
  metadata <- list()
  
  # REQUIRED SYSTEM FIELDS
  metadata <- collect_required_system_metadata(metadata)
  
  # OPTIONAL CUSTOM FIELDS
  metadata <- collect_optional_custom_metadata(metadata)
  
  return(metadata)
}

#' Collect required system metadata fields
#' @keywords internal
collect_required_system_metadata <- function(metadata) {
  cli::cli_h3("🔒 Métadonnées système requises")
  
  # Sensitivity level (1-5, no legend)
  cli::cli_text("� Niveau de sensibilité des données")
  repeat {
    sensitivity <- readline(prompt = "� Niveau de sensibilité [1-5]: ")
    if (sensitivity %in% c("1", "2", "3", "4", "5")) {
      metadata$sensitivity_level <- as.numeric(sensitivity)
      break
    }
    cli::cli_alert_danger("Veuillez entrer un niveau entre 1 et 5")
  }
  
  # Ethical approval
  cli::cli_text("")
  if (ask_yes_no("✅ Approbation éthique?")) {
    metadata$ethical_stamp <- "approved"
  } else {
    metadata$ethical_stamp <- "pending"
  }
  
  # Creation date (default to today)
  cli::cli_text("")
  cli::cli_text("� Date de création des données")
  default_date <- format(Sys.Date(), "%Y-%m-%d")
  creation_date <- readline(prompt = paste0("� Date de création [", default_date, "]: "))
  if (nchar(creation_date) == 0) creation_date <- default_date
  metadata$creation_date <- creation_date
  
  # Consent expiry (optional but important)
  cli::cli_text("")
  cli::cli_text("⏰ Date d'expiration du consentement (optionnel)")
  cli::cli_text("Format: YYYY-MM-DD ou tapez 'none' si pas applicable")
  consent_expiry <- readline(prompt = "⏰ Date d'expiration du consentement: ")
  if (nchar(consent_expiry) > 0 && tolower(consent_expiry) != "none") {
    metadata$consent_expiry_date <- consent_expiry
  }
  
  # Data destruction date (optional but important)
  cli::cli_text("")
  cli::cli_text("🗑️ Date de destruction des données (optionnel)")
  cli::cli_text("Format: YYYY-MM-DD ou tapez 'none' si pas applicable")
  destruction_date <- readline(prompt = "🗑️ Date de destruction: ")
  if (nchar(destruction_date) > 0 && tolower(destruction_date) != "none") {
    metadata$data_destruction_date <- destruction_date
  }
  
  cli::cli_text("")
  cli::cli_alert_success("✅ Métadonnées système collectées")
  
  return(metadata)
}

#' Collect optional custom metadata fields  
#' @keywords internal
collect_optional_custom_metadata <- function(metadata) {
  cli::cli_text("")
  cli::cli_h3("📝 Métadonnées personnalisées (optionnel)")
  
  if (!ask_yes_no("Voulez-vous ajouter des métadonnées personnalisées?")) {
    return(metadata)
  }
  
  cli::cli_text("Exemples: title, authors, year, description, contact, etc.")
  cli::cli_text("Tapez 'done' pour terminer")
  cli::cli_text("")
  
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
  
  metadata
}

#' Display upload summary before confirmation (enhanced with system metadata)
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
  
  # System metadata
  cli::cli_text("")
  cli::cli_text("🔒 Métadonnées système:")
  if (!is.null(metadata$creation_date)) {
    cli::cli_text("   📅 Date de création: {metadata$creation_date}")
  }
  if (!is.null(metadata$sensitivity_level)) {
    cli::cli_text("   🔒 Niveau de sensibilité: {metadata$sensitivity_level}")
  }
  if (!is.null(metadata$consent_expiry_date)) {
    cli::cli_text("   ⏰ Expiration consentement: {metadata$consent_expiry_date}")
  }
  if (!is.null(metadata$data_destruction_date)) {
    cli::cli_text("   🗑️ Destruction données: {metadata$data_destruction_date}")
  }
  if (!is.null(metadata$ethical_stamp)) {
    cli::cli_text("   ✅ Tampon éthique: {metadata$ethical_stamp}")
  }
  
  # Custom metadata
  system_fields <- c("creation_date", "consent_expiry_date", "data_destruction_date", 
                     "sensitivity_level", "ethical_stamp")
  user_metadata <- metadata[!names(metadata) %in% system_fields]
  
  if (length(user_metadata) > 0) {
    cli::cli_text("")
    cli::cli_text("📝 Métadonnées personnalisées:")
    for (name in names(user_metadata)) {
      cli::cli_text("   • {name}: {user_metadata[[name]]}")
    }
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
    "creation_date" = format(Sys.Date(), "%Y-%m-%d"),
    "consent_expiry_date" = format(Sys.Date() + 365, "%Y-%m-%d"), # Default 1 year
    "data_destruction_date" = format(Sys.Date() + 365*10, "%Y-%m-%d"), # Default 10 years
    "sensitivity_level" = "3", # Default medium sensitivity (1-5 scale)
    "ethical_stamp" = "true" # Default approved
  )
  
  # Override with actual values from collected metadata
  if (!is.null(custom_metadata)) {
    if (!is.null(custom_metadata$creation_date)) {
      s3_metadata[["creation_date"]] <- custom_metadata$creation_date
    }
    if (!is.null(custom_metadata$consent_expiry_date)) {
      s3_metadata[["consent_expiry_date"]] <- custom_metadata$consent_expiry_date
    }
    if (!is.null(custom_metadata$data_destruction_date)) {
      s3_metadata[["data_destruction_date"]] <- custom_metadata$data_destruction_date
    }
    if (!is.null(custom_metadata$sensitivity_level)) {
      s3_metadata[["sensitivity_level"]] <- as.character(custom_metadata$sensitivity_level)
    }
    if (!is.null(custom_metadata$ethical_stamp)) {
      s3_metadata[["ethical_stamp"]] <- ifelse(custom_metadata$ethical_stamp == "approved", "true", "false")
    }
    
    # Add only non-system custom metadata to user_metadata_json
    system_fields <- c("creation_date", "consent_expiry_date", "data_destruction_date", 
                       "sensitivity_level", "ethical_stamp")
    user_metadata <- custom_metadata[!names(custom_metadata) %in% system_fields]
    
    if (length(user_metadata) > 0) {
      # Convert only user custom metadata to JSON string
      custom_json <- jsonlite::toJSON(user_metadata, auto_unbox = TRUE)
      s3_metadata[["user_metadata_json"]] <- custom_json
    }
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
    # Find the correct lambda function using the generic finder
    lambda_name <- find_datalake_indexing_lambda(creds)
    
    if (is.null(lambda_name)) {
      logger::log_warn("[invoke_datalake_indexing_lambda] no matching lambda function found")
      cli::cli_alert_warning("⚠️ Fonction lambda d'indexation introuvable - indexation manuelle requise")
      return(FALSE)
    }
    
    logger::log_debug(paste("[invoke_datalake_indexing_lambda] using lambda:", lambda_name))
    
    # Setup Lambda client
    lambda_client <- paws.compute::lambda(config = creds)
    
    # Invoke the lambda function (no payload needed)
    result <- lambda_client$invoke(
      FunctionName = lambda_name,
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

#' Find the lambda function for datalake indexing
#' @param credentials AWS credentials from get_aws_credentials()
#' @return Lambda function name if found, NULL otherwise
#' @keywords internal
find_datalake_indexing_lambda <- function(credentials) {
  logger::log_debug("[find_datalake_indexing_lambda] entering function")
  
  # Updated patterns based on actual infrastructure
  patterns <- c(
    "publicdatalakecontent"           # Primary pattern
  )
  
  lambda_name <- find_lambda_by_pattern(credentials, patterns, return_first = TRUE)
  
  if (!is.null(lambda_name)) {
    logger::log_debug(paste("[find_datalake_indexing_lambda] found datalake indexing lambda:", lambda_name))
  } else {
    logger::log_warn("[find_datalake_indexing_lambda] no datalake indexing lambda found")
  }
  
  return(lambda_name)
}

# ==============================================================================
# SIMPLE FILE SELECTION WITH CLEAR NUMBERED CHOICES
# ==============================================================================

#' Simple file and folder selector with numbered choices
#' @keywords internal
simple_file_folder_selector <- function() {
  cli::cli_h2("📁 Sélectionnez un fichier ou dossier")
  cli::cli_text("📄 Formats: CSV, DTA, SAV, RDS, RDA, XLSX, XLS, DAT")
  cli::cli_text("")
  
  current_dir <- getwd()
  
  repeat {
    cli::cli_text("📂 Répertoire actuel: {cli::col_blue(current_dir)}")
    cli::cli_text("")
    
    # Get directory contents
    items <- list.files(current_dir, include.dirs = TRUE, no.. = TRUE)
    
    if (length(items) == 0) {
      cli::cli_text("💡 Répertoire vide")
      cli::cli_text("")
      
      # Create minimal choice map for empty directories
      choice_map <- list()
      choice_num <- 1
      
      # Add parent directory option (..) - with legend
      parent_dir <- dirname(current_dir)
      if (parent_dir != current_dir) {
        cli::cli_text("  {cli::col_yellow(choice_num)}. 📁 .. {cli::col_silver('(dossier parent)')}")
        choice_map[[as.character(choice_num)]] <- list(type = "parent", path = parent_dir, name = "..")
        choice_num <- choice_num + 1
      }
      
      # Add current directory option (.)
      cli::cli_text("  {cli::col_yellow(choice_num)}. 📂 . {cli::col_silver('(sélectionner ce dossier)')}")
      choice_map[[as.character(choice_num)]] <- list(type = "current", path = current_dir, name = ".")
      choice_num <- choice_num + 1
      
      # Add quit option
      cli::cli_text("  {cli::col_red(choice_num)}. ❌ Annuler")
      choice_map[[as.character(choice_num)]] <- list(type = "quit", path = NULL, name = "quit")
      
      cli::cli_text("")
      choice <- get_user_choice()
      result <- handle_simple_choice(choice, choice_map, current_dir)
      if (result$action == "update_dir") {
        current_dir <- result$path
      } else if (result$action == "select") {
        return(result$path)
      } else if (result$action == "quit") {
        stop("Sélection annulée par l'utilisateur")
      }
      next
    }
    
    # Separate and categorize items
    categorized_items <- categorize_directory_items(current_dir, items)
    
    # Display options with simple numbers
    choice_map <- display_simple_file_menu(categorized_items)
    
    # Get user choice
    choice <- get_user_choice()
    
    # Handle the choice
    result <- handle_simple_choice(choice, choice_map, current_dir)
    
    if (result$action == "continue") {
      next
    } else if (result$action == "update_dir") {
      current_dir <- result$path
    } else if (result$action == "select") {
      return(result$path)
    } else if (result$action == "quit") {
      stop("Sélection annulée par l'utilisateur")
    }
  }
}

#' Categorize directory items into supported files, folders, and other files
#' @keywords internal
categorize_directory_items <- function(current_dir, items) {
  full_paths <- file.path(current_dir, items)
  is_dir <- file.info(full_paths)$isdir
  
  dirs <- items[is_dir]
  files <- items[!is_dir]
  
  # Identify supported files
  supported_exts <- c("csv", "dta", "sav", "rds", "rda", "xlsx", "xls", "dat")
  supported_files <- character(0)
  other_files <- character(0)
  
  for (file in files) {
    ext <- tools::file_ext(tolower(file))
    if (ext %in% supported_exts) {
      supported_files <- c(supported_files, file)
    } else {
      other_files <- c(other_files, file)
    }
  }
  
  list(
    supported_files = supported_files,
    dirs = dirs,
    other_files = other_files,
    current_dir = current_dir
  )
}

#' Display simple file menu with literal navigation and numbered choices
#' @keywords internal
display_simple_file_menu <- function(items) {
  choice_map <- list()
  choice_num <- 1
  
  # Add current directory option (.) - literal option first
  cli::cli_text("  \t📂 . {cli::col_silver('(sélectionner ce dossier)')}")
  choice_map[["."]] <- list(type = "current", path = items$current_dir, name = ".")
  
  # Add parent directory option (..) - literal option second  
  parent_dir <- dirname(items$current_dir)
  if (parent_dir != items$current_dir) {
    cli::cli_text("  \t📂 .. {cli::col_silver('(dossier parent)')}")
    choice_map[[".."]] <- list(type = "parent", path = parent_dir, name = "..")
  }
  
  # Combine directories first, then supported files (folders first)
  all_items <- c(items$dirs, items$supported_files)
  
  if (length(all_items) > 0) {
    for (item in all_items) {
      item_path <- file.path(items$current_dir, item)
      
      # Check if it's a directory
      if (item %in% items$dirs) {
        file_count <- length(list.files(item_path, recursive = FALSE))
        cli::cli_text("  {cli::col_blue(sprintf('%2d', choice_num))}. 📁 {item}/ {cli::col_silver(paste0('(', file_count, ' éléments)'))}")
        choice_map[[as.character(choice_num)]] <- list(type = "dir", path = item_path, name = item)
      } else {
        # It's a supported file
        file_size <- format_file_size(file.info(item_path)$size)
        cli::cli_text("  {cli::col_green(sprintf('%2d', choice_num))}. 📄 {item} {cli::col_silver(paste0('(', file_size, ')'))}")
        choice_map[[as.character(choice_num)]] <- list(type = "file", path = item_path, name = item)
      }
      choice_num <- choice_num + 1
    }
  }
  
  # Add quit option
  cli::cli_text("  {cli::col_red(sprintf('%2d', choice_num))}. ❌ Annuler")
  choice_map[[as.character(choice_num)]] <- list(type = "quit", path = NULL, name = "quit")
  
  cli::cli_text("")
  return(choice_map)
}

# ==============================================================================

#' Handle numeric selection from the menu

#' Get user choice with validation
#' @keywords internal
get_user_choice <- function() {
  repeat {
    choice <- readline(prompt = "👆 Choix: ")
    
    if (nchar(choice) > 0) {
      return(trimws(choice))
    }
    
    cli::cli_alert_warning("⚠️ Veuillez entrer un choix valide")
  }
}

#' Handle simple user choice
#' @keywords internal
handle_simple_choice <- function(choice, choice_map, current_dir) {
  # Handle literal choices for navigation
  if (choice == ".") {
    if ("." %in% names(choice_map)) {
      item <- choice_map[["."]]
      if (confirm_simple_directory_selection(item$path)) {
        return(list(action = "select", path = item$path))
      } else {
        return(list(action = "continue"))
      }
    }
  }
  
  if (choice == "..") {
    if (".." %in% names(choice_map)) {
      item <- choice_map[[".."]]
      cli::cli_alert_info("📁 Remontée vers le dossier parent")
      return(list(action = "update_dir", path = item$path))
    }
  }
  
  # Handle numbered choices
  if (choice %in% names(choice_map)) {
    item <- choice_map[[choice]]
    
    if (item$type == "file") {
      cli::cli_alert_success("✅ Fichier sélectionné: {item$name}")
      return(list(action = "select", path = item$path))
    } else if (item$type == "dir") {
      cli::cli_alert_info("📁 Navigation vers: {item$name}")
      return(list(action = "update_dir", path = item$path))
    } else if (item$type == "quit") {
      return(list(action = "quit"))
    }
  }
  
  cli::cli_alert_warning("⚠️ Choix invalide (utilisez '.', '..' ou un numéro affiché)")
  return(list(action = "continue"))
}

#' Handle navigation-only choices
#' @keywords internal
handle_navigation_choice <- function(choice, current_dir) {
  if (choice == "q") {
    return(list(action = "quit"))
  }
  
  if (choice == "p") {
    parent_dir <- dirname(current_dir)
    if (parent_dir != current_dir) {
      cli::cli_alert_info("📁 Remontée vers: {basename(parent_dir)}")
      return(list(action = "update_dir", path = parent_dir))
    } else {
      cli::cli_alert_warning("⚠️ Déjà au répertoire racine")
      return(list(action = "continue"))
    }
  }
  
  if (choice == "h") {
    home_dir <- path.expand("~")
    cli::cli_alert_info("🏠 Retour au répertoire personnel")
    return(list(action = "update_dir", path = home_dir))
  }
  
  if (choice == ".") {
    if (confirm_simple_directory_selection(current_dir)) {
      return(list(action = "select", path = current_dir))
    } else {
      return(list(action = "continue"))
    }
  }
  
  cli::cli_alert_warning("⚠️ Choix invalide. Utilisez p/h/./q")
  return(list(action = "continue"))
}

#' Confirm directory selection with simple preview
#' @keywords internal
confirm_simple_directory_selection <- function(dir_path) {
  files <- list.files(dir_path, recursive = TRUE, full.names = TRUE)
  supported_exts <- c("csv", "dta", "sav", "rds", "rda", "xlsx", "xls", "dat")
  
  supported_count <- 0
  for (file in files) {
    if (tools::file_ext(tolower(file)) %in% supported_exts) {
      supported_count <- supported_count + 1
    }
  }
  
  cli::cli_rule("📁 Aperçu du dossier")
  cli::cli_text("📂 Dossier: {basename(dir_path)}")
  cli::cli_text("📄 Total fichiers: {length(files)}")
  cli::cli_text("✅ Fichiers supportés: {supported_count}")
  cli::cli_rule()
  
  if (supported_count == 0) {
    cli::cli_alert_warning("⚠️ Aucun fichier supporté trouvé dans ce dossier")
  }
  
  return(ask_yes_no("Confirmer la sélection de ce dossier?"))
}

# ==============================================================================

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

#' Get user choice with validation
#' @keywords internal
get_user_choice <- function() {
  repeat {
    choice <- readline(prompt = "👆 Choix: ")
    
    if (nchar(choice) > 0) {
      return(trimws(choice))
    }
    
    cli::cli_alert_warning("⚠️ Veuillez entrer un choix valide")
  }
}

#' Handle simple user choice
#' @keywords internal
handle_simple_choice <- function(choice, choice_map, current_dir) {
  # Handle numbered choices
  if (choice %in% names(choice_map)) {
    item <- choice_map[[choice]]
    
    if (item$type == "file") {
      cli::cli_alert_success("✅ Fichier sélectionné: {item$name}")
      return(list(action = "select", path = item$path))
    } else if (item$type == "dir") {
      cli::cli_alert_info("📁 Navigation vers: {item$name}")
      return(list(action = "update_dir", path = item$path))
    } else if (item$type == "parent") {
      cli::cli_alert_info("📁 Remontée vers le dossier parent")
      return(list(action = "update_dir", path = item$path))
    } else if (item$type == "current") {
      if (confirm_simple_directory_selection(item$path)) {
        return(list(action = "select", path = item$path))
      } else {
        return(list(action = "continue"))
      }
    } else if (item$type == "quit") {
      return(list(action = "quit"))
    }
  }
  
  cli::cli_alert_warning("⚠️ Choix invalide (utilisez un numéro affiché)")
  return(list(action = "continue"))
}

#' Confirm directory selection with simple preview
#' @keywords internal
confirm_simple_directory_selection <- function(dir_path) {
  files <- list.files(dir_path, recursive = TRUE, full.names = TRUE)
  supported_exts <- c("csv", "dta", "sav", "rds", "rda", "xlsx", "xls", "dat")
  
  supported_count <- 0
  for (file in files) {
    if (tools::file_ext(tolower(file)) %in% supported_exts) {
      supported_count <- supported_count + 1
    }
  }
  
  cli::cli_rule("📁 Aperçu du dossier")
  cli::cli_text("📂 Dossier: {basename(dir_path)}")
  cli::cli_text("📄 Total fichiers: {length(files)}")
  cli::cli_text("✅ Fichiers supportés: {supported_count}")
  cli::cli_rule()
  
  if (supported_count == 0) {
    cli::cli_alert_warning("⚠️ Aucun fichier supporté trouvé dans ce dossier")
  }
  
  return(ask_yes_no("Confirmer la sélection de ce dossier?"))
}
