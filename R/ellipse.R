# Global variable declarations to avoid "no visible binding" warnings
utils::globalVariables(c(
  "is_partition", "col_name", "table_name", "location", "table_tags",
  "categorie", "datamart", "description", "create_time", "update_time",
  "format_public_datalake_all_datasets", "format_public_datalake_pattern_search",
  "format_public_datalake_dataset_details", "format_public_datalake_tag_details",
  "ellipse_push_datalake_mode", "ellipse_push_landingzone_mode"
))

#' Se connecter à la plateforme de données ellipse
#'
#' Cette fonction utilise les clés d'accès AWS configurées dans le fichier
#' `.Renviron` pour se connecter à la plateforme de données.
#'
#' @param env The environment to connect to on ellipse-science. Accepted values are "PROD" and "DEV".
#' @param database The Glue/Athena database to connect to. Default to "datawarehouse"
#'
#' @returns Un object de connexion `DBI`.
#' @export
ellipse_connect <- function(
  env = NULL,
  database = "datawarehouse") {
  if (!check_env(env)) {
    cli::cli_alert_danger(paste("Oups, il faut choisir un environnement! 😅\n\n",
        "Le paramètre `env` peut être \"PROD\" ou \"DEV\"",
        sep = ""
      ))
    return(invisible(NULL))
  }
  cli::cli_alert_info(paste("Environnement:", env))

  if (!check_database(database)) {
    cli::cli_alert_danger(paste("Oups, il faut choisir une base de données! 😅\n\n",
        "Le paramètre `database` peut être \"datawarehouse\", \"datamarts\", ou \"datalake\"",
        sep = ""
      ))
    return(invisible(NULL))
  }
  cli::cli_alert_info(paste("Database:", database))

  aws_access_key_id <-
    switch(env,
      "PROD" = "AWS_ACCESS_KEY_ID_PROD",
      "DEV"  = "AWS_ACCESS_KEY_ID_DEV"
    ) |>
    Sys.getenv()

  aws_secret_access_key <-
    switch(env,
      "PROD" = "AWS_SECRET_ACCESS_KEY_PROD",
      "DEV"  = "AWS_SECRET_ACCESS_KEY_DEV"
    ) |>
    Sys.getenv()

  if (aws_access_key_id == "" || aws_secret_access_key == "") {
    usage <-
      paste(
        "On a besoin de vos clés d'accès sur AWS pour se connecter!\n\n",
        "Dans le fichier ~/.Renviron, ajoutez les lignes:\n\n",
        "AWS_ACCESS_KEY_ID_PROD=<votre access key id de production>\n",
        "AWS_SECRET_ACCESS_KEY_PROD=<votre secret access key de production>\n",
        "AWS_ACCESS_KEY_ID_DEV=<votre access key id de développement>\n",
        "AWS_SECRET_ACCESS_KEY_DEV=<votre secret access key de développement>\n\n",
        "Puis, redémarrez la session R."
      )
    cli::cli_alert_danger(usage)
    return(invisible(NULL))
  }

  creds <- get_aws_credentials(env)

  # aws_access_key_id <- creds$credentials$creds$access_key_id
  # aws_secret_access_key <- creds$credentials$creds$secret_access_key

  datawarehouse_database <- list_datawarehouse_database(creds)
  datamarts_database <- list_datamarts_database(creds)
  public_datalake_database <- list_public_datalake_database(creds)
  athena_staging_bucket <- list_athena_staging_bucket(creds)

  schema_name <- switch(database,
    "datawarehouse" = paste0(datawarehouse_database),
    "datamarts" = paste0(datamarts_database),
    "datalake" = paste0(public_datalake_database),
    database
  )

  logger::log_debug(paste("[ellipse_connect] datawarehouse_database = ", datawarehouse_database))
  logger::log_debug(paste("[ellipse_connect] datamarts_database = ", datamarts_database))
  logger::log_debug(paste("[ellipse_connect] public_datalake_database = ", public_datalake_database))
  logger::log_debug(paste("[ellipse_connect] athena_staging_bucket = ", athena_staging_bucket))
  logger::log_debug(paste("[ellipse_connect] schema_name = ", schema_name))

  cli::cli_alert_info("Connexion en cours...")

  con <- DBI::dbConnect(noctua::athena(),
    aws_access_key_id = aws_access_key_id,
    aws_secret_access_key = aws_secret_access_key,
    profile_name = env,
    schema_name = schema_name,
    work_group = "ellipse-work-group",
    s3_staging_dir = paste0("s3://", athena_staging_bucket)
  )
  # }

  schema <- DBI::dbGetInfo(con)$dbms.name

  logger::log_debug(paste("[ellipse_connect] schema = ", schema))

  if (length(athena_staging_bucket) > 0) {
    cli::cli_alert_info(paste("Compartiment:", athena_staging_bucket))
  } else {
    cli::cli_alert_danger("Oups, cette connexion n'a pas de compartiment de requêtes! 😅")
  }

  if (length(schema) > 0) {
    cli::cli_alert_info(paste("Base de données:", schema))
    cli::cli_alert_info("Pour déconnecter: tube::ellipse_disconnect(objet_de_connexion)")
    cli::cli_alert_success("Connexion établie avec succès! 👍")
    return(con)
  } else {
    cli::cli_alert_danger("Oups, cette connexion n'a pas de base de données! 😅")
    return(invisible(NULL))
  }
}

#' Se déconnecter de la plateforme de données ellipse
#' @param con L'objet de connexion à fermer
#' @returns TRUE if the connexion was closed or FALSE if no connexion existed
#' @export
ellipse_disconnect <- function(con = NULL) {
  if (is.null(con)) {
    cli::cli_alert_danger("Oups! Il faut fournir un objet de connection! 😅")
    invisible(FALSE)
  }

  tryCatch(
    {
      if (DBI::dbIsValid(con)) {
        DBI::dbDisconnect(con)
        cli::cli_alert_success("La connexion a été fermée avec succès! 👋")
        invisible(TRUE)
      } else {
        cli::cli_alert_warning("Il semble que la connexion n'existe pas ou soit déjà close! 😅")
        invisible(FALSE)
      }
    },
    error = function(e) {
      cli::cli_alert_danger("Oups, il semble que la connexion n'a pas pu être fermée! 😅")
      invisible(FALSE)
    }
  )
}

#' Obtenir le domaine de valeurs pour les dimensions d'une table
#'
#' @details
#' Les tables de données sur AWS sont segmentées en _partitions_. Les requêtes
#' qui ciblent une plage précise dans ces partitions réduisent les coûts
#' d'utilisation de la plateforme, parce que les données à l'extérieur de cette
#' plage ne sont pas lues par _AWS Athena_.
#'
#' Cette fonction permet d'obtenir les valeurs possibles des partitions d'une
#' table donnée afin de mieux cibler nos requêtes ensuite.
#'
#' @inheritParams ellipse_discover
#'
#' @returns Un `tibble` contenant le nombre d'observations par valeur de
#'   groupement des variables partitionnées.
#'
#' @export
ellipse_partitions <- function(con, table) {
  df <- ellipse_discover(con, table)
  if (is.list(df) && "columns" %in% names(df)) {
    df <- df$columns
  }
  df <- dplyr::filter(df, is_partition)
  partitions <- dplyr::pull(df, col_name)
  ellipse_query(con, table) |>
    dplyr::count(dplyr::across(dplyr::all_of(partitions))) |>
    dplyr::collect() |>
    dplyr::arrange(dplyr::across(dplyr::all_of(partitions)))
}

#' Découvrir les tables disponibles sur la plateforme ellipse, ainsi que leur
#' contenu
#'
#' Si aucune table n'est passée en paramètre, un sommaire des tables disponibles
#' dans l'entrepôt de données est retourné. Si un nom de `table` est passé en
#' paramètre, une description des colonnes de cette table est retournée.
#'
#' Pour le datalake public (connexion via `ellipse_connect("DEV", "datalake")`),
#' cette fonction supporte trois modes de découverte avec métadonnées enrichies :
#' - `ellipse_discover(con)` : Tous les jeux de données avec métadonnées
#' - `ellipse_discover(con, "pattern")` : Recherche par motif avec résumé
#' - `ellipse_discover(con, "nom", "tag")` : Détails spécifiques nom/tag
#'
#' @param con Un objet de connexion tel qu'obtenu via `tube::ellipse_connect()`.
#' @param table Une table pour laquelle on veut obtenir les informations.
#' @param tag Un tag spécifique pour la découverte dans le datalake public (optionnel).
#'
#' @returns Un `tibble` contenant les tables disponibles dans l'entrepôt de
#'   données, ou un descriptino des colonnes pour une table en particulier.
#'
#' @export
ellipse_discover <- function(con, table = NULL, tag = NULL) {
  env <- DBI::dbGetInfo(con)$profile_name
  schema <- DBI::dbGetInfo(con)$dbms.name
  creds <- get_aws_credentials(env)

  if (length(schema) == 0) {
    cli::cli_alert_danger("Oups, cette connection n'a pas de base de données! 😅")
    return(invisible(NULL))
  }

  # Check if this is a public datalake connection
  if (grepl("publicdatalake", schema, ignore.case = TRUE)) {
    # Handle public datalake discovery with three patterns
    if (is.null(table) && is.null(tag)) {
      # Pattern 1: ellipse_discover(con) - All datasets
      return(format_public_datalake_all_datasets(con))
    } else if (!is.null(table) && is.null(tag)) {
      # Check if this is an exact match or pattern search
      exact_check_query <- paste0('SELECT COUNT(*) as count FROM "public-data-lake-content"
                                  WHERE name = \'', table, "'")
      exact_result <- DBI::dbGetQuery(con, exact_check_query)

      if (exact_result$count > 0) {
        # Pattern 3: ellipse_discover(con, exact_name) - Specific dataset details
        return(format_public_datalake_dataset_details(con, table))
      } else {
        # Pattern 2: ellipse_discover(con, pattern) - Pattern search
        return(format_public_datalake_pattern_search(con, table))
      }
    } else if (!is.null(table) && !is.null(tag)) {
      # Pattern 4: ellipse_discover(con, name, tag) - Specific name/tag combination
      return(format_public_datalake_tag_details(con, table, tag))
    }
  }

  # Continue with existing logic for datawarehouse and datamarts
  tables <- DBI::dbGetQuery(
    con, paste0("SHOW TABLES IN ", schema)
  )$tab_name

  if (!is.null(table)) {
    if (!any(grepl(table, tables))) {
      cli::cli_alert_danger("La table demandée est inconnue.")
      return(invisible(NULL))
    }

    # See if there is only one table or many tables that match the table name
    if (length(grep(table, tables)) == 1 || table %in% tables) {
      table_properties_df <- list_glue_table_properties(creds, schema, table)
      table_df <- list_glue_tables(creds, schema) |>
        dplyr::filter(table_name == table)
      if ("table_tags" %in% colnames(table_properties_df)) {
        tags <- unlist(table_properties_df$table_tags)
      } else {
        tags <- NA_character_
      }
      return(list(
        name = table_properties_df$table_name,
        description = table_properties_df$description,
        tags = tags,
        columns = table_df
      ))
    } else {
      if (length(grep(table, tables)) > 1) {
        cli::cli_alert_info("Plusieurs tables correspondent à votre recherche (voir résultat retourné).")
        cli::cli_alert_info("Veuillez préciser votre recherche pour explorer la table.")
        tables <- tables[grep(table, tables)]
      }
    }
  }

  tables_properties <- lapply(tables, function(table) {
    logger::log_debug(paste("[ellipse_discover] listing table properties for", table, "in schema", schema))
    list_glue_table_properties(creds, schema, table)
  }) |>
    dplyr::bind_rows() |>
    dplyr::select(-location)

  tables_tibble <- tibble::tibble(table_name = tables) |>
    dplyr::left_join(tables_properties, by = "table_name")

  if (!"table_tags" %in% colnames(tables_tibble)) {
    tables_tibble <- tables_tibble |>
      dplyr::mutate(table_tags = list(NULL))
  }

  # Extract x-amz-meta-category from table_tags
  tables_tibble <- tables_tibble |>
    dplyr::mutate(
      category_from_tags =
        purrr::map_chr(table_tags, ~ {
          if (!("x-amz-meta-category" %in% names(.x))) {
            NA_character_
          } else {
            .x[["x-amz-meta-category"]]
          }
        })
    )

  # Extract x-amz-meta-datamart from table_tags
  tables_tibble <- tables_tibble |>
    dplyr::mutate(
      datamart_from_tags =
        purrr::map_chr(table_tags, ~ {
          if (!("x-amz-meta-datamart" %in% names(.x))) {
            NA_character_
          } else {
            .x[["x-amz-meta-datamart"]]
          }
        })
    )

  has_non_na_datamart <- any(!is.na(tables_tibble$datamart_from_tags))

  tables_tibble <- tables_tibble |>
    dplyr::mutate(
      categorie =
        dplyr::case_when(
          startsWith(table_name, "a-") ~ "Agora+",
          startsWith(table_name, "c-") ~ "Civimètre+",
          startsWith(table_name, "r-") ~ "Radar+",
          startsWith(table_name, "dict-") ~ "Dictionnaire",
          startsWith(table_name, "dim-") ~ "Dimension",
          !is.na(category_from_tags) ~ category_from_tags,
          TRUE ~ "Autre"
        )
    ) |>
    dplyr::mutate(
      datamart =
        dplyr::case_when(
          !is.na(datamart_from_tags) ~ datamart_from_tags,
          TRUE ~ NA_character_
        )
    )

  if (has_non_na_datamart) {
    ret <- tables_tibble |>
      dplyr::select(
        table_name, categorie, datamart, description, create_time,
        update_time, table_tags
      )
    return(ret)
  } else {
    ret <- tables_tibble |>
      dplyr::select(
        table_name, categorie, description, create_time, update_time,
        table_tags
      )
    return(ret)
  }
}

#' Lire et exploiter une table contenue dans l'entrepôt de données ellipse
#'
#' @param con Un objet de connexion tel qu'obtenu via `tube::ellipse_connect()`.
#' @param dataset Pour les connexions datawarehouse/datamarts: nom d'une table.
#'   Pour les connexions datalake: nom d'un dataset à agréger.
#' @param tag Optionnel. Pour les connexions datalake seulement: tag spécifique
#'   à filtrer. Si NULL, agrège tous les tags du dataset.
#'
#' @returns Pour datawarehouse/datamarts: Une table Athena qui peut être interrogée
#'   dans un pipeline `dplyr`. Pour datalake datasets: Un dataframe agrégé de tous les
#'   fichiers du dataset. Pour datalake images: Interface interactive pour visualiser
#'   les images une par une.
#' @export
ellipse_query <- function(con, dataset, tag = NULL) {
  logger::log_debug(paste("[ellipse_query] entering function with dataset = ", dataset, ", tag = ", tag))
  schema_name <- DBI::dbGetInfo(con)$dbms.name

  # Detect if this is a datalake connection (same logic as ellipse_discover)
  is_datalake <- grepl("publicdatalake", schema_name, ignore.case = TRUE)

  if (is_datalake) {
    # File aggregator mode for public datalake
    logger::log_debug("[ellipse_query] datalake mode - file aggregator")
    return(ellipse_query_datalake_aggregator(con, dataset, tag))
  } else {
    # Traditional table query mode for datawarehouse/datamarts
    logger::log_debug("[ellipse_query] traditional table mode")
    return(ellipse_query_table_mode(con, dataset))
  }
}

#' Injecter/Pousser de nouvelles données dans la plateforme tube
#'
#' Cette fonction unifie l'injection de données vers différentes destinations:
#' - Datalake public (pour connexions "datalake"): Upload direct avec métadonnées
#'   Formats supportés: CSV, DTA, SAV, RDS, RDA, XLSX, XLS, DAT, XML, PNG, JPEG
#' - Landing zone (pour connexions "datawarehouse"): Pipeline traditionnel ETL
#'
#' @param con Un objet de connexion tel qu'obtenu via `tube::ellipse_connect()`.
#' @param file_or_folder Le chemin vers le fichier ou répertoire à charger.
#'   Optionnel en mode interactif - l'utilisateur sera invité à sélectionner.
#' @param dataset_name Pour connexions datalake: nom du dataset (obligatoire)
#' @param tag Pour connexions datalake: tag de version (obligatoire)
#' @param metadata Pour connexions datalake: métadonnées personnalisées (liste nommée, optionnel).
#'   Pour datasets: métadonnées système requises (éthique, consentement, etc.)
#'   Pour images: métadonnées simplifiées (seulement date de création et champs personnalisés)
#'   \itemize{
#'     \item \code{creation_date} - Date de création des données (YYYY-MM-DD)
#'     \item \code{consent_expiry_date} - Date d'expiration du consentement (datasets seulement)
#'     \item \code{data_destruction_date} - Date de destruction des données (datasets seulement)
#'     \item \code{sensitivity_level} - Niveau de sensibilité (datasets seulement)
#'     \item \code{ethical_stamp} - Tampon éthique (datasets seulement)
#'   }
#'   Exemple dataset: \code{list(creation_date = "2025-01-01", sensitivity_level = 1,
#'     ethical_stamp = "false", custom_field = "valeur personnalisée")}
#'   Exemple image: \code{list(creation_date = "2025-01-01", photographer = "Jean Doe",
#'     title = "Photo de démonstration")}
#' @param interactive Pour connexions datalake: mode interactif (défaut: TRUE)
#' @param pipeline Pour connexions datawarehouse: nom du pipeline (obligatoire)
#' @param file_batch Pour connexions datawarehouse: nom du batch (optionnel, NULL sinon)
#' @param file_version Pour connexions datawarehouse: version des données (optionnel, NULL sinon)
#'
#' @returns La liste des fichiers qui ont été injectés dans tube
#' @export
ellipse_push <- function(
  con, file_or_folder = NULL, dataset_name = NULL, tag = NULL,
  metadata = NULL, interactive = TRUE,
  pipeline = NULL, file_batch = NULL, file_version = NULL) {
  logger::log_debug("[ellipse_push] entering function")

  # Detect connection type (same logic as ellipse_query)
  schema_name <- DBI::dbGetInfo(con)$dbms.name
  is_datalake <- grepl("publicdatalake", schema_name, ignore.case = TRUE)

  if (is_datalake) {
    # Route to public datalake upload mode
    logger::log_debug("[ellipse_push] datalake mode - public datalake upload")
    ellipse_push_datalake_mode(con, file_or_folder, dataset_name, tag, metadata, interactive)
  } else {
    # Route to traditional landing zone mode
    # For datawarehouse mode, file_or_folder is still required
    if (is.null(file_or_folder)) {
      cli::cli_alert_danger("Oups, il faut fournir un fichier ou répertoire pour l'upload vers la landing zone! 😅")
      invisible(NULL)
    }
    logger::log_debug("[ellipse_push] traditional mode - landing zone upload")
    ellipse_push_landingzone_mode(con, file_or_folder, pipeline, file_batch, file_version)
  }
}

#' Traditional landing zone upload mode (legacy ellipse_ingest logic)
#' @keywords internal
ellipse_push_landingzone_mode <- function(con, file_or_folder, pipeline, file_batch = NULL, file_version = NULL) {
  env <- DBI::dbGetInfo(con)$profile_name

  if (!check_env(env)) {
    cli::cli_alert_danger(
      paste("Oups, il faut choisir un environnement! 😅\n\n",
        "Le paramètre `env` peut être \"PROD\" ou \"DEV\"",
        sep = ""
      )
    )
    return(invisible(NULL))
  }

  creds <- get_aws_credentials(env)

  landing_zone_bucket <- list_landing_zone_bucket(creds)

  if (is.null(landing_zone_bucket)) {
    cli::cli_alert_danger(
      paste0(
        "Oups, il semble que le bucket de la landing zone n'a ",
        "pas été trouvé! Contacter votre ingénieur de données 😅"
      )
    )
    return(invisible(NULL))
  }

  if (!check_file_versioning_before_ingest(file_batch, file_version)) {
    cli::cli_alert_danger("Contacter votre ingénieur de données! 😅")
    return(invisible(NULL))
  }

  landing_zone_partitions <- list_landing_zone_partitions(creds)
  if (!check_pipeline_before_ingest(pipeline, landing_zone_partitions, file_batch, file_version)) {
    cli::cli_alert_danger("Contacter votre ingénieur de données! 😅")
    return(invisible(NULL))
  }

  if (is.null(file_or_folder)) {
    cli::cli_alert_danger("Oups, il faut fournir un fichier ou un répertoire à injecter! 😅")
    return(invisible(NULL))
  }

  # check whether the file_or_folder is a file or a folder
  cli::cli_alert_info("Vérification des données à injecter dans tube...")
  folder_content <- parse_landing_zone_input(file_or_folder)

  cli::cli_alert_info("Les données sont en cours d'ingestion dans la landing zone...")

  # Initialize progress bar
  total_files <- length(folder_content)
  cli::cli_progress_bar(
    name = "Upload des fichiers",
    total = total_files,
    format = "{cli::pb_name} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
    clear = TRUE
  )

  # Loop with progress bar
  for (file in folder_content) {
    upload_file_to_landing_zone(creds, file, pipeline, file_batch, file_version)
    cli::cli_progress_update() # Update the progress bar
  }

  # TODO: do something better than the progress bar for 1 file : length(folder_content)
  cli::cli_alert_info("Les données ont été injectées dans la landing zone.\
  N'oubliez pas de vous déconnecter de la plateforme ellipse avec `ellipse_disconnect(...)` 👋.")
  return(invisible(folder_content))
}

#' Injecter de nouvelles données brutes manuellement dans tube via la landing zone
#'
#' @description DEPRECATED: Utilisez `ellipse_push()` à la place.
#' Cette fonction est maintenue pour la compatibilité descendante.
#'
#' @param con Un objet de connexion tel qu'obtenu via `tube::ellipse_connect()`.
#' @param file_or_folder Le chemin vers le répertoire qui contient les fichiers à charger dans tube
#' @param pipeline Le nom du pipeline qui doit être exécuté pour charger les données.
#' @param file_batch Le nom du batch qui doit être accollé aux données dans l'entrepôt de
#'   données. Utilisé pour les données factuelles seulement, NULL sinon.
#' @param file_version La version des données qui doit être accollée aux données dans
#'   l'entrepôt de données. Utilisé pour les données dimensionnelles et les dictionnaires
#'   seulement, NULL sinon.
#'
#' @returns La liste des fichiers qui ont été injectés dans tube
#' @export
ellipse_ingest <- function(con, file_or_folder, pipeline, file_batch = NULL, file_version = NULL) {
  # Delegate to ellipse_push for backward compatibility
  ellipse_push(con, file_or_folder,
    pipeline = pipeline, file_batch = file_batch,
    file_version = file_version, interactive = FALSE
  )
}


#' Publier un dataframe dans un datamart
#'
#' @param con Un objet de connexion tel qu'obtenu via `tube::ellipse_connect()`.
#' @param dataframe Le chemin vers le répertoire qui contient les fichiers à charger dans tube
#' @param datamart Le nom du pipeline qui doit être exécuté pour charger les données
#' @param table Le nom de la table qui doit être créée dans le datamart
#' @param data_tag Le tag à ajouter aux données pour les identifier à l'intérieur-même du
#'   jeu de données (une colonne tag est ajoutée au dataframe)
#' @param table_tags Les tags à ajouter à la table pour la catégoriser dans le datamart
#'   pour faciliter la découvrabilité des données dans le catalogue de données
#' @param table_description La description de la table à ajouter dans le datamart pour
#'   faciliter la découvrabilité des données dans le catalogue de données
#' @param unattended_options une liste nommée contenant les options de la commande de
#'   publication pour l'exécution de la tâche de publication de façon automatisée
#'   (pour les raffineurs de données)
#' - create_datamart: "oui" si on doit créer un nouveau datamart si celui spécifié n'existe pas, "non" sinon
#' - addto_or_replace_table: 1 pour ajouter des données à la table existante, 2 pour écraser la table existante
#' - are_you_sure: "oui" si on est certain de vouloir écraser la table existante, "non" sinon
#' - create_table: "oui" si on doit créer la table si elle n'existe pas, "non" sinon
#' - process_data: "oui" si on doit traiter les données maintenant, "non" sinon
#'
#' @returns TRUE si le dataframe a été envoyé dans le datamart  FALSE sinon.
#' @export
ellipse_publish <- function(
  con,
  dataframe,
  datamart,
  table,
  data_tag = NULL,
  table_tags = NULL,
  table_description = NULL,
  unattended_options = NULL) {
  env <- DBI::dbGetInfo(con)$profile_name
  schema <- DBI::dbGetInfo(con)$dbms.name

  danger <- function(msg) {
    if (is.null(unattended_options)) {
      cli::cli_alert_danger(msg)
    } else {
      logger::log_error(msg)
    }
  }

  info <- function(msg) {
    if (is.null(unattended_options)) {
      cli::cli_alert_info(msg)
    } else {
      logger::log_info(msg)
    }
  }

  success <- function(msg) {
    if (is.null(unattended_options)) {
      cli::cli_alert_success(msg)
    } else {
      logger::log_info(msg)
    }
  }

  # Protect datawarehouse from publishing
  if (grepl("datawarehouse", schema)) {
    danger("L'opération ellipse_publish n'est pas permis dans l'entrepôt de données (datawarehouse)! 😅")
    invisible(FALSE)
  }

  # if the x-amz-meta-category named element is not provided in table_tag, we add it
  if (is.null(table_tags)) {
    table_tags <- list()
  }
  if (!"x-amz-meta-category" %in% names(table_tags)) {
    table_tags$`x-amz-meta-category` <- "DatamartTable"
  }

  if (!check_params_before_publish(env, dataframe, datamart, table, data_tag, table_tags, table_description)) {
    invisible(FALSE)
  }

  if (!"x-amz-meta-datamart" %in% names(table_tags)) {
    table_tags$`x-amz-meta-datamart` <- datamart
  }

  dataframe <- dataframe |> dplyr::mutate(tag = data_tag)

  creds <- get_aws_credentials(env)
  dm_glue_database <- list_datamarts_database(creds)
  dm_bucket <- list_datamarts_bucket(creds)

  # check that the datamart exists by checking that the 1st level partition exists in the datamart bucket
  dm_partitions <- list_s3_partitions(creds, dm_bucket)
  dm_list <- lapply(dm_partitions, function(x) gsub("/$", "", x))
  if (is.null(datamart)) {
    danger("Oups, il faut fournir un datamart pour publier les données! 😅")
    FALSE
  }

  if (!datamart %in% dm_list) {
    danger("Le datamart fourni n'existe pas! 😅")
    # ask the user is we must create a new datamart
    if (ask_yes_no(
      question = "Voulez-vous créer un nouveau datamart?",
      unattended_option = unattended_options$create_datamart
    )) {
      cli::cli_alert_info("Création du datamart en cours...")
      # the file path will be created when the first file is uploaded with it in its key
    } else {
      danger("Publication des données abandonnée.")
      invisible(FALSE)
    }
  }

  # check that the table does not exist in the datamart in the form of s3://datamarts-bucket/datamart/table
  dm_folders <- list_s3_folders(creds, dm_bucket, paste0(datamart, "/"))

  if (table %in% dm_folders) {
    # ici on suppose que si le dossier datamart/table existe dans le bucket s3 des datamarts
    # alors la table GLUE existe aussi ce qui n'est possible pas le cas dans les situations
    # où la GLUE job n'a pas roulé
    danger("La table demandée existe déjà! 😅")

    choice <- ask_1_2(
      paste("Voulez-vous",
        "  1. ajouter des données à la table existante?",
        "  2. écraser la table existante?",
        "  Votre choix:",
        sep = "\n"
      ),
      unattended_option = unattended_options$addto_or_replace_table
    )

    if (choice == 1) {
      info("Ajout des données à la table existante en cours...")
      # append the dataframe to the table by uploading
      # upload the csv in s3://datamarts-bucket/datamart/table/unprocessed
      r <- upload_dataframe_to_datamart(creds, dataframe, dm_bucket, datamart, table)
      if (is.character(r) && r == "Unsupported column type") {
        danger("Il y a une colonne dont le type n'est pas pris en charge dans votre dataframe! 😅")
        invisible(FALSE)
      } else {
        if (r == FALSE) {
          danger("Il y a eu une erreur lors de la publication des données! 😅")
          invisible(FALSE)
        }
        success("Les données ont été ajoutées à la table existante.")
      }
    }

    if (choice == 2) {
      # confirm by the user
      if (!ask_yes_no("Êtes-vous certain.e de vouloir écraser la table existante?",
          unattended_option = unattended_options$are_you_sure
        )) {
        info("Publication des données abandonnée.")
        invisible(FALSE)
      }
      info("Ecrasement de la table existante en cours...")
      # delete the glue table
      r1 <- delete_glue_table(creds, dm_glue_database, paste0(datamart, "-", table))
      # delete the content of the folders :
      #  - s3://datamarts-bucket/datamart/table
      #  - s3://datamarts-bucket/datamart/table-output
      r2 <- delete_s3_folder(creds, dm_bucket, paste0(datamart, "/", table))
      r3 <- delete_s3_folder(creds, dm_bucket, paste0(datamart, "/", table, "-output"))

      if (r1 || (r2 && r3)) {
        success("La table a été écrasée avec succès.")
      } else {
        danger("Il y a eu une erreur lors de la suppression de la table dans la datamart! 😅")
        danger("Veuillez contacter votre ingénieur de données.")
        invisible(FALSE)
      }

      # upload new csv in s3://datamarts-bucket/datamart/table/unprocessed
      r <- upload_dataframe_to_datamart(creds, dataframe, dm_bucket, datamart, table)
      if (is.character(r) && r == "Unsupported column type") {
        danger("Il y a une colonne dont le type n'est pas pris en charge dans votre dataframe! 😅")
        invisible(FALSE)
      } else {
        if (r == FALSE) {
          danger("Il y a eu une erreur lors de la publication des données! 😅")
          invisible(FALSE)
        }
        success("La table existante a été écrasée et les nouvelles données ont été ajoutées.")
      }
    }
  } else {
    danger("La table demandée n'existe pas")
    if (ask_yes_no("Voulez-vous créer la table?",
        unattended_option = unattended_options$create_table
      )) {
      # create the glue table by uploading the csv in s3://datamarts-bucket/datamart/table/unprocessed
      info("Création de la table en cours...")
      r <- upload_dataframe_to_datamart(creds, dataframe, dm_bucket, datamart, table)
      if (is.character(r) && r == "Unsupported column type") {
        danger("Il y a une colonne dont le type n'est pas pris en charge dans votre dataframe! 😅")
        invisible(FALSE)
      } else {
        if (r == FALSE) {
          danger("Il y a eu une erreur lors de la publication des données! 😅")
          invisible(FALSE)
        }
        success("La table a été créée avec succès.")
      }
    } else {
      danger("Publication des données abandonnée.")
      invisible(FALSE)
    }
  }

  # At this point we have files in the datamart bucket under datamart/table/unprocessed
  # We can now trigger the glue job to process the files
  # The table will be created in the form of datamart-table
  # The glue job will move the files from unprocessed to processed
  # The glue job will also create the table in the datamart database
  if (ask_yes_no(
    paste("Voulez-vous traiter les données maintenant pour les rendre ",
      "disponibles immédiatement?  Si vous ne le faites pas maintenant, ",
      "le traitement sers déclenché automatiquement dans les 6 prochaines heures.",
      "  Votre choix",
      sep = "\n"
    ),
    unattended_option = unattended_options$process_data
  )) {
    glue_job <- list_glue_jobs(creds)
    run_glue_job(creds, glue_job, "datamarts", paste0(datamart, "/", table), table_tags, table_description)
    success("Le traitement des données a été déclenché avec succès.")
    info("Les données seront disponibles dans les prochaines minutes\n")
    info("N'oubliez pas de vous déconnecter de la plateforme ellipse avec `ellipse_disconnect(...)` 👋.")
  } else {
    success("Publication des données complétée avec succès")
    info("Les données seront disponibles dans les 6 prochaines heures")
    info("N'oubliez pas de vous déconnecter de la plateforme ellipse avec `ellipse_disconnect(...)` 👋.")
    invisible(FALSE)
  }
}

#' Retirer une table d'un datamart ou un datamart complet
#'
#' @param con Un objet de connexion tel qu'obtenu via `tube::ellipse_connect()`.
#' @param datamart Le nom du datamart contenant la table à retirer.
#' @param table Le nom de la table à retirer.  Lorsque toutes les tables sont retirées,
#' le datamart est détruit et supprimé de la plateforme
#'
#' @returns TRUE si la table a été retirée avec succès, FALSE sinon.
#' @export
ellipse_unpublish <- function(con, datamart, table) {
  env <- DBI::dbGetInfo(con)$profile_name
  schema <- DBI::dbGetInfo(con)$dbms.name

  # Protect datawarehouse from unpublishing
  if (grepl("datawarehouse", schema)) {
    cli::cli_alert_danger(
      "L'opération ellipse_unpublish n'est pas permis dans l'entrepôt de données (datawarehouse)! 😅"
    )
    invisible(FALSE)
  }

  if (!check_params_before_unpublish(env, datamart, table)) {
    invisible(FALSE)
  }

  creds <- get_aws_credentials(env)
  dm_bucket <- list_datamarts_bucket(creds)
  dm_glue_database <- list_datamarts_database(creds)


  # check that the datamart exists by checking that the 1st level partition exists in the datamart bucket
  dm_partitions <- list_s3_partitions(creds, dm_bucket)
  dm_list <- lapply(dm_partitions, function(x) gsub("/$", "", x))
  if (!datamart %in% dm_list) {
    cli::cli_alert_danger("Le datamart fourni n'existe pas! 😅")
    invisible(FALSE)
  }

  # check that the table exists in the datamart in the form of s3://datamarts-bucket/datamart/table
  dm_folders <- list_s3_folders(creds, dm_bucket, paste0(datamart, "/"))

  if (!table %in% dm_folders) {
    cli::cli_alert_danger("La table demandée n'existe pas! 😅")
    invisible(FALSE)
  }

  # confirm by the user
  if (!ask_yes_no("Êtes-vous certain.e de vouloir retirer la table?")) {
    cli::cli_alert_info("Retrait de la table abandonné.")
    invisible(FALSE)
  }

  cli::cli_alert_info("Retrait de la table en cours...")

  # delete the glue table
  r1 <- delete_glue_table(creds, dm_glue_database, paste0(datamart, "-", table))

  # delete the content of the folder s3://datamarts-bucket/datamart/table
  r2 <- delete_s3_folder(creds, dm_bucket, paste0(datamart, "/", table))
  r3 <- delete_s3_folder(creds, dm_bucket, paste0(datamart, "/", table, "-output"))

  if (r1 && r2 && r3) {
    cli::cli_alert_success("La table a été retirée avec succès.")
    invisible(TRUE)
  } else {
    cli::cli_alert_danger("Il y a eu une erreur lors du retrait de la table! 😅")
    cli::cli_alert_danger("Veuillez contacter votre ingénieur de données.")
    invisible(FALSE)
  }
}


#' Changer pes proprités d'une table dans un datamart ou dans la datawarehouse
#'
#' @param con Un objet de connexion tel qu'obtenu via `tube::ellipse_connect()`.
#' @param table Le nom de la table à modifier.
#' @param new_table_tags Les nouveaux tags à ajouter à la table pour la catégoriser dans
#' le datamart pour faciliter la découvrabilité des données dans le catalogue de données
#' @param new_table_desc La nouvelle description de la table à ajouter dans le datamart
#' pour faciliter la découvrabilité des données dans le catalogue de données
#'
#' @returns TRUE si la table a été modifiée avec succès, FALSE sinon.
#' @export
ellipse_describe <- function(con, table, new_table_tags = NULL, new_table_desc = NULL) {
  env <- DBI::dbGetInfo(con)$profile_name
  schema <- DBI::dbGetInfo(con)$dbms.name

  # If schema contains datawarehouse, exit the function BEFORE any AWS calls
  if (grepl("datawarehouse", schema)) {
    cli::cli_alert_danger("L'opération ellipse_describe n'est pas permis dans l'entrepôt de données (datawarehouse)! 😅")
    return(invisible(FALSE))
  }

  creds <- get_aws_credentials(env)

  if (!check_params_before_describe(env, schema, table, new_table_tags, new_table_desc)) {
    return(invisible(FALSE))
  }

  table_props <- list_glue_table_properties(creds, schema, table)

  cli::cli_rule()

  if (!is.null(table_props$description)) {
    cli::cli_alert_info("Description actuelle de la table:")
    cli::cli_text(cli::col_cyan(table_props$description))
    cli::cli_text("")
  }

  if (!is.null(table_props$table_tags)) {
    cli::cli_alert_info("Tags actuels de la table")
    print_list_with_nulls(unlist(table_props$table_tags))
  }

  cli::cli_rule()

  # add x-amz-meta- prefix to the tags if not already present
  if (!is.null(new_table_tags) && length(new_table_tags) > 0) {
    new_table_tags <- setNames(
      new_table_tags,
      ifelse(
        !sapply(grepl("x-amz-meta-", names(new_table_tags)), \(x) x),
        paste0("x-amz-meta-", names(new_table_tags)),
        names(new_table_tags)
      )
    )
  }

  # if there are new tags in new_table_tags that are not in the current table tags, we add them
  # if there are tags in new_table_tags that are also in the current table tags and have diffrent
  # values, we update them
  current_table_tags <- unlist(table_props$table_tags)

  if (!is.null(new_table_tags) && length(new_table_tags) > 0) {
    tags_to_add <- new_table_tags[!names(new_table_tags) %in% names(current_table_tags)]
    tags_to_update <- new_table_tags[names(new_table_tags) %in% names(current_table_tags)]
    tags_to_update <- tags_to_update[tags_to_update != current_table_tags[names(tags_to_update)]]
    new_tags <- c(tags_to_add, tags_to_update)
  } else {
    new_tags <- NULL
  }

  # add x-amz-meta- prefix to the tags if not already present
  if (!is.null(new_tags) && length(new_tags) > 0) {
    new_tags <- setNames(
      new_tags,
      ifelse(
        !sapply(grepl("x-amz-meta-", names(new_tags)), \(x) x),
        paste0("x-amz-meta-", names(new_tags)),
        names(new_tags)
      )
    )
  }

  change_tags <- FALSE
  change_desc <- FALSE

  if (!is.null(new_tags) && length(new_tags) > 0) {
    # confirm by the user
    cli::cli_alert_info("Les changements apportés sur les tags seront:")
    print_list_with_nulls(new_tags)
    change_tags <- TRUE
  } else {
    cli::cli_alert_info("Aucun changement n'est à faire sur les tags.")
  }

  cli::cli_text("")

  if (!is.null(new_table_desc) && nchar(new_table_desc) != 0 &&
      (is.na(table_props$description) || table_props$description != new_table_desc)) {
    cli::cli_alert_info("La nouvelle description de la table sera:")
    cli::cli_alert_info(cli::col_cyan(new_table_desc))
    change_desc <- TRUE
  } else {
    cli::cli_alert_info("Aucun changement n'est à faire sur la description.")
  }

  cli::cli_rule()
  if (change_tags || change_desc) {
    confirm_change <- ask_yes_no("Voulez-vous vraiment changer les propriétés de la table?")

    if (!confirm_change) {
      cli::cli_alert_info("Les changements ont été abandonnés.")
      return(invisible(FALSE))
    }
  } else {
    cli::cli_alert_info("Aucun changement n'est requis.")
    return(invisible(TRUE))
  }

  if (change_tags || change_desc) {
    # update the table tags and description
    if (change_tags) {
      cli::cli_alert_info("Mise à jour des tags de la table en cours...")

      r1 <- update_glue_table_tags(creds, schema, table, new_tags)

      if (r1) {
        cli::cli_alert_success("Les tags de la table ont été mises à jour avec succès.")
      } else {
        cli::cli_alert_danger("Il y a eu une erreur lors de la mise à jour des tags de la table! 😅")
        cli::cli_alert_danger("Veuillez contacter votre ingénieur de données.")
        invisible(FALSE)
      }
    }

    if (change_desc) {
      cli::cli_alert_info("Mise à jour de la description de la table en cours...")
      r2 <- update_glue_table_desc(creds, schema, table, new_table_desc)
      if (r2) {
        cli::cli_alert_success("La description de la table a été mise à jour avec succès.")
      } else {
        cli::cli_alert_danger("Il y a eu une erreur lors de la mise à jour de la description de la table! 😅")
        cli::cli_alert_danger("Veuillez contacter votre ingénieur de données.")
        invisible(FALSE)
      }
    }
  }

  invisible(TRUE)
}


#' Traiter les données en attente d'être insérées dans une table
#'
#' @param con Un objet de connexion tel qu'obtenu via `tube::ellipse_connect()`.
#' @param table Le nom de la table qui doit traitée
#'
#' @returns TRUE si les données en attente d'être insérées ont bien été traitées.
#' FALSE sinon.
#' @export
ellipse_process <- function(con, table) {
  env <- DBI::dbGetInfo(con)$profile_name
  schema <- DBI::dbGetInfo(con)$dbms.name
  creds <- get_aws_credentials(env)

  if (!check_params_before_refresh(con, schema, table)) {
    invisible(FALSE)
  }

  # confirm by the user
  if (!ask_yes_no("Êtes-vous certain.e de vouloir rafraîchir la table?")) {
    cli::cli_alert_info("Rafraîchissement de la table abandonné.")
    invisible(FALSE)
  }

  cli::cli_alert_info("Rafraîchissement de la table en cours...")
  glue_job <- list_glue_jobs(creds)

  database <- dplyr::case_when(
    grepl("datawarehouse", schema) ~ "datawarehouse",
    grepl("datamart", schema) ~ "datamarts",
    TRUE ~ NA
  )

  if (is.na(database)) {
    cli::cli_alert_danger("Oups, il semble que la base de données n'a pas été trouvée! 😅")
    invisible(FALSE)
  }

  logger::log_debug(paste(
    "[ellipse_process] about to run glue job on database = ",
    database, " schema = ", schema, " table = ", table
  ))

  r <- run_glue_job(creds, glue_job, database, table, NULL, NULL)

  if (r) {
    if (r == -1) {
      cli::cli_alert_info("Il n'y a aucune nouvelle donnée à traiter.")
    } else {
      cli::cli_alert_success("Le traitement des données a été déclenché avec succès.")
      cli::cli_alert_info("Les données seront disponibles dans les prochaines minutes\n")
      cli::cli_alert_info(
        "N'oubliez pas de vous déconnecter de la plateforme ellipse avec `ellipse_disconnect(...)` 👋."
      )
    }
    invisible(TRUE)
  } else {
    cli::cli_alert_danger("Il y a eu une erreur lors du rafraîchissement de la table! 😅")
    cli::cli_alert_danger("Veuillez contacter votre ingénieur de données.")
    invisible(FALSE)
  }
}

#' Retirer des données du datalake public
#'
#' Cette fonction permet de supprimer des datasets, des tags spécifiques,
#' ou des fichiers individuels du datalake public. Elle peut supprimer
#' un fichier spécifique, un tag complet, ou le dataset entier avec tous ses tags.
#'
#' @param con Un objet de connexion tel qu'obtenu via `tube::ellipse_connect()`.
#'   Doit être une connexion au datalake public.
#' @param dataset_name Le nom du dataset à supprimer (obligatoire).
#' @param tag Le tag spécifique à supprimer. Si NULL, supprime le dataset entier
#'   avec tous ses tags (optionnel).
#' @param filename Le nom du fichier spécifique à supprimer dans le tag
#'   (partie après tag/). Si spécifié, le paramètre tag devient obligatoire (optionnel).
#'
#' @returns TRUE si la suppression a été effectuée avec succès, FALSE sinon.
#' @export
ellipse_unpush <- function(con, dataset_name, tag = NULL, filename = NULL) {
  logger::log_debug("[ellipse_unpush] entering function")
  
  # Validate connection type - must be datalake
  schema_name <- DBI::dbGetInfo(con)$dbms.name
  is_datalake <- grepl("publicdatalake", schema_name, ignore.case = TRUE)
  
  if (!is_datalake) {
    cli::cli_alert_danger(
      "L'opération ellipse_unpush fonctionne seulement avec les connexions datalake! 😅"
    )
    return(invisible(FALSE))
  }
  
  # Get environment and credentials
  env <- DBI::dbGetInfo(con)$profile_name
  
  if (!check_env(env)) {
    cli::cli_alert_danger(
      paste("Oups, il faut choisir un environnement! 😅\n\n",
        "Le paramètre `env` peut être \"PROD\" ou \"DEV\"",
        sep = ""
      )
    )
    return(invisible(FALSE))
  }
  
  # Validate required parameters
  if (is.null(dataset_name) || nchar(dataset_name) == 0) {
    cli::cli_alert_danger("Le nom du dataset est requis! 😅")
    return(invisible(FALSE))
  }
  
  # If filename is specified, tag is required
  if (!is.null(filename) && (is.null(tag) || nchar(tag) == 0)) {
    cli::cli_alert_danger("Le paramètre 'tag' est requis quand 'filename' est spécifié! 😅")
    return(invisible(FALSE))
  }
  
  creds <- get_aws_credentials(env)
  bucket <- list_public_datalake_bucket(creds)
  
  if (is.null(bucket) || length(bucket) == 0) {
    cli::cli_alert_danger("Impossible de trouver le bucket du datalake public! 😅")
    return(invisible(FALSE))
  }
  
  # Use first bucket if multiple
  if (length(bucket) > 1) {
    bucket <- bucket[1]
  }
  
  # Check if dataset exists
  datasets <- list_s3_folders(creds, bucket, "")
  
  if (is.null(datasets) || !dataset_name %in% datasets) {
    cli::cli_alert_danger("Le dataset '{dataset_name}' n'existe pas! 😅")
    return(invisible(FALSE))
  }
  
  # If specific tag requested, validate it exists
  if (!is.null(tag)) {
    tags <- list_s3_folders(creds, bucket, paste0(dataset_name, "/"))
    
    if (is.null(tags) || !tag %in% tags) {
      cli::cli_alert_danger("Le tag '{tag}' n'existe pas dans le dataset '{dataset_name}'! 😅")
      return(invisible(FALSE))
    }
    
    # If specific filename requested, validate it exists
    if (!is.null(filename)) {
      s3_client <- paws.storage::s3(config = c(creds, close_connection = TRUE))
      prefix <- paste0(dataset_name, "/", tag, "/")
      
      r <- tryCatch({
        s3_client$list_objects_v2(Bucket = bucket, Prefix = prefix)
      }, error = function(e) NULL)
      
      if (is.null(r) || is.null(r$Contents) || length(r$Contents) == 0) {
        cli::cli_alert_danger("Aucun fichier trouvé dans le tag '{tag}' du dataset '{dataset_name}'! 😅")
        return(invisible(FALSE))
      }
      
      # Check if the specific filename exists
      file_keys <- sapply(r$Contents, function(obj) obj$Key)
      target_key <- paste0(dataset_name, "/", tag, "/", filename)
      
      if (!target_key %in% file_keys) {
        cli::cli_alert_danger("Le fichier '{filename}' n'existe pas dans le tag '{tag}' du dataset '{dataset_name}'! 😅")
        return(invisible(FALSE))
      }
    }
  }
  
  # Prepare deletion summary
  cli::cli_rule()
  
  if (is.null(tag)) {
    # Full dataset deletion
    tags <- list_s3_folders(creds, bucket, paste0(dataset_name, "/"))
    
    if (is.null(tags) || length(tags) == 0) {
      cli::cli_alert_warning("Le dataset '{dataset_name}' semble vide.")
      return(invisible(TRUE))
    }
    
    cli::cli_text("⚠️ SUPPRESSION COMPLÈTE DU DATASET")
    cli::cli_text("Dataset: {dataset_name}")
    cli::cli_text("Tags à supprimer: {length(tags)}")
    
    for (tag_name in tags) {
      cli::cli_text("  • {tag_name}")
    }
    
    # Count total files across all tags and collect file paths
    total_files <- 0
    all_file_paths <- c()
    s3_client <- paws.storage::s3(config = c(creds, close_connection = TRUE))
    
    for (tag_name in tags) {
      prefix <- paste0(dataset_name, "/", tag_name, "/")
      r <- tryCatch({
        s3_client$list_objects_v2(Bucket = bucket, Prefix = prefix)
      }, error = function(e) NULL)
      
      if (!is.null(r) && !is.null(r$Contents)) {
        total_files <- total_files + length(r$Contents)
        # Extract file paths relative to dataset
        tag_files <- sapply(r$Contents, function(obj) {
          # Remove dataset_name/ prefix to show relative path
          gsub(paste0("^", dataset_name, "/"), "", obj$Key)
        })
        all_file_paths <- c(all_file_paths, tag_files)
      }
    }
    
    cli::cli_text("Fichiers à supprimer: {total_files}")
    if (length(all_file_paths) > 0) {
      for (file_path in all_file_paths) {
        cli::cli_text("  • {file_path}")
      }
    }
    
    deletion_target <- paste0(dataset_name, "/")
    confirmation_msg <- paste(
      "Êtes-vous certain.e de vouloir supprimer TOUT le dataset", dataset_name, "et TOUS ses tags?"
    )

  } else if (is.null(filename)) {
    # Specific tag deletion (but not specific file)
    cli::cli_alert_info("🏷️ SUPPRESSION D'UN TAG SPÉCIFIQUE")
    cli::cli_text("Dataset: {dataset_name}")
    cli::cli_text("Tag: {tag}")
    
    # Count files in this tag and collect file paths
    prefix <- paste0(dataset_name, "/", tag, "/")
    s3_client <- paws.storage::s3(config = c(creds, close_connection = TRUE))
    
    r <- tryCatch({
      s3_client$list_objects_v2(Bucket = bucket, Prefix = prefix)
    }, error = function(e) NULL)
    
    files_to_delete <- if (!is.null(r) && !is.null(r$Contents)) length(r$Contents) else 0
    
    # Force variable usage for linter
    message_text <- sprintf("Fichiers à supprimer: %d", files_to_delete)
    cli::cli_text(message_text)
    
    if (!is.null(r) && !is.null(r$Contents) && length(r$Contents) > 0) {
      cli::cli_text("Fichiers concernés:")
      for (obj in r$Contents) {
        # Remove dataset_name/ prefix to show relative path
        relative_path <- gsub(paste0("^", dataset_name, "/"), "", obj$Key)
        cli::cli_text("  • {relative_path}")
      }
    }
    
    deletion_target <- paste0(dataset_name, "/", tag, "/")
    confirmation_msg <- "Êtes-vous certain.e de vouloir supprimer le tag '{tag}' du dataset '{dataset_name}'?"
  } else {
    # Specific file deletion
    cli::cli_alert_info("📄 SUPPRESSION D'UN FICHIER SPÉCIFIQUE")
    cli::cli_text("Dataset: {dataset_name}")
    cli::cli_text("Tag: {tag}")
    cli::cli_text("Fichier: {filename}")
    
    deletion_target <- paste0(dataset_name, "/", tag, "/", filename)
    confirmation_msg <- paste0(
      "Êtes-vous certain.e de vouloir supprimer le fichier '", filename,
      "' du tag '", tag, "' dans le dataset '", dataset_name, "'?"
    )
  }
  
  cli::cli_rule()
  
  # User confirmation
  if (!ask_yes_no(confirmation_msg)) {
    cli::cli_alert_info("Suppression annulée.")
    return(invisible(FALSE))
  }
  
  # Execute deletion
  cli::cli_alert_info("Suppression en cours...")
  
  if (!is.null(filename)) {
    # Single file deletion - use direct S3 object deletion
    s3_client <- paws.storage::s3(config = c(creds, close_connection = TRUE))
    
    deletion_success <- tryCatch({
      s3_client$delete_object(Bucket = bucket, Key = deletion_target)
      TRUE
    }, error = function(e) {
      logger::log_error(paste("Failed to delete file:", e$message))
      FALSE
    })
  } else {
    # Folder deletion - use existing delete_s3_folder function
    deletion_success <- delete_s3_folder(creds, bucket, deletion_target)
  }
  
  if (!deletion_success) {
    cli::cli_alert_danger("❌ Erreur lors de la suppression des fichiers S3!")
    return(invisible(FALSE))
  }
  
  cli::cli_alert_success("✅ Fichiers supprimés avec succès.")
  
  # Trigger lambda re-indexing (required)
  cli::cli_alert_info("Déclenchement de la réindexation...")
  
  lambda_success <- invoke_datalake_indexing_lambda(creds)
  
  if (!lambda_success) {
    cli::cli_alert_danger("❌ Erreur lors de la réindexation!")
    cli::cli_alert_danger("Les fichiers ont été supprimés mais l'index n'a pas été mis à jour.")
    cli::cli_alert_danger("Contactez votre ingénieur de données.")
    return(invisible(FALSE))
  }
  
  cli::cli_alert_success("✅ Réindexation déclenchée avec succès!")
  
  if (is.null(tag)) {
    cli::cli_alert_success("Dataset '{dataset_name}' supprimé complètement.")
  } else if (is.null(filename)) {
    cli::cli_alert_success("Tag '{tag}' supprimé du dataset '{dataset_name}'.")
  } else {
    cli::cli_alert_success("Fichier '{filename}' supprimé du tag '{tag}' dans le dataset '{dataset_name}'.")
  }
  
  cli::cli_alert_info("Les données ne seront plus disponibles dans ellipse_discover() dans quelques minutes.")
  cli::cli_alert_info("N'oubliez pas de vous déconnecter avec ellipse_disconnect(con) 👋")
  
  invisible(TRUE)
}
