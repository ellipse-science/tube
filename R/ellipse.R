#' Se connecter à la plateforme de données ellipse sur AWS
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
  database  = "datawarehouse"
) {

  if (!check_env(env)) {
    cli::cli_alert_danger(paste("Oups, il faut choisir un environnement! 😅\n\n",
                                "Le paramètre `env` peut être \"PROD\" ou \"DEV\"",
                                sep = ""))
    return(invisible(NULL))
  }
  cli::cli_alert_info(paste("Environnement:", env))

  if (!check_database(database)) {
    cli::cli_alert_danger(paste("Oups, il faut choisir une base de données! 😅\n\n",
                                "Le paramètre `database` peut être \"datawarehouse\" ou \"datamarts\"",
                                sep = ""))
    return(invisible(NULL))
  }
  cli::cli_alert_info(paste("Database:", database))

  aws_access_key_id <-
    switch(env,
           "PROD" = "AWS_ACCESS_KEY_ID_PROD",
           "DEV"  = "AWS_ACCESS_KEY_ID_DEV") |>
    Sys.getenv()

  aws_secret_access_key <-
    switch(env,
           "PROD" = "AWS_SECRET_ACCESS_KEY_PROD",
           "DEV"  = "AWS_SECRET_ACCESS_KEY_DEV") |>
    Sys.getenv()

  if (aws_access_key_id == "" || aws_secret_access_key == "") {
    usage <-
      paste("On a besoin de vos clés d'accès sur AWS pour se connecter!\n\n",
            "Dans le fichier ~/.Renviron, ajoutez les lignes:\n\n",
            "AWS_ACCESS_KEY_ID_PROD=<votre access key id de production>\n",
            "AWS_SECRET_ACCESS_KEY_PROD=<votre secret access key de production>\n",
            "AWS_ACCESS_KEY_ID_DEV=<votre access key id de développement>\n",
            "AWS_SECRET_ACCESS_KEY_DEV=<votre secret access key de développement>\n\n",
            "Puis, redémarrez la session R.")
    cli::cli_alert_danger(usage)
    return(invisible(NULL))
  }

  # https://github.com/ellipse-science/tube/issues/16
  Sys.setenv("AWS_ACCESS_KEY_ID" = aws_access_key_id)
  Sys.setenv("AWS_SECRET_ACCESS_KEY" = aws_secret_access_key)

  creds <- get_aws_credentials(env)

  aws_access_key_id <- creds$credentials$creds$access_key_id
  aws_secret_access_key <- creds$credentials$creds$secret_access_key

  datawarehouse_database <- list_datawarehouse_database(creds)
  datamarts_database <- list_datamarts_database(creds)
  athena_staging_bucket <- list_athena_staging_bucket(creds)

  schema_name <- switch(database,
                        "datawarehouse" = paste0(datawarehouse_database),
                        "datamarts" = paste0(datamarts_database),
                        database)

  cli::cli_alert_info("Pour déconnecter: tube::ellipse_disconnect(objet_de_connexion)")
  con <- DBI::dbConnect(noctua::athena(),
                 aws_access_key_id = aws_access_key_id,
                 aws_secret_access_key = aws_secret_access_key,
                 schema_name = schema_name,
                 profile_name = env,
                 work_group = "ellipse-work-group",
                 s3_staging_dir = paste0("s3://",athena_staging_bucket))

  schema <- DBI::dbGetInfo(con)$dbms.name

  if (length(schema) > 0) {
    cli::cli_alert_info(paste("Base de données:", schema))
    cli::cli_alert_success("Connexion établie avec succès! 👍")
    return(con)
  } else {
    cli::cli_alert_danger("Oups, cette connection n'a pas de base de données! 😅")
    return(invisible(NULL))
  }
}

#' Se déconnecter de la plateforme de données ellipse
#' @returns TRUE if the connexion was closed or FALSE if no connexion existed
#' @export
ellipse_disconnect <- function(con = NULL) {
  if (is.null(con)) {
    cli::cli_alert_danger("Oups! Il faut fournir un objet de connection! 😅")
    return(invisible(FALSE))
  }

  tryCatch({
    if (DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
      cli::cli_alert_success("La connexion a été fermée avec succès! 👋")
      return(invisible(TRUE))
    } else {
      cli::cli_alert_warning("Il semble que la connexion n'existe pas ou soit déjà close! 😅")
      return(invisible(FALSE))
    }
  }, error = function(e) {
    cli::cli_alert_danger("Oups, il semble que la connexion n'a pas pu être fermée! 😅")
    return(invisible(FALSE))
  })
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
  df <- ellipse_discover(con, table) |> dplyr::filter(is_partition)
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
#' @param con Un objet de connexion tel qu'obtenu via `tube::ellipse_connect()`.
#' @param table Une table pour laquelle on veut obtenir les informations.
#'
#' @returns Un `tibble` contenant les tables diposnibles dans l'entrepôt de
#'   données, ou un descriptino des colonnes pour une table en particulier.
#'
#' @export
ellipse_discover <- function(con, table = NULL) {
  schema <- DBI::dbGetInfo(con)$dbms.name
  creds <- get_aws_credentials(DBI::dbGetInfo(con)$profile_name)

  if (length(schema) == 0) {
    cli::cli_alert_danger("Oups, cette connection n'a pas de base de données! 😅")
    return(invisible(NULL))
  }

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
      df <- list_glue_tables(creds, schema) |>
        dplyr::filter(table_name == table)
      return(df)
    } else {
      if (length(grep(table, tables)) > 1) {
        cli::cli_alert_info("Plusieurs tables correspondent à votre recherche (voir résultat retourné).")
        cli::cli_alert_info("Veuillez préciser votre recherche pour explorer la table.")
        tables <- tables[grep(table, tables)]
      }
    }
  }

  tables_properties <- lapply(tables, function(table) {
    list_glue_table_properties(creds, schema, table)
  }) |> dplyr::bind_rows() |> dplyr::select(-c(location))

  tables_tibble <- tibble::tibble(table_name = tables) |>
    dplyr::left_join(tables_properties, by = "table_name")

  tables_tibble |>
    dplyr::mutate(categorie =
      dplyr::case_when(
      startsWith(table_name, "a-")    ~ "Agora+",
      startsWith(table_name, "c-")    ~ "Civimètre+",
      startsWith(table_name, "r-")    ~ "Radar+",
      startsWith(table_name, "dict-") ~ "Dictionnaire",
      startsWith(table_name, "dim-")  ~ "Dimension",
      !is.na(table_tags[["x-amz-meta-category"]]) ~ table_tags[["x-amz-meta-category"]],
      TRUE ~ "Autre"
      ))
}

#' Lire et exploiter une table contenue dans l'entrepôt de données ellipse
#'
#' @param con Un objet de connexion tel qu'obtenu via `tube::ellipse_connect()`.
#' @param table Une table que l'on souhaite interroger avec `dplyr`.
#'
#' @returns Une table Athena qui peut être interrogée dans un _pipeline_
#'   `dplyr`.
#' @export
ellipse_query <- function(con, table) {
  schema_name <- DBI::dbGetInfo(con)$dbms.name

  tables <- DBI::dbGetQuery(
    con, paste0("SHOW TABLES IN ", schema_name)
  )$tab_name
  
  if (!table %in% tables) {
    cli::cli_alert_danger("La table demandée est inconnue.")
    return(NULL)
  }
  dplyr::tbl(con, table)
}


#' Injecter de nouvelles données brutes manuellement dans tube via la landing zone
#' Le processus consiste à envoyer un fichier unique ou un dossier contenant plusieurs fichiers
#' vers la plateforme de données pour qu'ils soient transformés en données structurées (lignes/colonnes)
#' dans une table de la datawarehouse.
#'
#' @param con Un objet de connexion tel qu'obtenu via `tube::ellipse_connect()`.
#' @param file_or_folder Le chemin vers le répertoire qui contient les fichiers à charger dans tube
#' @param pipeline Le nom du pipeline qui doit être exécuté pour charger les données.  Cela va va déterminer dans quelle table de données les données vont être injectées.
#' @param file_batch Le nom du batch qui doit être accollé aux données dans l'entrepôt de données.  Utilisé pour les données factuelles seulement, NULL sinon.  Si NULL, il faut fournir un file_version.
#' @param file_version La version des données qui doit être accollée aux données dans l'entrepôt de données. Utilisé pour les données dimensionnelles et les dictionnaires seulement, NULL sinon.  Si NULL, il faut fournir un file_batch.
#'
#' @returns La liste des fichiers qui ont été injectés dans tube
#' @export 
ellipse_ingest <- function(con, file_or_folder, pipeline, file_batch = NULL, file_version = NULL) {
  env <- DBI::dbGetInfo(con)$profile_name

  if (!check_env(env)) {
    cli::cli_alert_danger(paste("Oups, il faut choisir un environnement! 😅\n\n",
                                "Le paramètre `env` peut être \"PROD\" ou \"DEV\"",
                                sep = ""))
    return(invisible(NULL))
  }

  creds <- get_aws_credentials(env)

  landing_zone_bucket <- list_landing_zone_bucket(creds)

  if (is.null(landing_zone_bucket)) {
    cli::cli_alert_danger("Oups, il semble que le bucket de la landing zone n'a pas été trouvé! Contacter votre ingénieur de données 😅")
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
  # Create a progress bar object
  pb <- progress::progress_bar$new(
    format = "  uploading files [:bar] :percent eta: :eta",
    total = length(folder_content), # total number of iterations
    clear = FALSE,
    width = 60
  )

  # Loop with progress bar
  for (file in folder_content) {
    upload_file_to_landing_zone(creds, file, pipeline, file_batch, file_version)
    pb$tick() # Update the progress bar
  }

  # TODO: do something better than the progress bar for 1 file : length(folder_content)
  cli::cli_alert_info("Les données ont été injectées dans la landing zone.\
  N'oubliez pas de vous déconnecter de la plateforme ellipse avec `ellipse_disconnect(...)` 👋.")
  return(invisible(folder_content))
}


#' Publier un dataframe dans un datamart
#'
#' @param con Un objet de connexion tel qu'obtenu via `tube::ellipse_connect()`.
#' @param dataframe Le chemin vers le répertoire qui contient les fichiers à charger dans tube
#' @param datamart Le nom du pipeline qui doit être exécuté pour charger les données
#' @param table Le nom de la table qui doit être créée dans le datamart
#'
#' @returns TRUE si le dataframe a été envoyé dans le datamart  FALSE sinon.
#' @export
ellipse_publish <- function(con, dataframe, datamart, table, data_tag = NULL, table_tags = NULL, table_description = NULL) {
  env <- DBI::dbGetInfo(con)$profile_name
  
  # if the x-amz-meta-category named element is not provided in table_tag, we add it 
  if (is.null(table_tags)) {
    table_tags <- list()
  }
  if (!"x-amz-meta-category" %in% names(table_tags)) {
    table_tags$`x-amz-meta-category` <- "Datamart table"
  }

  if (!check_params_before_publish(env, dataframe, datamart, table, data_tag, table_tags, table_description)) {
    return(invisible(FALSE))
  }

  dataframe <- dataframe |> dplyr::mutate(tag = data_tag)
  
  creds <- get_aws_credentials(env)
  dm_glue_database <- list_datamarts_database(creds)
  dm_bucket <- list_datamarts_bucket(creds)

  # check that the datamart exists by checking that the 1st level partition exists in the datamart bucket
  dm_partitions <- list_s3_partitions(creds, dm_bucket)
  dm_list <- lapply(dm_partitions, function(x) gsub("/$", "", x))
  if (is.null(datamart)) {
    cli::cli_alert_danger("Oups, il faut fournir un datamart pour publier les données! 😅")
    return(FALSE)
  }

  if (!datamart %in% dm_list) {
    cli::cli_alert_danger("Le datamart fourni n'existe pas! 😅")
    # ask the user is we must create a new datamart
    if (ask_yes_no("Voulez-vous créer un nouveau datamart?")) {
      cli::cli_alert_info("Création du datamart en cours...")
      # the file path will be created when the first file is uploaded with it in its key
    } else {
      cli::cli_alert_danger("Publication des données abandonnée.")
      return(invisible(FALSE))
    }
  }

  # check that the table does not exist in the datamart in the form of s3://datamarts-bucket/datamart/table
  dm_folders <- list_s3_folders(creds, dm_bucket, paste0(datamart, "/"))

  if (table %in% dm_folders) {
    # ici on suppose que si le dossier datamart/table existe dans le bucket s3 des datamarts
    # alors la table GLUE existe aussi ce qui n'est possible pas le cas dans les situations 
    # où la GLUE job n'a pas roulé
    cli::cli_alert_danger("La table demandée existe déjà! 😅")

    choice <- ask_1_2(paste("Voulez-vous",
      "  1. ajouter des données à la table existante?",
      "  2. écraser la table existante?",
      "  Votre choix:", sep = "\n"))

    if (choice == 1) {
      cli::cli_alert_info("Ajout des données à la table existante en cours...")
      # append the dataframe to the table by uploading
      # upload the csv in s3://datamarts-bucket/datamart/table/unprocessed
      r <- upload_dataframe_to_datamart(creds, dataframe, dm_bucket, datamart, table)
      if (r) {
        cli::cli_alert_success("Les données ont été ajoutées à la table existante.")
      } else {
        cli::cli_alert_danger("Il y a eu une erreur lors de la publication des données! 😅")
        return(invisible(FALSE))
      }
    }

    if (choice == 2) {
      # confirm by the user
      if (!ask_yes_no("Êtes-vous certain.e de vouloir écraser la table existante?")) {
        cli::cli_alert_info("Publication des données abandonnée.")
        return(invisible(FALSE))
      }
      cli::cli_alert_info("Ecrasement de la table existante en cours...")
      # delete the glue table
      r1 <- delete_glue_table(creds, dm_glue_database, paste0(datamart, "-", table))
      # delete the content of the folder s3://datamarts-bucket/datamart/table and s3://datamarts-bucket/datamart/table-output
      r2 <- delete_s3_folder(creds, dm_bucket, paste0(datamart, "/", table))
      r3 <- delete_s3_folder(creds, dm_bucket, paste0(datamart, "/", table, "-output"))
  
      if (r1 && r2 && r3) {
        cli::cli_alert_success("La table a été écrasée avec succès.")
      } else {
        cli::cli_alert_danger("Il y a eu une erreur lors de la suppression de la table dans la datamart! 😅")
        cli::cli_alert_danger("Veuillez contacter votre ingénieur de données.")
        return(invisible(FALSE))
      }
      
      # upload new csv in s3://datamarts-bucket/datamart/table/unprocessed
      r <- upload_dataframe_to_datamart(creds, dataframe, dm_bucket, datamart, table)
      if (r) {
        cli::cli_alert_success("La table existante a été écrasée et les nouvelles données ont été ajoutées.")
      } else {
        cli::cli_alert_danger("Il y a eu une erreur lors de la publication des données! 😅")
        return(invisible(FALSE))
      }
    }
  } else {
    cli::cli_alert_danger("La table demandée n'existe pas")
    if (ask_yes_no("Voulez-vous créer la table?")) {
      # create the glue table by uploading the csv in s3://datamarts-bucket/datamart/table/unprocessed
      cli::cli_alert_info("Création de la table en cours...")
      r <- upload_dataframe_to_datamart(creds, dataframe, dm_bucket, datamart, table)
      if (r) {
        cli::cli_alert_success("La table a été créée avec succès.")
      } else {
        cli::cli_alert_danger("Il y a eu une erreur lors de la publication des données! 😅")
        return(invisible(FALSE))
      }
    } else {
      cli::cli_alert_danger("Publication des données abandonnée.")
      return(invisible(FALSE))
    }
  }

  # At this point we have files in the datamart bucket under datamart/table/unprocessed
  # We can now trigger the glue job to process the files
  # The table will be created in the form of datamart-table
  # The glue job will move the files from unprocessed to processed
  # The glue job will also create the table in the datamart database
  if (ask_yes_no(paste(
    "Voulez-vous traiter les données maintenant pour les rendre disponibles immédiatement?",
    "  Si vous ne le faites pas maintenant, le traitement sers déclenché automatiquement dans les 6 prochaines heures.", 
    "  Votre choix", sep = "\n"))) {
    glue_job <- list_glue_jobs(creds)
    run_glue_job(creds, glue_job, "datamarts", paste0(datamart, "/", table), table_tags, table_description)
    cli::cli_alert_success("Le traitement des données a été déclenché avec succès.")
    cli::cli_alert_info("Les données seront disponibles dans les prochaines minutes\n")
    cli::cli_alert_info("N'oubliez pas de vous déconnecter de la plateforme ellipse avec `ellipse_disconnect(...)` 👋.")
  } else {
    cli::cli_alert_success("Publication des données complétée avec succès")
    cli::cli_alert_info("Les données seront disponibles dans les 6 prochaines heures")
    cli::cli_alert_info("N'oubliez pas de vous déconnecter de la plateforme ellipse avec `ellipse_disconnect(...)` 👋.")
    return(invisible(FALSE))
  }
}

#' Retirer une table d'un datamart ou un datamart complet
#'
#' @param con Un objet de connexion tel qu'obtenu via `tube::ellipse_connect()`.
#' @param datamart Le nom du datamart contenant la table à retirer.
#' @param table Paramètre optionnel : Le nom de la table à retirer.  S'il est manquant, 
#' vide ou null alors c'est le datamart complet qui est retiré
#'
#' @returns TRUE si la table a été retirée avec succès, FALSE sinon.
#' @export
ellipse_unpublish <- function(con, datamart, table) {
  env <- DBI::dbGetInfo(con)$profile_name
  
  if (!check_params_before_unpublish(env, datamart, table)) {
    return(invisible(FALSE))
  }
  
  creds <- get_aws_credentials(env)
  dm_bucket <- list_datamarts_bucket(creds)
  dm_glue_database <- list_datamarts_database(creds)

  
  # check that the datamart exists by checking that the 1st level partition exists in the datamart bucket
  dm_partitions <- list_s3_partitions(creds, dm_bucket)
  dm_list <- lapply(dm_partitions, function(x) gsub("/$", "", x))
  if (!datamart %in% dm_list) {
    cli::cli_alert_danger("Le datamart fourni n'existe pas! 😅")
    return(invisible(FALSE))
  }
  
  # check that the table exists in the datamart in the form of s3://datamarts-bucket/datamart/table
  dm_folders <- list_s3_folders(creds, dm_bucket, paste0(datamart, "/"))
  
  if (!table %in% dm_folders) {
    cli::cli_alert_danger("La table demandée n'existe pas! 😅")
    return(invisible(FALSE))
  }
  
  # confirm by the user
  if (!ask_yes_no("Êtes-vous certain.e de vouloir retirer la table?")) {
    cli::cli_alert_info("Retrait de la table abandonné.")
    return(invisible(FALSE))
  }
  
  cli::cli_alert_info("Retrait de la table en cours...")
  
  # delete the glue table
  r1 <- delete_glue_table(creds, dm_glue_database, paste0(datamart, "-", table))
  
  # delete the content of the folder s3://datamarts-bucket/datamart/table
  r2 <- delete_s3_folder(creds, dm_bucket, paste0(datamart, "/", table))
  r3 <- delete_s3_folder(creds, dm_bucket, paste0(datamart, "/", table, "-output"))
  
  if (r1 && r2 && r3) {
    cli::cli_alert_success("La table a été retirée avec succès.")
    return(invisible(TRUE))
  } else {
    cli::cli_alert_danger("Il y a eu une erreur lors du retrait de la table! 😅")
    cli::cli_alert_danger("Veuillez contacter votre ingénieur de données.")
    return(invisible(FALSE))
  }
}
