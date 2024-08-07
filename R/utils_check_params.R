#' @title Check the env parameter provided to a function
#' @description Check if the env parameter is valid: either DEV or PROD
#' @param env The environment to check
#' @return TRUE if the env parameter is valid, FALSE otherwise
#' 
check_env <- function(env) {
  if (is.null(env) || !env %in% c("DEV", "PROD")) {
    return(FALSE)
  }
  return(TRUE)
}

#' @title Check the database parameter provided to a function
#' @description Check if the database parameter is valid : either datawarehouse or datamarts
#' @param database The database to check
#' @return TRUE if the database parameter is valid, FALSE otherwise
check_database <- function(database) {
  if (is.null(database) || !database %in% c("datawarehouse", "datamarts")) {
    return(FALSE)
  }
  return(TRUE)
}

#' @title Check the parameters provided to the ellipse_ingest function
#' @description Check if the parameters are valid before ingesting the data
#' @param pipeline The pipeline to check
#' @param landing_zone_partitions The list of partitions in the landing zone bucket which
#' corresponds to the pipelines names that have been implemented in the platform
#' @param file_batch The batch value of the file to ingest that is going to 
#' be transformed into a column in the datawarehouse table
#' @param file_version The version of the file to ingest that is going to
#' be transformed into a column in the datawarehouse table
#' @return TRUE if the parameters are valid, FALSE otherwise
check_pipeline_before_ingest <- function(pipeline, landing_zone_partitions, file_batch, file_version) {
  if (is.null(pipeline)) {
    cli::cli_alert_danger("Oups, il faut fournir un pipeline pour injecter les données! 😅")
    return(FALSE)
  }

  # check that the pipeline exists by checking that the partition exists in the landing zone bucket
  if (! paste0(pipeline,"/") %in% landing_zone_partitions) {
    cli::cli_alert_danger("Oups, le pipeline fourni n'existe pas! 😅\
      demandez à votre ingénieur de données de créer le pipeline dans la plateforme de données\
      pour que vous puissiez y injecter des données.")
    return(FALSE)
  }

  # check that pipeline name start with a, r, c, dict or dim
  if (!grepl("^(a-|r-|c-|dict-|dim-)", pipeline)) {
    cli::cli_alert_danger("Oups, le nom du pipeline doit commencer par a-, r-, c-, dict- ou dim-! 😅")
    return(FALSE)
  }

  # check that we have a version for dim, or dict and that we have a batch for a, r, c pipelines
  if (grepl("^(a-|r-|c-)", pipeline) && is.null(file_batch)) {
    cli::cli_alert_danger("Oups, il faut fournir un batch pour les données factuelles (pipelines a-, r- ou c-)! 😅")
    return(FALSE)
  }

  if (grepl("^(dict-|dim-)", pipeline) && is.null(file_version)) {
    cli::cli_alert_danger("Oups, il faut fournir une version pour les données dimensionnelles ou les dictionnaires (pipelines dict- ou dim-)! 😅")
    return(FALSE)
  }

  return(TRUE)
}

#' @title Check the parameters provided to the ellipse_ingest function
#' @description Check if the parameters are valid before ingesting the data
#' @param file_batch The batch value of the file to ingest that is going to
#' be transformed into a column in the datawarehouse table
#' @param file_version The version of the file to ingest that is going to
#' be transformed into a column in the datawarehouse table
#' @return TRUE if the parameters are valid, FALSE otherwise
check_file_versioning_before_ingest <- function(file_batch, file_version) {
  if (is.null(file_batch) && is.null(file_version)) {
    cli::cli_alert_danger("Oups, il faut fournir un batch ou une version pour injecter les données! 😅\
    Si vous ne fournissez pas de batch, vous devez fournir une version.\
    Si vous ne fournissez pas de version, vous devez fournir un batch.\
    On utilise un batch pour les données factuelles, et une version pour les données dimensionnelles ou les dictionnaires.")
    return(FALSE)
  }

  if (!is.null(file_batch) && !is.null(file_version)) {
    cli::cli_alert_danger("Oups, il faut fournir soit un batch, soit une version, mais pas les deux pour injecter les données! 😅\
    On utilise un batch pour les données factuelles, et une version pour les données dimensionnelles ou les dictionnaires.")
    return(FALSE)
  }

  return(TRUE)
}

#' @title Check the parameters provided to the ellipse_publish function
#' @description Check if the parameters are valid before publishing the data
#' @param env The environment to publish the data to
#' @param table The table to publish the data to
#' @param dataframe The dataframe to publish
#' @return TRUE if the parameters are valid, FALSE otherwise
check_params_before_publish <- function(env, dataframe, datamart, table, data_tag, table_tags, table_description) {
  logger::log_debug("[tube::check_params_before_publish] Checking parameters before publishing the data")
  logger::log_debug("[tube::check_params_before_publish] Checking the env parameter")
  if (!check_env(env)) {
    cli::cli_alert_danger(paste("Oups, il faut choisir un environnement! 😅\n\n",
      "Le paramètre `env` peut être \"PROD\" ou \"DEV\"",
      sep = ""))
    return(FALSE)
  }

  logger::log_debug("[tube::check_params_before_publish] Checking the data_tag parameter")
  if (!is.null(data_tag)) {
    if (!is.character(data_tag)) {
      cli::cli_alert_danger("Le data_tag doit être une chaîne de caractères! 😅")
      return(FALSE)
    }
  }

  logger::log_debug("[tube::check_params_before_publish] Checking the table_tags parameter")
  # check that the table_tags parameter is a list that translates into a valid json structure
  if (!is.null(table_tags)) {
    if (!is.list(table_tags)) {
      cli::cli_alert_danger("Les table_tags doivent être une liste! 😅")
      return(FALSE)
    }
    r <- tryCatch({
      jsonlite::toJSON(table_tags)
      return(TRUE)
    }, error = function(e) {
      cli::cli_alert_danger("Les table_tags doivent être une liste valide! 😅")
      return(FALSE)
    })
    return(r)
  }
  
  logger::log_debug("[tube::check_params_before_publish] Checking the table description parameter")
  if (!is.null(table_description)) {
    if (!is.character(table_description)) {
      cli::cli_alert_danger("La description de la table doit être une chaîne de caractères! 😅")
      return(FALSE)
    }
  }

  logger::log_debug("[tube::check_params_before_publish] Checking the table parameter")
  if (is.null(table)) {
    cli::cli_alert_danger("Oups, il faut fournir un nom de table pour publier les données! 😅")
    return(FALSE)
  }

  logger::log_debug("[tube::check_params_before_publish] Checking the dataframe parameter pass 1")
  if (is.null(dataframe) || !is.data.frame(dataframe)) {
    cli::cli_alert_danger("Oups, il faut fournir un dataframe pour publier les données! 😅")
    return(FALSE)
  }

  logger::log_debug("[tube::check_params_before_publish] Checking the dataframe parameter pass 2")
  if (nrow(dataframe) == 0) {
    cli::cli_alert_danger("Oups, le dataframe est vide! 😅")
    return(FALSE)
  }

  logger::log_debug("[tube::check_params_before_publish] Checking the datamart parameter pass 3")
  if (ncol(dataframe) == 0) {
    cli::cli_alert_danger("Oups, le dataframe n'a pas de colonnes! 😅")
    return(FALSE)
  }

  logger::log_debug("[tube::check_params_before_publish] Checking the datamart parameter pass 4")
  if (any(duplicated(names(dataframe)))) {
    cli::cli_alert_danger("Oups, le dataframe a des colonnes en double! 😅")
    return(FALSE)
  }

  logger::log_debug("[tube::check_params_before_publish] Exitting function")
  return(TRUE)
}

#' @title Check the parameters provided to the ellipse_unpublish function
#' @description Check if the parameters are valid before unpublishing the data
#' @param env The environment to unpublish the data from
#' @param datamart The datamart to unpublish the data from
#' @param table The table to unpublish the data from
#' @return TRUE if the parameters are valid, FALSE otherwise
check_params_before_unpublish <- function(env, datamart, table) {
  logger::log_debug("[tube::check_params_before_unpublish] Checking parameters before unpublishing the data")
  logger::log_debug("[tube::check_params_before_unpublish] Checking the env parameter")
  if (!check_env(env)) {
    cli::cli_alert_danger(paste("Oups, il faut choisir un environnement! 😅\n\n",
      "Le paramètre `env` peut être \"PROD\" ou \"DEV\"",
      sep = ""))
    return(FALSE)
  }

  logger::log_debug("[tube::check_params_before_unpublish] Checking the datamart parameter")
  if (is.null(datamart)) {
    cli::cli_alert_danger("Oups, il faut fournir un nom de datamart pour désactiver la publication des données! 😅")
    return(FALSE)
  }

  logger::log_debug("[tube::check_params_before_unpublish] Checking the table parameter")
  if (is.null(table)) {
    cli::cli_alert_danger("Oups, il faut fournir un nom de table pour désactiver la publication des données! 😅")
    return(FALSE)
  }

  logger::log_debug("[tube::check_params_before_unpublish] Exitting function")
  return(TRUE)
}

#' @title Check the parameters provided to the ellipse_describe function
#' @description Check if the parameters are valid before executing the function
#' @param env The environment to describe the data from
#' @param schema The datamart to describe the data from
#' @param table The table to describe the data from 
#' @param new_table_tags The new table tags to apply to the table
#' @param new_table_description The new table description to apply to the table
#' @return TRUE if the parameters are valid, FALSE otherwise
check_params_before_describe <- function(env, schema, table, new_table_tags, new_table_description) {
  logger::log_debug("[tube::check_params_before_describe] Checking parameters before applying changes to a glue table properties")
  logger::log_debug("[tube::check_params_before_describe] Checking the env parameter")

  # Check env
  if (!check_env(env)) {
    cli::cli_alert_danger(paste("Oups, il faut choisir un environnement! 😅\n\n",
      "Le paramètre `env` peut être \"PROD\" ou \"DEV\"",
      sep = ""))
    return(FALSE)
  }

  # Check schema
  logger::log_debug("[tube::check_params_before_describe] Checking the schema parameter")
  if (is.null(schema)) {
    cli::cli_alert_danger("Oups, il faut fournir un nom de datamart pour décrire les données! 😅")
    return(FALSE)
  }

  # Check table
  logger::log_debug("[tube::check_params_before_describe] Checking the table parameter")
  if (is.null(table)) {
    cli::cli_alert_danger("Oups, il faut fournir un nom de table pour décrire les données! 😅")
    return(FALSE)
  }

  # Check tags 
  logger::log_debug("[tube::check_params_before_describe] Checking the new_table_tags parameter")
  # check that the table_tags parameter is a list that translates into a valid json structure
  if (!is.null(new_table_tags)) {
    if (!is.list(new_table_tags)) {
      cli::cli_alert_danger("Les table_tags doivent être une liste! 😅")
      return(FALSE)
    }
    r <- tryCatch({
      jsonlite::toJSON(new_table_tags)
    }, error = function(e) {
      cli::cli_alert_danger("Les tags doivent être une liste valide! 😅")
      cli::cli_alert_danger(e$message)
      return(FALSE)
    })
  }

  # Check description
  logger::log_debug("[tube::check_params_before_describe] Checking the new_table_description parameter")
  if (!is.null(new_table_description)) {
    if (!is.character(new_table_description)) {
      cli::cli_alert_danger("La description de la table doit être une chaîne de caractères! 😅")
      return(FALSE)
    }
  }

  creds <- get_aws_credentials(env)

  # Check that schema exists
  logger::log_debug("[tube::check_params_before_describe] checking that the glue schema exists")
  schemas_list <- list_glue_databases(creds, "datamart|datawarehouse") |> unique()
  if (! schema %in% schemas_list) {
    cli::cli_alert_danger("Oups, le datamart n'existe pas! 😅")
    cli::cli_alert_danger(paste("Les datamarts disponibles sont:", sep = ""))
    print(as.list(schemas_list))
    return(FALSE)
  }

  #Check that table exists
  logger::log_debug("[tube::check_params_before_describe] checking that the glue table exists")
  tables_details <- list_glue_tables(creds, schema, table)
  if (! table %in% tables_details$table_name) {
    cli::cli_alert_danger("Oups, la table n'existe pas! 😅")
    tables_list <- list_glue_tables(creds, schema) |> dplyr::select(table_name) |> unique()
    cli::cli_alert_danger(paste("Les tables disponibles dans ", schema, " sont:", sep = ""))
    print(as.list(tables_list))
    return(FALSE)
  }

  logger::log_debug("[tube::check_params_before_describe] Exiting function")
  return(TRUE)
}


#' @title Check the parameters provided to the ellipse_process function
#' @description Check if the parameters are valid before executing the function
#' @param con The connection to the database
#' @param schema The datamart to refresh the table from
#' @param table The table to run the glue job against
#' @return TRUE if the parameters are valid, FALSE otherwise
check_params_before_refresh <- function(con, schema, table) {
  logger::log_debug("[tube::check_params_before_describe] Checking parameters before refreshing the table")
  logger::log_debug("[tube::check_params_before_describe] Checking the con parameter")
  if (is.null(con)) {
    cli::cli_alert_danger("Oups, il faut fournir une connexion à la base de données pour rafraîchir la table! 😅")
    return(FALSE)
  }

  logger::log_debug("[tube::check_params_before_describe] Checking the schema parameter")
  if (is.null(schema)) {
    cli::cli_alert_danger("Oups, il faut fournir un nom de datamart pour rafraîchir la table! 😅")
    return(FALSE)
  }

  logger::log_debug("[tube::check_params_before_describe] Checking the table parameter")
  if (is.null(table)) {
    cli::cli_alert_danger("Oups, il faut fournir un nom de table pour rafraîchir la table! 😅")
    return(FALSE)
  }

  # check that the table exists
  logger::log_debug("[tube::check_params_before_describe] Checking that the table exists")
  if (!DBI::dbExistsTable(con, table)) {
    cli::cli_alert_danger("Oups, la table n'existe pas! 😅")
    return(FALSE)
  }

  logger::log_debug("[tube::check_params_before_describe] Exiting function")
  return(TRUE)
}
