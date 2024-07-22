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
check_params_before_publish <- function(env, dataframe, datamart, table, tag) {
  logger::debug("[tube::check_params_before_publish] Checking parameters before publishing the data")
  logger::debug("[tube::check_params_before_publish] Checking the env parameter")
  if (!check_env(env)) {
    cli::cli_alert_danger(paste("Oups, il faut choisir un environnement! 😅\n\n",
      "Le paramètre `env` peut être \"PROD\" ou \"DEV\"",
      sep = ""))
    return(FALSE)
  }

  logger::debug("[tube::check_params_before_publish] Checking the tag parameter")
  if (!is.null(tag)) {
    if (!is.character(tag)) {
      cli::cli_alert_danger("Le tag doit être une chaîne de caractères! 😅")
      return(FALSE)
    }
  }

  logger::debug("[tube::check_params_before_publish] Checking the table parameter")
  if (is.null(table)) {
    cli::cli_alert_danger("Oups, il faut fournir un nom de table pour publier les données! 😅")
    return(FALSE)
  }

  logger::debug("[tube::check_params_before_publish] Checking the dataframe parameter pass 1")
  if (is.null(dataframe) || !is.data.frame(dataframe)) {
    cli::cli_alert_danger("Oups, il faut fournir un dataframe pour publier les données! 😅")
    return(FALSE)
  }

  logger::debug("[tube::check_params_before_publish] Checking the dataframe parameter pass 2")
  if (nrow(dataframe) == 0) {
    cli::cli_alert_danger("Oups, le dataframe est vide! 😅")
    return(FALSE)
  }

  logger::debug("[tube::check_params_before_publish] Checking the datamart parameter pass 3")
  if (ncol(dataframe) == 0) {
    cli::cli_alert_danger("Oups, le dataframe n'a pas de colonnes! 😅")
    return(FALSE)
  }

  logger::debug("[tube::check_params_before_publish] Checking the datamart parameter pass 4")
  if (any(duplicated(names(dataframe)))) {
    cli::cli_alert_danger("Oups, le dataframe a des colonnes en double! 😅")
    return(FALSE)
  }

  logger::debug("[tube::check_params_before_publish] Exitting function")
}
