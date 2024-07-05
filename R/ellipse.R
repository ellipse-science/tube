memoized_get_aws_credentials <- memoise::memoise(get_aws_credentials)

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

  if (is.null(env) || !env %in% c("DEV", "PROD", "dev", "prod")) {
    cli::cli_alert_danger(paste("Oups, il faut choisir un environnement! 😅\n\n",
                                "Le paramètre `env` peut être \"PROD\" ou \"DEV\"",
                                sep = ""))
    return(NULL)
  }
  cli::cli_alert_info(paste("Environnement:", env))

  env <- toupper(env)

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
    return(NULL)
  }

  # https://github.com/ellipse-science/tube/issues/16
  Sys.setenv("AWS_ACCESS_KEY_ID" = aws_access_key_id)
  Sys.setenv("AWS_SECRET_ACCESS_KEY" = aws_secret_access_key)

  database <- match.arg(database)
  cli::cli_alert_info(paste("Database:", database))

  creds <- get_aws_credentials()

  aws_access_key_id <- creds$credentials$creds$access_key_id
  aws_secret_access_key <- creds$credentials$creds$secret_access_key

  datawarehouse_database <- list_datawarehouse_database(creds)
  athena_staging_bucket <- list_athena_staging_bucket(creds)

  schema_name <- switch(database,
                        "datawarehouse" = paste0(datawarehouse_database),
                        database)

  cli::cli_alert_info("Pour déconnecter: tube::ellipse_disconnect(objet_de_connexion)")
  DBI::dbConnect(noctua::athena(),
                 aws_access_key_id = aws_access_key_id,
                 aws_secret_access_key = aws_secret_access_key,
                 schema_name = schema_name,
                 work_group = "ellipse-work-group",
                 s3_staging_dir = paste0("s3://",athena_staging_bucket))
}

#' Se déconnecter de la plateforme de données ellipse
#' @returns TRUE if the connexion was closed or FALSE if no connexion existed
#' @export
ellipse_disconnect <- function(con = NULL) {
  if (is.null(con)) {
    cli::cli_alert_danger("Oups! Il faut fournit un objet de connection! 😅")
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
  tables <- DBI::dbListTables(con)
  if (!is.null(table)) {
    if (!table %in% tables) {
      cli::cli_alert_danger("La table demandée est inconnue.")
      return(NULL)
    }
    creds <- memoized_get_aws_credentials()
    df <-
      list_datawarehouse_tables(creds) %>%
      dplyr::filter(table_name == table)
    return(df)
  }
  tibble::tibble(table = tables) %>%
    dplyr::mutate(categorie =
                    dplyr::case_when(startsWith(table, "a-")    ~ "Agora+",
                                     startsWith(table, "c-")    ~ "Civimètre+",
                                     startsWith(table, "r-")    ~ "Radar+",
                                     startsWith(table, "dict-") ~ "Dictionnaire", # nolint
                                     startsWith(table, "dim-")  ~ "Dimension",
                                     .default = "Autre")) %>%
    dplyr::select(categorie, table)
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
  tables <- DBI::dbListTables(con)
  if (!table %in% tables) {
    cli::cli_alert_danger("La table demandée est inconnue.")
    return(NULL)
  }
  dplyr::tbl(con, table)
}


#' Injecter de nouvelles données brutes manuellement dans tube via la landing zone
#'
#' @param env L'environnement dans lequel les données doivent être injectées
#' @param folder Le chemin vers le répertoire qui contient les fichiers à charger dans tube
#' @param pipeline Le nom du pipeline qui doit être exécuté pour charger les données
#' @param batch Le nom du batch qui doit être accollé aux données dans l'entrepôt de données
#'
#' @returns La liste des fichiers qui ont été injectés dans tube
ellipse_ingest <- function(env, folder, pipeline, batch) {
  creds <- memoized_get_aws_credentials()

  cli::cli_alert_danger("Cette fonction n'est pas encore implémentée! Revenez plus tard😅")
}


#' Publier un dataframe dans un datamart
#'
#' @param env L'environnement dans lequel les données doivent être injectées
#' @param dataframe Le chemin vers le répertoire qui contient les fichiers à charger dans tube
#' @param datamart Le nom du pipeline qui doit être exécuté pour charger les données
#' @param table Le nom de la table qui doit être créée dans le datamart
#'
#' @returns TRUE si le dataframe a été envoyé dans le datamart  FALSE sinon.
ellipse_publish <- function(env, dataframe, datamart, table) {
  creds <- memoized_get_aws_credentials()

  cli::cli_alert_danger("Cette fonction n'est pas encore implémentée! Revenez plus tard😅")

}
