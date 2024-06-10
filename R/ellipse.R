memoized_aws_session <- memoise::memoise(aws_session)

#' Se connecter à la plateforme de données ellipse sur AWS
#'
#' Cette fonction utilise les clés d'accès AWS configurées dans le fichier
#' `.Renviron` pour se connecter à la plateforme de données.
#'
#' @param env The environment to connect to on ellipse-science. Defaults to "PROD"
#' @param database The Glue/Athena database to connect to. Default to "datawarehouse"
#'
#' @returns Un object de connexion `DBI`.
#' @export
ellipse_connect <- function(
  env = c("PROD", "DEV"),
  database  = "datawarehouse"
) {
  env <- match.arg(env)
  cli::cli_alert_info(paste("Environnement:", env))

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

  # https://github.com/ellipse-science/tube/issues/16
  Sys.setenv("AWS_ACCESS_KEY_ID" = aws_access_key_id)
  Sys.setenv("AWS_SECRET_ACCESS_KEY" = aws_secret_access_key)

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
  database <- match.arg(database)
  cli::cli_alert_info(paste("Database:", database))

  session <- memoized_aws_session(id = aws_access_key_id,
                                  key = aws_secret_access_key)
  schema_name <- switch(database,
                        "datawarehouse" = paste0(session$datawarehouse_database),
                        database)

  cli::cli_alert_info("Pour déconnecter: DBI::dbDisconnect(objet_de_connexion)")
  DBI::dbConnect(noctua::athena(),
                 aws_access_key_id = aws_access_key_id,
                 aws_secret_access_key = aws_secret_access_key,
                 schema_name = schema_name,
                 work_group = "ellipse-work-group",
                 s3_staging_dir = paste0("s3://",
                                         session$athena_staging_bucket))

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
    session <- memoized_aws_session()
    df <-
      list_datawarehouse_tables(session) %>%
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
