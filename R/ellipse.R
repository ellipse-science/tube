memoized_aws_session <- memoise::memoise(aws_session)

#' Se connecter à la plateforme de données ellipse sur AWS
#'
#' Cette fonction utilise les clés d'accès AWS configurées dans le fichier
#' `.Renviron` pour se connecter à la plateforme de données.
#'
#' @returns Un object de connexion `DBI`.
#' @export
ellipse_connect <- function() {
  aws_access_key_id     <- Sys.getenv("AWS_ACCESS_KEY_ID")
  aws_secret_access_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
  if (aws_access_key_id == "" || aws_secret_access_key == "") {
    usage <-
      paste("On a besoin de vos clés d'accès sur AWS pour se connecter!\n\n",
            "Dans le fichier ~/.Renviron, ajoutez les lignes:\n\n",
            "AWS_ACCESS_KEY_ID=<votre access key id>\n",
            "AWS_SECRET_ACCESS_KEY=<votre secret access key>\n\n",
            "Puis, redémarrez la session R.")
    cli::cli_alert_danger(usage)
    return(NULL)
  }
  session <- memoized_aws_session()
  cli::cli_alert_info("Pour déconnecter: DBI::dbDisconnect(objet_de_connexion)")
  DBI::dbConnect(noctua::athena(),
                 aws_access_key_id = aws_access_key_id,
                 aws_secret_access_key = aws_secret_access_key,
                 schema_name = session$datawarehouse_database,
                 s3_staging_dir = paste0("s3://",
                                         session$athena_staging_bucket))

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
                    dplyr::case_when(startsWith(table, "a-") ~ "Agora+",
                                     startsWith(table, "c-") ~ "Civimètre+",
                                     startsWith(table, "r-") ~ "Radar+",
                                     startsWith(table, "dict-") ~ "Dictionnaire", # nolint
                                     startsWith(table, "dim-") ~ "Dimension",
                                     .default = "De quessé?")) %>% # nolint
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
ellipse_read <- function(con, table) {
  tables <- DBI::dbListTables(con)
  if (!table %in% tables) {
    cli::cli_alert_danger("La table demandée est inconnue.")
    return(NULL)
  }
  dplyr::tbl(con, table)
}