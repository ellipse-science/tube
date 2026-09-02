#' Athena DBI driver/connection/result classes
#'
#' `tube` implémente son propre pilote DBI pour AWS Athena, basé directement
#' sur `paws.analytics::athena()`. Ce pilote remplace le package `noctua`
#' (retiré de CRAN), sans ajouter de nouvelle dépendance puisque
#' `paws.analytics` est déjà utilisé ailleurs dans le package (voir R/glue.R).
#'
#' Les classes héritent des classes S4 de `DBI` (`DBIDriver`, `DBIConnection`,
#' `DBIResult`), ce qui permet à `dplyr`/`dbplyr` de générer et exécuter des
#' requêtes paresseuses (`dplyr::tbl()`) sans traduction SQL personnalisée:
#' `dbplyr` retombe automatiquement sur ses méthodes génériques pour
#' `DBIConnection`, qui conviennent au dialecte SQL d'Athena (Presto/Trino).
#'
#' @importClassesFrom DBI DBIDriver DBIConnection DBIResult
#' @importMethodsFrom DBI dbConnect dbDisconnect dbIsValid dbGetInfo dbSendQuery dbFetch dbHasCompleted dbClearResult dbColumnInfo dbListTables dbExistsTable
#' @importFrom methods new setClass setMethod
#' @name athena_dbi
NULL

setClass("EllipseAthenaDriver", contains = "DBIDriver")

setClass("EllipseAthenaConnection",
  contains = "DBIConnection",
  slots = c(ptr = "environment")
)

setClass("EllipseAthenaResult",
  contains = "DBIResult",
  slots = c(ptr = "environment")
)

#' Construire le pilote DBI Athena interne de tube
#' @return Un objet `EllipseAthenaDriver`
#' @keywords internal
athena_ellipse_driver <- function() {
  methods::new("EllipseAthenaDriver")
}

#' @rdname athena_dbi
setMethod("dbConnect", "EllipseAthenaDriver", function(drv,
    aws_access_key_id, aws_secret_access_key, profile_name,
    schema_name, s3_staging_dir, work_group = NULL, ...) {
  creds <- list(credentials = list(creds = list(
    access_key_id = aws_access_key_id,
    secret_access_key = aws_secret_access_key
  )))

  client <- paws.analytics::athena(config = creds)

  ptr <- new.env(parent = emptyenv())
  ptr$client <- client
  ptr$schema_name <- schema_name
  ptr$profile_name <- profile_name
  ptr$work_group <- work_group
  ptr$s3_staging_dir <- s3_staging_dir
  ptr$valid <- TRUE

  methods::new("EllipseAthenaConnection", ptr = ptr)
})

#' @rdname athena_dbi
setMethod("dbDisconnect", "EllipseAthenaConnection", function(conn, ...) {
  conn@ptr$valid <- FALSE
  invisible(TRUE)
})

#' @rdname athena_dbi
setMethod("dbIsValid", "EllipseAthenaConnection", function(dbObj, ...) {
  isTRUE(dbObj@ptr$valid)
})

#' @rdname athena_dbi
setMethod("dbGetInfo", "EllipseAthenaConnection", function(dbObj, ...) {
  list(
    dbname = dbObj@ptr$schema_name,
    dbms.name = dbObj@ptr$schema_name,
    profile_name = dbObj@ptr$profile_name,
    work_group = dbObj@ptr$work_group,
    s3_staging_dir = dbObj@ptr$s3_staging_dir,
    db.version = NA_character_
  )
})

#' Utiliser l'interface moderne (edition 2) de dbplyr pour dplyr::tbl()
#' @param con Un objet `EllipseAthenaConnection`
#' @return `2L`
#' @exportS3Method dbplyr::dbplyr_edition
dbplyr_edition.EllipseAthenaConnection <- function(con) {
  2L
}

#' Exécuter une requête Athena et attendre sa complétion
#' @param conn Un objet `EllipseAthenaConnection`
#' @param statement La requête SQL à exécuter
#' @param poll_interval_sec Délai (en secondes) entre deux vérifications d'état
#' @param timeout_sec Délai maximal (en secondes) à attendre avant d'abandonner
#' @return L'identifiant de l'exécution de requête (`QueryExecutionId`)
#' @keywords internal
athena_execute_query <- function(conn, statement, poll_interval_sec = 1, timeout_sec = 300) {
  client <- conn@ptr$client

  exec_args <- list(
    QueryString = statement,
    QueryExecutionContext = list(Database = conn@ptr$schema_name),
    ResultConfiguration = list(OutputLocation = conn@ptr$s3_staging_dir)
  )
  if (!is.null(conn@ptr$work_group)) {
    exec_args$WorkGroup <- conn@ptr$work_group
  }

  exec <- do.call(client$start_query_execution, exec_args)
  query_execution_id <- exec$QueryExecutionId

  start_time <- Sys.time()
  state <- "QUEUED"
  status <- NULL
  repeat {
    status <- client$get_query_execution(QueryExecutionId = query_execution_id)
    state <- status$QueryExecution$Status$State

    if (state %in% c("SUCCEEDED", "FAILED", "CANCELLED")) {
      break
    }

    if (as.numeric(difftime(Sys.time(), start_time, units = "secs")) > timeout_sec) {
      stop(paste0(
        "La requête Athena a dépassé le délai d'attente de ", timeout_sec, " secondes."
      ), call. = FALSE)
    }

    Sys.sleep(poll_interval_sec)
  }

  if (state != "SUCCEEDED") {
    reason <- status$QueryExecution$Status$StateChangeReason
    stop(paste0(
      "La requête Athena a échoué (", state, "): ", reason
    ), call. = FALSE)
  }

  query_execution_id
}

#' Convertir les lignes brutes retournées par Athena en un vecteur R typé
#' @param x Un vecteur de chaînes de caractères
#' @param type Le type de colonne Athena (ex.: "varchar", "bigint", "double")
#' @return Un vecteur R du type approprié
#' @keywords internal
athena_coerce_column <- function(x, type) {
  switch(type,
    "tinyint" = ,
    "smallint" = ,
    "integer" = as.integer(x),
    "bigint" = ,
    "double" = ,
    "float" = ,
    "real" = ,
    "decimal" = as.numeric(x),
    "boolean" = as.logical(x),
    "date" = as.Date(x),
    "timestamp" = as.POSIXct(x, tz = "UTC"),
    x
  )
}

#' Transformer les lignes retournées par `GetQueryResults` en tibble typé
#' @param rows Une liste de lignes Athena (chacune une liste `Data`)
#' @param column_info La métadonnée de colonnes retournée par Athena
#'   (`ResultSet$ResultSetMetadata$ColumnInfo`)
#' @return Un `tibble`
#' @keywords internal
athena_rows_to_tibble <- function(rows, column_info) {
  col_names <- vapply(column_info, function(ci) ci$Name, character(1))
  col_types <- vapply(column_info, function(ci) ci$Type, character(1))

  n <- length(rows)
  m <- length(col_names)

  raw <- matrix(NA_character_, nrow = n, ncol = m)
  for (i in seq_len(n)) {
    data <- rows[[i]]$Data
    for (j in seq_len(m)) {
      val <- data[[j]]$VarCharValue
      raw[i, j] <- if (length(val) == 0) NA_character_ else val
    }
  }

  result <- tibble::as_tibble(as.data.frame(raw, stringsAsFactors = FALSE), .name_repair = "minimal")
  colnames(result) <- col_names

  for (j in seq_len(m)) {
    result[[j]] <- athena_coerce_column(result[[j]], col_types[j])
  }

  result
}

#' Récupérer, en paginant, tous les résultats d'une requête Athena complétée
#' @inheritParams athena_execute_query
#' @param query_execution_id L'identifiant retourné par `athena_execute_query()`
#' @return Un `tibble` contenant les résultats de la requête
#' @keywords internal
athena_fetch_results <- function(conn, query_execution_id, statement) {
  client <- conn@ptr$client

  rows <- list()
  column_info <- NULL
  next_token <- NULL
  first_page <- TRUE
  is_select <- grepl("^\\s*(SELECT|WITH|SHOW|DESCRIBE)", statement, ignore.case = TRUE)

  repeat {
    page_args <- list(QueryExecutionId = query_execution_id, MaxResults = 1000)
    if (!is.null(next_token)) {
      page_args$NextToken <- next_token
    }
    page <- do.call(client$get_query_results, page_args)

    if (is.null(column_info)) {
      column_info <- page$ResultSet$ResultSetMetadata$ColumnInfo
    }

    page_rows <- page$ResultSet$Rows
    if (first_page && is_select && length(page_rows) > 0) {
      page_rows <- page_rows[-1]
    }
    rows <- c(rows, page_rows)

    next_token <- page$NextToken
    first_page <- FALSE
    has_more_pages <- !is.null(next_token) && length(next_token) == 1 &&
      !is.na(next_token) && nzchar(next_token)
    if (!has_more_pages) {
      break
    }
  }

  athena_rows_to_tibble(rows, column_info)
}

#' @rdname athena_dbi
setMethod("dbSendQuery", "EllipseAthenaConnection", function(conn, statement, ...) {
  query_execution_id <- athena_execute_query(conn, statement)
  data <- athena_fetch_results(conn, query_execution_id, statement)

  ptr <- new.env(parent = emptyenv())
  ptr$data <- data
  ptr$fetched <- FALSE
  ptr$statement <- statement

  methods::new("EllipseAthenaResult", ptr = ptr)
})

#' @rdname athena_dbi
setMethod("dbFetch", "EllipseAthenaResult", function(res, n = -1, ...) {
  res@ptr$fetched <- TRUE
  res@ptr$data
})

#' @rdname athena_dbi
setMethod("dbHasCompleted", "EllipseAthenaResult", function(res, ...) {
  isTRUE(res@ptr$fetched)
})

#' @rdname athena_dbi
setMethod("dbClearResult", "EllipseAthenaResult", function(res, ...) {
  invisible(TRUE)
})

#' @rdname athena_dbi
setMethod("dbColumnInfo", "EllipseAthenaResult", function(res, ...) {
  data.frame(
    name = colnames(res@ptr$data),
    type = vapply(res@ptr$data, function(col) class(col)[1], character(1)),
    stringsAsFactors = FALSE
  )
})

#' @rdname athena_dbi
setMethod("dbListTables", "EllipseAthenaConnection", function(conn, ...) {
  r <- DBI::dbGetQuery(conn, paste0("SHOW TABLES IN ", conn@ptr$schema_name))
  if (ncol(r) == 0) {
    return(character(0))
  }
  if ("tab_name" %in% colnames(r)) r$tab_name else r[[1]]
})

#' @rdname athena_dbi
setMethod("dbExistsTable", c("EllipseAthenaConnection", "character"), function(conn, name, ...) {
  name %in% DBI::dbListTables(conn)
})
