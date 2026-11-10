# Dialecte SQL Athena (Presto/Trino) pour le pilote DBI maison (voir R/athena_dbi.R).
#
# Le pilote utilisait auparavant le dialecte ANSI par defaut de dbplyr, ce qui
# envoyait du SQL invalide sur Athena (ex.: Sys.Date() rendu tel quel, INSTR
# inexistant, floor_date sans traduction). Ce fichier porte le dialecte Athena
# de noctua (archive DyfanJones/noctua : sql_translate_env.R, dplyr_integration.R),
# adapte a dbplyr 2.6.0 qui dispatche sur sql_dialect(con).
#
# Note : l'echappement des litteraux reste fonction de la classe R du littéral
# (une string reste un varchar). Comparer une colonne date/timestamp a une
# string echoue toujours sur Athena (TYPE_MISMATCH) : il faut passer as.Date() /
# as.POSIXct() / un nombre cote appelant.

#' Dialecte SQL Athena
#' @keywords internal
athena_dialect <- function() {
  dbplyr::new_sql_dialect(
    "athena",
    quote_identifier = function(x) dbplyr::sql_quote(x, '"')
  )
}

#' @exportS3Method dbplyr::sql_dialect
sql_dialect.EllipseAthenaConnection <- function(con) {
  athena_dialect()
}

# --- Echappement des litteraux (dbplyr 2.6 dispatche sur la classe du dialecte) ---

#' @exportS3Method dbplyr::sql_escape_date
sql_escape_date.sql_dialect_athena <- function(con, x) {
  dbplyr::sql(paste0("DATE '", as.character(x), "'"))
}

#' @exportS3Method dbplyr::sql_escape_datetime
sql_escape_datetime.sql_dialect_athena <- function(con, x) {
  dbplyr::sql(paste0("TIMESTAMP '", format(x, "%Y-%m-%d %H:%M:%S"), "'"))
}

#' @exportS3Method dbplyr::sql_escape_string
sql_escape_string.sql_dialect_athena <- function(con, x) {
  dbplyr::sql_quote(x, "'")
}

# --- Traductions de fonctions ---

#' @exportS3Method dbplyr::sql_translation
sql_translation.sql_dialect_athena <- function(con) {
  sql_quantile <- getFromNamespace("sql_quantile", "dbplyr")
  sql_median <- getFromNamespace("sql_median", "dbplyr")

  dbplyr::sql_variant(
    dbplyr::sql_translator(
      .parent = dbplyr::base_scalar,
      # casts explicites
      as.character = dbplyr::sql_cast("VARCHAR"),
      as.numeric = dbplyr::sql_cast("DOUBLE"),
      as.double = dbplyr::sql_cast("DOUBLE"),
      as.integer = dbplyr::sql_cast("INTEGER"),
      as.integer64 = dbplyr::sql_cast("BIGINT"),
      as.Date = dbplyr::sql_cast("DATE"),
      as.logical = dbplyr::sql_cast("BOOLEAN"),
      as.raw = dbplyr::sql_cast("VARBINARY"),
      # dates/heures courantes
      Sys.Date = function() dbplyr::build_sql("current_date"),
      Sys.time = function(tz = NULL) {
        if (is.null(tz)) dbplyr::build_sql("now()") else dbplyr::build_sql("now() at time zone ", tz)
      },
      today = function() dbplyr::build_sql("current_date"),
      now = function(tz = NULL) {
        if (is.null(tz)) dbplyr::build_sql("now()") else dbplyr::build_sql("now() at time zone ", tz)
      },
      as_date = dbplyr::sql_cast("DATE"),
      as_datetime = function(x, tz = NULL) {
        x <- as.POSIXct(x)
        if (is.null(tz)) dbplyr::build_sql(x) else dbplyr::build_sql(x, " at time zone ", tz)
      },
      # durees -> INTERVAL
      seconds = function(x) dbplyr::build_sql("INTERVAL ", as.character(as.integer(x)), " second"),
      minutes = function(x) dbplyr::build_sql("INTERVAL ", as.character(as.integer(x)), " minute"),
      hours = function(x) dbplyr::build_sql("INTERVAL ", as.character(as.integer(x)), " hour"),
      days = function(x) dbplyr::build_sql("INTERVAL ", as.character(as.integer(x)), " day"),
      weeks = function(x) dbplyr::build_sql("INTERVAL ", as.character(as.integer(x)), " week"),
      months = function(x) dbplyr::build_sql("INTERVAL ", as.character(as.integer(x)), " month"),
      years = function(x) dbplyr::build_sql("INTERVAL ", as.character(as.integer(x)), " year"),
      # lubridate
      floor_date = function(x, unit = "second") {
        unit <- match.arg(unit, c("second", "minute", "hour", "day", "week", "month", "quarter", "year"))
        dbplyr::build_sql("date_trunc(", unit, ", ", x, ")")
      },
      month = function(x, label = FALSE, abbr = TRUE) {
        if (!label) dbplyr::build_sql("date_format(", x, ", '%m')")
        else if (abbr) dbplyr::build_sql("date_format(", x, ", '%b')")
        else dbplyr::build_sql("date_format(", x, ", '%M')")
      },
      quarter = function(x, with_year = FALSE, fiscal_start = 1) {
        if (fiscal_start != 1) stop("`fiscal_start` non supporte sur Athena.", call. = FALSE)
        if (with_year) dbplyr::build_sql("year(", x, ")", " || '.' || ", "quarter(", x, ")")
        else dbplyr::build_sql("quarter(", x, ")")
      },
      wday = function(x, label = FALSE, abbr = TRUE, week_start = NULL) {
        if (!label) {
          week_start <- week_start %||% getOption("lubridate.week.start", 7)
          offset <- as.integer(7 - week_start)
          dbplyr::build_sql("(dow(", x, ") + ", offset, ") %7 + 1")
        } else if (label && !abbr) {
          dbplyr::build_sql("format_datetime(", x, ", 'EEEE')")
        } else if (label && abbr) {
          dbplyr::build_sql("format_datetime(", x, ", 'E')")
        } else {
          stop("Arguments non reconnus pour `wday`", call. = FALSE)
        }
      },
      yday = function(x) dbplyr::build_sql("doy(", x, ")"),
      # regex / stringr (fonctions Presto/Athena)
      grepl = function(pattern, x, ...) dbplyr::build_sql("REGEXP_LIKE(", x, ", ", pattern, ")"),
      regexpr = function(pattern, x, ...) dbplyr::build_sql("REGEXP_LIKE(", x, ", ", pattern, ")"),
      str_detect = function(string, pattern, negate = FALSE) {
        if (isTRUE(negate)) dbplyr::build_sql("NOT REGEXP_LIKE(", string, ", ", pattern, ")")
        else dbplyr::build_sql("REGEXP_LIKE(", string, ", ", pattern, ")")
      },
      str_locate = function(string, pattern) dbplyr::build_sql("strpos(", string, ", ", pattern, ")"),
      str_replace = function(string, pattern, replacement) {
        dbplyr::build_sql("regexp_replace(", string, ", ", pattern, ", ", replacement, ")")
      },
      str_replace_all = function(string, pattern, replacement) {
        dbplyr::build_sql("regexp_replace(", string, ", ", pattern, ", ", replacement, ")")
      },
      str_remove = function(string, pattern) {
        dbplyr::build_sql("regexp_replace(", string, ", ", pattern, ", '')")
      },
      str_remove_all = function(string, pattern) {
        dbplyr::build_sql("regexp_replace(", string, ", ", pattern, ", '')")
      },
      str_squish = function(string) {
        dbplyr::build_sql("trim(regexp_replace(", string, ", '\\\\s+', ' '))")
      },
      str_split = function(string, pattern, n = Inf, simplify = FALSE) {
        if (simplify) stop("`simplify` non supporte sur Athena.", call. = FALSE)
        dbplyr::build_sql("split(", string, ", ", pattern, ")")
      },
      str_extract = function(string, pattern) {
        dbplyr::build_sql("regexp_extract(", string, ", ", pattern, ", 0)")
      },
      str_extract_all = function(string, pattern, simplify = FALSE) {
        if (simplify) stop("`simplify` non supporte sur Athena.", call. = FALSE)
        dbplyr::build_sql("regexp_extract_all(", string, ", ", pattern, ")")
      }
    ),
    # agregats
    dbplyr::sql_translator(
      .parent = dbplyr::base_agg,
      cor = dbplyr::sql_aggregate_2("CORR"),
      cov = dbplyr::sql_aggregate_2("COVAR_SAMP"),
      sd = dbplyr::sql_aggregate("STDDEV_SAMP", "sd"),
      var = dbplyr::sql_aggregate("VAR_SAMP", "var"),
      all = dbplyr::sql_aggregate("BOOL_AND", "all"),
      any = dbplyr::sql_aggregate("BOOL_OR", "any"),
      quantile = sql_quantile("approx_percentile"),
      median = sql_median("approx_percentile")
    ),
    # fenetres
    dbplyr::sql_translator(
      .parent = dbplyr::base_win,
      cor = dbplyr::win_aggregate_2("CORR"),
      cov = dbplyr::win_aggregate_2("COVAR_SAMP"),
      sd = dbplyr::win_aggregate("STDDEV_SAMP"),
      var = dbplyr::win_aggregate("VAR_SAMP"),
      all = dbplyr::win_aggregate("BOOL_AND"),
      any = dbplyr::win_aggregate("BOOL_OR"),
      quantile = sql_quantile("approx_percentile", window = TRUE),
      median = sql_median("approx_percentile", window = TRUE)
    )
  )
}
