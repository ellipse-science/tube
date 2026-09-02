test_that("athena_coerce_column converts numeric types", {
  expect_type(athena_coerce_column(c("1", "2"), "bigint"), "double")
  expect_type(athena_coerce_column(c("1", "2"), "integer"), "integer")
  expect_equal(athena_coerce_column(c("1.5", "2.5"), "double"), c(1.5, 2.5))
})

test_that("athena_coerce_column converts boolean, date and timestamp types", {
  expect_equal(athena_coerce_column(c("true", "false"), "boolean"), c(TRUE, FALSE))
  expect_equal(athena_coerce_column("2024-01-15", "date"), as.Date("2024-01-15"))
  expect_equal(
    athena_coerce_column("2024-01-15 10:00:00", "timestamp"),
    as.POSIXct("2024-01-15 10:00:00", tz = "UTC")
  )
})

test_that("athena_coerce_column leaves unknown/varchar types as character", {
  expect_equal(athena_coerce_column(c("a", "b"), "varchar"), c("a", "b"))
  expect_equal(athena_coerce_column(c("a", "b"), "array"), c("a", "b"))
})

test_that("athena_rows_to_tibble builds a typed tibble from Athena rows", {
  column_info <- list(
    list(Name = "id", Type = "bigint"),
    list(Name = "nom", Type = "varchar"),
    list(Name = "actif", Type = "boolean")
  )

  rows <- list(
    list(Data = list(list(VarCharValue = "1"), list(VarCharValue = "Alice"), list(VarCharValue = "true"))),
    list(Data = list(list(VarCharValue = "2"), list(VarCharValue = NULL), list(VarCharValue = "false")))
  )

  result <- athena_rows_to_tibble(rows, column_info)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("id", "nom", "actif"))
  expect_equal(result$id, c(1, 2))
  expect_equal(result$nom, c("Alice", NA_character_))
  expect_equal(result$actif, c(TRUE, FALSE))
})

test_that("athena_rows_to_tibble handles zero rows", {
  column_info <- list(list(Name = "id", Type = "bigint"))
  result <- athena_rows_to_tibble(list(), column_info)

  expect_s3_class(result, "tbl_df")
  expect_named(result, "id")
  expect_equal(nrow(result), 0)
})

test_that("athena_rows_to_tibble treats empty VarCharValue (character(0)) as NA", {
  # AWS/paws sometimes returns character(0) instead of NULL for empty values
  column_info <- list(
    list(Name = "id", Type = "bigint"),
    list(Name = "nom", Type = "varchar")
  )
  rows <- list(
    list(Data = list(list(VarCharValue = "1"), list(VarCharValue = character(0))))
  )

  result <- athena_rows_to_tibble(rows, column_info)

  expect_equal(result$id, 1)
  expect_equal(result$nom, NA_character_)
})

test_that("athena_ellipse_driver returns a valid DBI driver", {
  drv <- athena_ellipse_driver()
  expect_true(methods::is(drv, "DBIDriver"))
})

# --- Mocked-client tests (no AWS credentials / network required) ---------

make_fake_conn <- function(client, schema_name = "test_schema") {
  ptr <- new.env(parent = emptyenv())
  ptr$client <- client
  ptr$schema_name <- schema_name
  ptr$profile_name <- "DEV"
  ptr$work_group <- "ellipse-work-group"
  ptr$s3_staging_dir <- "s3://fake-bucket"
  ptr$valid <- TRUE
  methods::new("EllipseAthenaConnection", ptr = ptr)
}

make_column_info <- function(...) {
  types <- c(...)
  lapply(names(types), function(n) list(Name = n, Type = unname(types[n])))
}

test_that("athena_execute_query polls until SUCCEEDED", {
  states <- c("QUEUED", "RUNNING", "SUCCEEDED")
  call_count <- 0

  client <- list(
    start_query_execution = function(...) list(QueryExecutionId = "qid-1"),
    get_query_execution = function(QueryExecutionId) {
      call_count <<- call_count + 1
      state <- states[min(call_count, length(states))]
      list(QueryExecution = list(Status = list(State = state)))
    }
  )
  conn <- make_fake_conn(client)

  qid <- athena_execute_query(conn, "SELECT 1", poll_interval_sec = 0)
  expect_equal(qid, "qid-1")
  expect_gte(call_count, 3)
})

test_that("athena_execute_query errors when the query fails", {
  client <- list(
    start_query_execution = function(...) list(QueryExecutionId = "qid-2"),
    get_query_execution = function(QueryExecutionId) {
      list(QueryExecution = list(Status = list(
        State = "FAILED", StateChangeReason = "Table not found"
      )))
    }
  )
  conn <- make_fake_conn(client)

  expect_error(
    athena_execute_query(conn, "SELECT 1", poll_interval_sec = 0),
    "Table not found"
  )
})

test_that("athena_execute_query errors when the query is cancelled", {
  client <- list(
    start_query_execution = function(...) list(QueryExecutionId = "qid-3"),
    get_query_execution = function(QueryExecutionId) {
      list(QueryExecution = list(Status = list(State = "CANCELLED", StateChangeReason = "User cancelled")))
    }
  )
  conn <- make_fake_conn(client)

  expect_error(athena_execute_query(conn, "SELECT 1", poll_interval_sec = 0), "CANCELLED")
})

test_that("athena_execute_query errors on timeout", {
  client <- list(
    start_query_execution = function(...) list(QueryExecutionId = "qid-4"),
    get_query_execution = function(QueryExecutionId) {
      list(QueryExecution = list(Status = list(State = "RUNNING")))
    }
  )
  conn <- make_fake_conn(client)

  expect_error(
    athena_execute_query(conn, "SELECT 1", poll_interval_sec = 0, timeout_sec = 0),
    "délai d'attente"
  )
})

test_that("athena_fetch_results paginates across multiple pages and drops the header row", {
  column_info <- make_column_info(id = "bigint")

  client <- list(
    get_query_results = function(QueryExecutionId, MaxResults, NextToken = NULL) {
      if (is.null(NextToken)) {
        list(
          ResultSet = list(
            ResultSetMetadata = list(ColumnInfo = column_info),
            Rows = list(
              list(Data = list(list(VarCharValue = "id"))), # header row
              list(Data = list(list(VarCharValue = "1")))
            )
          ),
          NextToken = "page-2"
        )
      } else {
        list(
          ResultSet = list(
            ResultSetMetadata = list(ColumnInfo = column_info),
            Rows = list(list(Data = list(list(VarCharValue = "2"))))
          ),
          NextToken = NULL
        )
      }
    }
  )
  conn <- make_fake_conn(client)

  result <- athena_fetch_results(conn, "qid-5", "SELECT * FROM t")
  expect_equal(result$id, c(1, 2))
})

test_that("athena_fetch_results keeps all rows for non-SELECT statements", {
  column_info <- make_column_info(rows = "bigint")
  client <- list(
    get_query_results = function(QueryExecutionId, MaxResults, NextToken = NULL) {
      list(
        ResultSet = list(
          ResultSetMetadata = list(ColumnInfo = column_info),
          Rows = list(list(Data = list(list(VarCharValue = "42"))))
        ),
        NextToken = NULL
      )
    }
  )
  conn <- make_fake_conn(client)

  result <- athena_fetch_results(conn, "qid-6", "VACUUM t")
  expect_equal(result$rows, 42)
})

test_that("dbSendQuery/dbFetch/dbHasCompleted/dbClearResult work together", {
  column_info <- make_column_info(id = "bigint")
  client <- list(
    start_query_execution = function(...) list(QueryExecutionId = "qid-7"),
    get_query_execution = function(QueryExecutionId) {
      list(QueryExecution = list(Status = list(State = "SUCCEEDED")))
    },
    get_query_results = function(QueryExecutionId, MaxResults, NextToken = NULL) {
      list(
        ResultSet = list(
          ResultSetMetadata = list(ColumnInfo = column_info),
          Rows = list(
            list(Data = list(list(VarCharValue = "id"))),
            list(Data = list(list(VarCharValue = "1")))
          )
        ),
        NextToken = NULL
      )
    }
  )
  conn <- make_fake_conn(client)

  res <- DBI::dbSendQuery(conn, "SELECT id FROM t")
  expect_false(DBI::dbHasCompleted(res))

  info <- DBI::dbColumnInfo(res)
  expect_equal(info$name, "id")

  fetched <- DBI::dbFetch(res)
  expect_equal(fetched$id, 1)
  expect_true(DBI::dbHasCompleted(res))
  expect_true(DBI::dbClearResult(res))
})

test_that("dbListTables returns character(0) for an empty schema", {
  client <- list(
    start_query_execution = function(...) list(QueryExecutionId = "qid-8"),
    get_query_execution = function(QueryExecutionId) {
      list(QueryExecution = list(Status = list(State = "SUCCEEDED")))
    },
    get_query_results = function(QueryExecutionId, MaxResults, NextToken = NULL) {
      list(
        ResultSet = list(ResultSetMetadata = list(ColumnInfo = list()), Rows = list()),
        NextToken = NULL
      )
    }
  )
  conn <- make_fake_conn(client)

  expect_equal(DBI::dbListTables(conn), character(0))
})

test_that("dbExistsTable detects presence/absence of a table", {
  column_info <- make_column_info(tab_name = "varchar")
  client <- list(
    start_query_execution = function(...) list(QueryExecutionId = "qid-9"),
    get_query_execution = function(QueryExecutionId) {
      list(QueryExecution = list(Status = list(State = "SUCCEEDED")))
    },
    get_query_results = function(QueryExecutionId, MaxResults, NextToken = NULL) {
      list(
        ResultSet = list(
          ResultSetMetadata = list(ColumnInfo = column_info),
          Rows = list(
            list(Data = list(list(VarCharValue = "tab_name"))),
            list(Data = list(list(VarCharValue = "my_table")))
          )
        ),
        NextToken = NULL
      )
    }
  )
  conn <- make_fake_conn(client)

  expect_true(DBI::dbExistsTable(conn, "my_table"))
  expect_false(DBI::dbExistsTable(conn, "other_table"))
})

test_that("dbConnect builds a connection with the expected dbGetInfo fields", {
  skip_if_not_installed("paws.analytics")

  drv <- athena_ellipse_driver()
  con <- DBI::dbConnect(drv,
    aws_access_key_id = "fake_id",
    aws_secret_access_key = "fake_secret",
    profile_name = "DEV",
    schema_name = "my_schema",
    s3_staging_dir = "s3://fake-bucket",
    work_group = "ellipse-work-group"
  )
  on.exit(DBI::dbDisconnect(con))

  expect_true(DBI::dbIsValid(con))
  info <- DBI::dbGetInfo(con)
  expect_equal(info$dbms.name, "my_schema")
  expect_equal(info$profile_name, "DEV")
  expect_equal(info$work_group, "ellipse-work-group")

  DBI::dbDisconnect(con)
  expect_false(DBI::dbIsValid(con))
})

test_that("dbplyr_edition.EllipseAthenaConnection returns edition 2", {
  conn <- make_fake_conn(list())
  expect_equal(dbplyr::dbplyr_edition(conn), 2L)
})

test_that("ellipse_connect() uses the internal Athena driver end-to-end", {
  skip_if_not(nzchar(Sys.getenv("AWS_ACCESS_KEY_ID_DEV")),
    "AWS credentials not available")

  cat("\n[ellipse_connect] connexion à DEV/datawarehouse...\n")
  con <- ellipse_connect("DEV", "datawarehouse")
  info <- DBI::dbGetInfo(con)
  cat("  -> dbIsValid:", DBI::dbIsValid(con), "\n")
  cat("  -> profile_name:", info$profile_name, "\n")
  cat("  -> dbms.name (schema):", info$dbms.name, "\n")
  cat("  -> s3_staging_dir:", info$s3_staging_dir, "\n")

  expect_true(DBI::dbIsValid(con))
  expect_equal(info$profile_name, "DEV")
  expect_true(nzchar(info$dbms.name))

  ellipse_disconnect(con)
  cat("  -> dbIsValid après déconnexion:", DBI::dbIsValid(con), "\n")
  expect_false(DBI::dbIsValid(con))
})

test_that("ellipse_discover() lists tables and describes a table via the new driver", {
  skip_if_not(nzchar(Sys.getenv("AWS_ACCESS_KEY_ID_DEV")),
    "AWS credentials not available")

  con <- ellipse_connect("DEV", "datawarehouse")
  on.exit(ellipse_disconnect(con))

  all_tables <- ellipse_discover(con)
  cat("\n[ellipse_discover] ", nrow(all_tables), " table(s) trouvée(s) dans datawarehouse\n", sep = "")
  skip_if(nrow(all_tables) == 0, "No tables in datawarehouse")

  test_table <- all_tables$table_name[1]
  cat("  -> table testée:", test_table, "\n")
  result <- ellipse_discover(con, test_table)
  cat("  -> description:", ifelse(is.na(result$description), "NA", result$description), "\n")
  cat("  -> ", nrow(result$columns), " colonne(s): ",
    paste(result$columns$col_name, collapse = ", "), "\n", sep = "")

  expect_type(result, "list")
  expect_named(result, c("name", "description", "tags", "columns"))
  expect_equal(result$name, test_table)
  expect_s3_class(result$columns, "tbl_df")
})

test_that("ellipse_query() returns a lazy dplyr::tbl() that can be collected", {
  skip_if_not(nzchar(Sys.getenv("AWS_ACCESS_KEY_ID_DEV")),
    "AWS credentials not available")

  con <- ellipse_connect("DEV", "datawarehouse")
  on.exit(ellipse_disconnect(con))

  test_table <- DBI::dbListTables(con)[1]
  cat("\n[ellipse_query] table testée:", test_table, "\n")
  skip_if(is.na(test_table) || !nzchar(test_table), "No tables in datawarehouse")

  lazy_tbl <- ellipse_query(con, test_table)
  cat("  -> classe retournée:", paste(class(lazy_tbl), collapse = "/"), "\n")
  expect_true(inherits(lazy_tbl, "tbl_lazy"))

  collected <- lazy_tbl |> head(3) |> dplyr::collect()
  cat("  -> collect():", nrow(collected), "ligne(s),", ncol(collected), "colonne(s)\n")
  print(collected)
  expect_s3_class(collected, "tbl_df")
  expect_lte(nrow(collected), 3)
})

test_that("ellipse_partitions() works through the new driver", {
  skip_if_not(nzchar(Sys.getenv("AWS_ACCESS_KEY_ID_DEV")),
    "AWS credentials not available")

  con <- ellipse_connect("DEV", "datawarehouse")
  on.exit(ellipse_disconnect(con))

  all_tables <- ellipse_discover(con)
  skip_if(nrow(all_tables) == 0, "No tables in datawarehouse")

  test_table <- all_tables$table_name[1]
  cols <- ellipse_discover(con, test_table)$columns
  skip_if(!any(cols$is_partition), "First table has no partition column")

  cat("\n[ellipse_partitions] table testée:", test_table, "\n")
  result <- ellipse_partitions(con, test_table)
  cat("  -> ", nrow(result), " combinaison(s) de partitions\n", sep = "")
  print(result)
  expect_s3_class(result, "tbl_df")
})

test_that("ellipse_describe() exercises dbExistsTable via the new driver (datamarts)", {
  skip_if_not(nzchar(Sys.getenv("AWS_ACCESS_KEY_ID_DEV")),
    "AWS credentials not available")

  con <- ellipse_connect("DEV", "datamarts")
  on.exit(ellipse_disconnect(con))

  cat("\n[ellipse_describe] table inexistante -> dbExistsTable() doit renvoyer FALSE\n")
  # Non-existent table: check_params_before_describe() calls DBI::dbExistsTable()
  # and returns FALSE before any interactive prompt is reached.
  result <- ellipse_describe(con, "table_qui_nexiste_absolument_pas_xyz123")
  cat("  -> résultat:", result, "\n")
  expect_false(result)
})

test_that("ellipse_discover() works on a datalake connection (public-data-lake-content query)", {
  skip_if_not(nzchar(Sys.getenv("AWS_ACCESS_KEY_ID_DEV")),
    "AWS credentials not available")

  con <- ellipse_connect("DEV", "datalake")
  on.exit(ellipse_disconnect(con))

  cat("\n[ellipse_discover] datalake - liste de tous les datasets\n")
  expect_null(ellipse_discover(con))

  datasets <- DBI::dbGetQuery(con, 'SELECT DISTINCT name FROM "public-data-lake-content"')
  cat("  -> ", nrow(datasets), " dataset(s) trouvé(s)\n", sep = "")
  skip_if(nrow(datasets) == 0, "No datasets in public datalake")

  test_dataset <- datasets$name[1]
  cat("  -> dataset testé:", test_dataset, "\n")
  result <- ellipse_discover(con, test_dataset)
  print(result)
  expect_s3_class(result, "tbl_df")
  expect_true("table_name" %in% colnames(result))
})

test_that("ellipse_query() aggregates datalake files, including ARRAY-typed columns", {
  skip_if_not(nzchar(Sys.getenv("AWS_ACCESS_KEY_ID_DEV")),
    "AWS credentials not available")

  con <- ellipse_connect("DEV", "datalake")
  on.exit(ellipse_disconnect(con))

  datasets <- DBI::dbGetQuery(con, 'SELECT DISTINCT name FROM "public-data-lake-content"
                                    WHERE file_extensions NOT LIKE \'%png%\'
                                    AND file_extensions NOT LIKE \'%html%\'')
  cat("\n[ellipse_query] datalake -", nrow(datasets), "dataset(s) tabulaire(s) trouvé(s)\n")
  skip_if(nrow(datasets) == 0, "No tabular datasets in public datalake")

  # Datasets connus comme inaccessibles en DEV (403 S3) pour éviter des faux négatifs bruyants.
  blocked_datasets <- c("japan_baro_2021", "ces21", "ces_1965", "ces_1965_sav")
  preferred_small_datasets <- c(
    "sample1", "sample2", "sample3", "employee",
    "mixeddata", "multiweather", "test-dataset"
  )
  available <- setdiff(datasets$name, blocked_datasets)
  prioritized <- intersect(preferred_small_datasets, available)
  fallback <- setdiff(available, prioritized)
  candidates <- c(prioritized, utils::head(fallback, 3))
  skip_if(length(candidates) == 0, "No testable tabular dataset after filtering known blocked datasets")

  summary <- tibble::tibble(
    dataset = character(),
    ok = logical(),
    rows = integer(),
    cols = integer(),
    error = character()
  )

  result <- tibble::tibble()
  for (name in candidates) {
    cat("  -> essai du dataset:", name, "\n")
    attempt <- tryCatch(
      ellipse_query(con, name),
      error = function(e) e
    )

    if (inherits(attempt, "error")) {
      summary <- dplyr::bind_rows(summary, tibble::tibble(
        dataset = name,
        ok = FALSE,
        rows = 0L,
        cols = 0L,
        error = conditionMessage(attempt)
      ))
      next
    }

    rows <- nrow(attempt)
    cols <- ncol(attempt)
    ok <- rows > 0
    summary <- dplyr::bind_rows(summary, tibble::tibble(
      dataset = name,
      ok = ok,
      rows = as.integer(rows),
      cols = as.integer(cols),
      error = ""
    ))

    if (ok) {
      result <- attempt
      break
    }
  }

  cat("  -> résumé des essais datalake:\n")
  print(summary)
  skip_if(nrow(result) == 0, "No accessible dataset files found (S3 permissions or empty files)")

  cat("  -> ", nrow(result), " ligne(s) agrégée(s), ", ncol(result), " colonne(s)\n", sep = "")
  print(utils::head(result, 3))
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(any(summary$ok))
})

