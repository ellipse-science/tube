# CURRENT-FUNCTION-INVENTORY — tube (après feature/011)

## Fonctions exportées (NAMESPACE, 14)
| Fonction | Rôle | Fichier |
|---|---|---|
| `ellipse_connect(env, database)` | Connexion DBI Athena (PROD/DEV × datawarehouse/datamarts/datalake) | R/ellipse.R |
| `ellipse_disconnect(con)` | Fermeture de connexion | R/ellipse.R |
| `ellipse_discover(con, table, tag)` | Liste des tables / description d'une table | R/ellipse.R |
| `ellipse_query(con, dataset, tag, file)` | Requête lazy (table) ou agrégation fichiers (datalake) | R/ellipse.R → R/utils_datalake_query.R |
| `ellipse_describe(con, table)` | Description d'une table | R/ellipse.R |
| `ellipse_partitions(con, table)` | Valeurs des partitions d'une table | R/ellipse.R |
| `ellipse_push(...)` | Upload datalake public ou landing zone | R/ellipse.R |
| `ellipse_ingest` / `ellipse_process` / `ellipse_publish` | Pipeline landing zone (legacy) | R/ellipse.R |
| `ellipse_unpublish` / `ellipse_unpush` | Retrait de données publiées | R/ellipse.R |
| `convert_url_to_key` | Normalisation d'URL en clé | R/utils_url.R |
| `%>%` | Pipe réexporté | R/utils_pipe.R |

## Internes clés autour du fix
- `athena_ellipse_driver()` / classe S4 `EllipseAthenaConnection` — R/athena_dbi.R
- `athena_execute_query()` — start + polling Athena — R/athena_dbi.R
- `athena_fetch_results()` — pagination `GetQueryResults` + détection d'en-tête — R/athena_dbi.R (fix feature/011)
- `athena_rows_to_tibble()` / `athena_coerce_column()` — typage des résultats — R/athena_dbi.R
- `ellipse_query_table_mode()` — `SHOW TABLES IN <schema>` puis `dplyr::tbl()` — R/utils_datalake_query.R
- `list_datawarehouse_database()` / `list_glue_databases()` — résolution du schéma par `grep("datawarehouse", ...)` — R/datawarehouse.R, R/glue.R

## Schémas AWS (compte PROD 767398150629, ca-central-1)
- datawarehouse : `gluestackdatawarehousedbe64d5725` (24 tables)
- datamarts : `gluestackdatamartdbd046f685`
- public datalake : `gluestackpublicdatalakedbbeb173fb`
- workgroup Athena : `ellipse-athena-workgroup`
- staging : `bucket-stack-athenaqueryresultsbucket6f63bbe4-axrogwxi2lrx`
