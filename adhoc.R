# Ad-hoc script for temporary R operations. Do NOT use this file to run unit tests.
pkgload::load_all(quiet = TRUE)

con <- ellipse_connect("DEV", "datawarehouse")
table <- DBI::dbListTables(con)[1]
cat("table:", table, "\n")

cat("\n--- dplyr::tbl() lazy print ---\n")
lazy_tbl <- dplyr::tbl(con, table)
print(lazy_tbl)

cat("\n--- dplyr chain + collect() ---\n")
result <- lazy_tbl |> head(3) |> dplyr::collect()
print(result)

cat("\n--- ellipse_query() ---\n")
r2 <- ellipse_query(con, table)
print(r2 |> head(3) |> dplyr::collect())

cat("\n--- ellipse_discover() ---\n")
print(ellipse_discover(con, table))

DBI::dbDisconnect(con)
cat("dbIsValid after disconnect:", DBI::dbIsValid(con), "\n")
