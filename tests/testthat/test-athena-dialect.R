library(testthat)

# Connexion factice hors-ligne : le client paws n'est instancie qu'a la
# premiere requete, donc dbConnect + sql_render ne touchent pas le reseau.
fake_con <- function() {
  drv <- athena_ellipse_driver()
  DBI::dbConnect(drv,
    aws_access_key_id = "fake_id",
    aws_secret_access_key = "fake_secret",
    profile_name = "DEV",
    schema_name = "test_schema",
    s3_staging_dir = "s3://fake-bucket",
    work_group = "ellipse-athena-workgroup"
  )
}

render <- function(expr, con) {
  as.character(dbplyr::sql_render(expr, con = con))
}

describe("dialecte Athena (sql_dialect_athena)", {
  con <- fake_con()
  lf <- dbplyr::lazy_frame(d = as.Date("2026-01-01"), n = 1L, txt = "abc", con = con)

  it("enregistre le dialecte athena sur la connexion", {
    expect_s3_class(dbplyr::sql_dialect(con), "sql_dialect_athena")
  })

  it("traduit Sys.Date() en current_date", {
    sql <- render(dplyr::filter(lf, d >= Sys.Date()), con)
    expect_match(sql, "current_date", fixed = TRUE)
    expect_false(grepl("Sys.Date", sql, fixed = TRUE))
  })

  it("traduit Sys.time() en now()", {
    sql <- render(dplyr::filter(lf, d >= as.Date(Sys.time())), con)
    expect_match(sql, "now()", fixed = TRUE)
  })

  it("traduit floor_date en date_trunc", {
    sql <- render(dplyr::mutate(lf, x = lubridate::floor_date(d, "month")), con)
    expect_match(sql, "date_trunc('month'", fixed = TRUE)
  })

  it("traduit str_detect en REGEXP_LIKE (pas INSTR)", {
    sql <- render(dplyr::filter(lf, stringr::str_detect(txt, "a")), con)
    expect_match(sql, "REGEXP_LIKE", fixed = TRUE)
    expect_false(grepl("INSTR", sql, fixed = TRUE))
  })

  it("traduit les durees lubridate en INTERVAL", {
    sql <- render(dplyr::mutate(lf, x = d + lubridate::days(3)), con)
    expect_match(sql, "INTERVAL '3' day", fixed = TRUE)
  })

  it("echappe un littéral Date en DATE '...'", {
    expect_equal(as.character(dbplyr::escape(as.Date("2026-11-01"), con = con)), "DATE '2026-11-01'")
  })

  it("echappe un littéral POSIXct en TIMESTAMP '...'", {
    x <- as.POSIXct("2026-11-01 10:30:00", tz = "UTC")
    expect_equal(as.character(dbplyr::escape(x, con = con)), "TIMESTAMP '2026-11-01 10:30:00'")
  })

  it("garde les casts as.* (CAST ... AS DATE, etc.)", {
    sql <- render(dplyr::mutate(lf, x = as.Date(d)), con)
    expect_match(sql, "CAST(", fixed = TRUE)
    expect_match(sql, "AS DATE", fixed = TRUE)
  })

  it("documente la règle restante : un littéral string reste du varchar", {
    # Comparer une colonne date a une string echoue encore sur Athena
    # (TYPE_MISMATCH) : le dialecte ne caste pas selon le type de la colonne.
    # Les appelants doivent utiliser as.Date() / as.POSIXct() / un nombre.
    sql <- render(dplyr::filter(lf, d >= "2026-11-01"), con)
    expect_match(sql, "'2026-11-01'", fixed = TRUE)
    expect_false(grepl("DATE '2026-11-01'", sql, fixed = TRUE))
  })

  it("garde les traductions ANSI qui marchent sur Athena (CASE WHEN, GREATEST, CONCAT_WS, EXTRACT)", {
    sql1 <- render(dplyr::mutate(lf, x = ifelse(n > 0, "a", "b")), con)
    expect_match(sql1, "CASE WHEN", fixed = TRUE)
    sql2 <- render(dplyr::mutate(lf, x = pmax(n, 0)), con)
    expect_match(sql2, "GREATEST", fixed = TRUE)
    sql3 <- render(dplyr::mutate(lf, x = paste0(txt, "x")), con)
    expect_match(sql3, "CONCAT_WS", fixed = TRUE)
    sql4 <- render(dplyr::mutate(lf, y = lubridate::year(d)), con)
    expect_match(sql4, "EXTRACT(year", fixed = TRUE)
  })
})
