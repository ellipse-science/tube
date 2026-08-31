test_that("build_process_prefix keeps pipeline prefix unchanged in datawarehouse", {
  expect_equal(
    tube:::build_process_prefix("datawarehouse", "a-qc-press-releases"),
    "a-qc-press-releases"
  )
})

test_that("build_process_prefix converts first separator for datamarts", {
  expect_equal(
    tube:::build_process_prefix("datamarts", "vitrine_datamart-radar_annotated"),
    "vitrine_datamart/radar_annotated"
  )
})

test_that("is_no_new_data_result detects run_glue_job sentinel -1", {
  expect_true(tube:::is_no_new_data_result(-1))
  expect_false(tube:::is_no_new_data_result(TRUE))
  expect_false(tube:::is_no_new_data_result(FALSE))
})

test_that("extract_partitions_from_common_prefixes returns datawarehouse first-level partitions", {
  common_prefixes <- list(
    list(Prefix = "a-qc-press-releases/PLQ/"),
    list(Prefix = "a-qc-press-releases/Q1/")
  )

  expect_equal(
    tube:::extract_partitions_from_common_prefixes(common_prefixes, "a-qc-press-releases"),
    c("PLQ", "Q1")
  )
})

test_that("extract_partitions_from_common_prefixes returns datamarts table partitions", {
  common_prefixes <- list(
    list(Prefix = "vitrine_datamart/radar_annotated/DEFAULT/"),
    list(Prefix = "vitrine_datamart/radar_annotated/2026-01-01/")
  )

  expect_equal(
    tube:::extract_partitions_from_common_prefixes(common_prefixes, "vitrine_datamart/radar_annotated"),
    c("DEFAULT", "2026-01-01")
  )
})

test_that("extract_partitions_from_common_prefixes handles empty prefixes", {
  expect_equal(
    tube:::extract_partitions_from_common_prefixes(NULL, "a-qc-press-releases"),
    character(0)
  )
  expect_equal(
    tube:::extract_partitions_from_common_prefixes(list(), "a-qc-press-releases"),
    character(0)
  )
})

test_that("normalize_glue_partitions keeps first segment for datawarehouse", {
  expect_equal(
    tube:::normalize_glue_partitions(c("CAQ/processed", "PLQ/unprocessed", "PLQ"), "datawarehouse"),
    c("CAQ", "PLQ")
  )
})

test_that("normalize_glue_partitions keeps full values for datamarts", {
  expect_equal(
    tube:::normalize_glue_partitions(c("DEFAULT", "2026-01-01"), "datamarts"),
    c("DEFAULT", "2026-01-01")
  )
})

test_that("resolve_unprocessed_prefixes handles datamarts direct unprocessed partition", {
  expect_equal(
    tube:::resolve_unprocessed_prefixes(
      database = "datamarts",
      prefix = "vitrine_datamart/radar_annotated",
      partition = "unprocessed",
      common_prefixes = NULL
    ),
    "vitrine_datamart/radar_annotated/unprocessed/"
  )
})

test_that("resolve_unprocessed_prefixes extracts unprocessed from common prefixes", {
  common_prefixes <- list(
    list(Prefix = "a-qc-press-releases/PLQ/processed/"),
    list(Prefix = "a-qc-press-releases/PLQ/unprocessed/")
  )

  expect_equal(
    tube:::resolve_unprocessed_prefixes(
      database = "datawarehouse",
      prefix = "a-qc-press-releases",
      partition = "PLQ",
      common_prefixes = common_prefixes
    ),
    "a-qc-press-releases/PLQ/unprocessed/"
  )
})

test_that("build_partition_listing_prefix always ends with slash", {
  expect_equal(
    tube:::build_partition_listing_prefix("a-qc-press-releases", "PLQ"),
    "a-qc-press-releases/PLQ/"
  )

  expect_equal(
    tube:::build_partition_listing_prefix("a-qc-press-releases/", "PLQ/"),
    "a-qc-press-releases/PLQ/"
  )
})
