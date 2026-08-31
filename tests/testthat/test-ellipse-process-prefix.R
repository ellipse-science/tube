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
