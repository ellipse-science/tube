test_that("MULTIPART_THRESHOLD_BYTES and MULTIPART_PART_SIZE_BYTES are positive integers", {
  expect_true(is.integer(tube:::MULTIPART_THRESHOLD_BYTES))
  expect_true(tube:::MULTIPART_THRESHOLD_BYTES > 0L)
  expect_true(is.integer(tube:::MULTIPART_PART_SIZE_BYTES))
  expect_true(tube:::MULTIPART_PART_SIZE_BYTES >= 5L * 1024L * 1024L)
})

test_that("multipart_upload_to_s3 uploads all parts and completes successfully", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeBin(as.raw(rep(0x41, 10)), tmp)

  parts_uploaded <- list()
  completed <- FALSE
  aborted <- FALSE

  mock_s3 <- list(
    create_multipart_upload = function(...) list(UploadId = "uid-test"),
    upload_part = function(PartNumber, ...) {
      parts_uploaded[[length(parts_uploaded) + 1L]] <<- PartNumber
      list(ETag = paste0("etag-", PartNumber))
    },
    complete_multipart_upload = function(MultipartUpload, ...) {
      completed <<- TRUE
      list()
    },
    abort_multipart_upload = function(...) {
      aborted <<- TRUE
    }
  )

  tube:::multipart_upload_to_s3(
    creds = list(),
    bucket = "test-bucket",
    key = "test/key.rds",
    file_path = tmp,
    metadata = list(),
    content_type = "application/octet-stream",
    s3_client = mock_s3
  )

  expect_true(completed)
  expect_false(aborted)
  expect_equal(length(parts_uploaded), 1L)
})

test_that("multipart_upload_to_s3 aborts and re-throws on upload_part error", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeBin(as.raw(rep(0x42, 10)), tmp)

  aborted <- FALSE

  mock_s3 <- list(
    create_multipart_upload = function(...) list(UploadId = "uid-fail"),
    upload_part = function(...) stop("simulated upload failure"),
    complete_multipart_upload = function(...) list(),
    abort_multipart_upload = function(...) {
      aborted <<- TRUE
    }
  )

  expect_error(
    tube:::multipart_upload_to_s3(
      creds = list(),
      bucket = "test-bucket",
      key = "test/key.rds",
      file_path = tmp,
      metadata = list(),
      content_type = "application/octet-stream",
      s3_client = mock_s3
    ),
    "simulated upload failure"
  )

  expect_true(aborted)
})
