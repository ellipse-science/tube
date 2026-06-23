test_that("is_missing_aws_credential detects vector values", {
  expect_true(is_missing_aws_credential(c("value1", "value2")))
})

test_that("is_missing_aws_credential detects missing scalar values", {
  expect_true(is_missing_aws_credential(""))
  expect_true(is_missing_aws_credential(NA_character_))
  expect_true(is_missing_aws_credential(NULL))
})

test_that("is_missing_aws_credential accepts valid scalar strings", {
  expect_false(is_missing_aws_credential("value1"))
})
