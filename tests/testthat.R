# This file is part of the standard testthat setup.
# It ensures that testthat is run during R CMD check.

library(testthat)
library(tube)

test_check("tube")
