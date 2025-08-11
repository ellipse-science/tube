#!/usr/bin/env Rscript

# adhoc.R - Reusable script for temporary R operations
# ALWAYS edit and reuse this file instead of creating new R scripts

cat("=== TESTING RESTORED STATE ===\n")

# Load environment and package
readRenviron('.Renviron')
library(devtools)
load_all('.')
library(testthat)

# Source helper functions
source("tests/testthat/helper-functions.R")

cat("\n1. CONFIRMING DATAWAREHOUSE FIX STILL WORKS:\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

creds <- get_real_aws_credentials_dev()
if (!is.null(creds)) {
  cat("✅ Credentials available\n")
  
  # Test the fixed datawarehouse function
  db_name <- list_datawarehouse_database(creds)
  cat("Database found:", db_name, "\n")
  
  tables_simple <- list_datawarehouse_tables(creds, simplify = TRUE)
  cat("Tables (simplified):", class(tables_simple), "with", nrow(tables_simple), "rows\n")
  
  cat("✅ Datawarehouse function still working!\n")
} else {
  cat("❌ No credentials available\n")
}

cat("\n2. RUNNING COMPLETE TEST SUITE:\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

# Run tests and capture results
test_results <- devtools::test()

cat("\n=== SHOULD BE BACK TO WORKING STATE ===\n")