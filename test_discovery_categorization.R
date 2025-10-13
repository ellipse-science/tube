#!/usr/bin/env Rscript

cat("=== Testing Discovery Categorization ===\n")

# Load environment and functions
readRenviron('.Renviron')
source('R/utils_datalake_discover.R')
source('R/utils_datalake_connect.R')

# Test connection
cat("\n1. Connecting to public datalake...\n")
tryCatch({
  con <- ellipse_connect()
  if (!is.null(con)) {
    cat("✅ Connected successfully\n")
    
    # Test the new categorized discovery
    cat("\n2. Testing categorized discovery...\n")
    result <- format_public_datalake_all_datasets(con)
    
    cat("✅ Discovery categorization complete\n")
    
    # Clean up
    ellipse_disconnect(con)
    cat("✅ Disconnected\n")
  } else {
    cat("❌ Connection failed\n")
  }
}, error = function(e) {
  cat("❌ Error:", conditionMessage(e), "\n")
})

cat("\n=== Test Complete ===\n")