# AWS Lambda utility functions

#' List all Lambda functions with robust AWS API pagination
#' @param credentials AWS credentials from get_aws_credentials()
#' @return Vector of lambda function names
#' @keywords internal
list_lambda_functions <- function(credentials) {
  logger::log_debug("[tube::list_lambda_functions] entering function")
  
  lambda_client <- paws.compute::lambda(
    config = c(
      credentials,
      close_connection = TRUE
    )
  )
  
  tryCatch({
    # Raw API call with comprehensive error handling and pagination
    all_lambda_functions <- list()
    next_marker <- NULL
    page_count <- 0

    repeat {
      page_count <- page_count + 1
      logger::log_debug(paste("Fetching Lambda functions page", page_count, "..."))

      lambda_functions_page <- tryCatch({
        # Make API call with or without marker
        if (is.null(next_marker)) {
          result <- lambda_client$list_functions()
        } else {
          result <- lambda_client$list_functions(Marker = next_marker)
        }

        logger::log_debug(paste("list_functions() page", page_count, "call successful"))
        result
      }, error = function(e) {
        logger::log_error(paste("list_functions() page", page_count, "failed with error:", e$message))
        stop(e)
      })

      # Add functions from this page to our collection
      if (length(lambda_functions_page$Functions) > 0) {
        all_lambda_functions <- c(all_lambda_functions, lambda_functions_page$Functions)
        logger::log_debug(paste("Page", page_count, "returned", length(lambda_functions_page$Functions), "functions"))
        logger::log_debug(paste("Total functions collected so far:", length(all_lambda_functions)))
      }

      # Check if there are more pages with safe evaluation
      has_next_marker <- tryCatch({
        !is.null(lambda_functions_page$NextMarker) &&
        !is.na(lambda_functions_page$NextMarker) &&
        nchar(lambda_functions_page$NextMarker) > 0
      }, error = function(e) {
        logger::log_error(paste("Error checking NextMarker:", e$message))
        FALSE
      })

      if (isTRUE(has_next_marker)) {
        next_marker <- lambda_functions_page$NextMarker
        logger::log_debug("More pages available")
      } else {
        logger::log_debug("No more pages available")
        break
      }

      # Safety check to prevent infinite loops
      if (page_count > 100) {
        logger::log_warn("Stopping pagination after 100 pages (safety limit)")
        break
      }
    }

    logger::log_debug(paste("Pagination complete. Total functions found:", length(all_lambda_functions)))

    if (length(all_lambda_functions) == 0) {
      logger::log_debug("[tube::list_lambda_functions] no lambda functions found")
      return(character(0))
    }

    # Safely extract function names with error handling
    function_names <- tryCatch({
      sapply(all_lambda_functions, function(f) {
        if (is.null(f$FunctionName) || is.na(f$FunctionName)) {
          return("")  # Return empty string for invalid names
        }
        f$FunctionName
      })
    }, error = function(e) {
      logger::log_error(paste("Error extracting function names:", e$message))
      return(character(0))
    })

    # Filter out empty names
    function_names <- function_names[function_names != ""]
    
    logger::log_debug(paste("[tube::list_lambda_functions] found", length(function_names), "lambda functions across", page_count, "pages"))
    
    return(function_names)
    
  }, error = function(e) {
    logger::log_error(paste("[tube::list_lambda_functions] error listing lambdas:", e$message))
    return(character(0))
  })
}

#' Find lambda function by pattern matching with robust AWS API pagination
#' @param credentials AWS credentials from get_aws_credentials()
#' @param patterns Vector of regex patterns to search for
#' @param return_first Whether to return first match or all matches
#' @return Lambda function name(s) if found, NULL if none found
#' @keywords internal
find_lambda_by_pattern <- function(credentials, patterns, return_first = TRUE) {
  logger::log_debug("[tube::find_lambda_by_pattern] entering function")
  
  lambda_client <- paws.compute::lambda(
    config = c(
      credentials,
      close_connection = TRUE
    )
  )
  
  tryCatch({
    logger::log_debug("Starting Lambda function search with patterns...")
    
    # Raw API call with comprehensive error handling and pagination
    all_lambda_functions <- list()
    next_marker <- NULL
    page_count <- 0

    repeat {
      page_count <- page_count + 1
      logger::log_debug(paste("Fetching Lambda functions page", page_count, "..."))

      lambda_functions_page <- tryCatch({
        # Make API call with or without marker
        if (is.null(next_marker)) {
          result <- lambda_client$list_functions()
        } else {
          result <- lambda_client$list_functions(Marker = next_marker)
        }

        logger::log_debug(paste("list_functions() page", page_count, "call successful"))
        result
      }, error = function(e) {
        logger::log_error(paste("list_functions() page", page_count, "failed with error:", e$message))
        stop(e)
      })

      # Add functions from this page to our collection
      if (length(lambda_functions_page$Functions) > 0) {
        all_lambda_functions <- c(all_lambda_functions, lambda_functions_page$Functions)
        logger::log_debug(paste("Page", page_count, "returned", length(lambda_functions_page$Functions), "functions"))
        logger::log_debug(paste("Total functions collected so far:", length(all_lambda_functions)))
      }

      # Debug NextMarker before processing
      logger::log_debug(paste("NextMarker exists:", !is.null(lambda_functions_page$NextMarker)))
      if (!is.null(lambda_functions_page$NextMarker)) {
        logger::log_debug(paste("NextMarker nchar:", nchar(lambda_functions_page$NextMarker)))
      }

      # Check if there are more pages with safe evaluation
      has_next_marker <- tryCatch({
        !is.null(lambda_functions_page$NextMarker) &&
        !is.na(lambda_functions_page$NextMarker) &&
        nchar(lambda_functions_page$NextMarker) > 0
      }, error = function(e) {
        logger::log_error(paste("Error checking NextMarker:", e$message))
        FALSE
      })

      logger::log_debug(paste("has_next_marker result:", has_next_marker))

      if (isTRUE(has_next_marker)) {
        next_marker <- lambda_functions_page$NextMarker
        logger::log_debug(paste("More pages available, next marker:", substr(next_marker, 1, 20), "..."))
      } else {
        logger::log_debug("No more pages available")
        break
      }

      # Safety check to prevent infinite loops
      if (page_count > 100) {
        logger::log_warn("Stopping pagination after 100 pages (safety limit)")
        break
      }
    }

    logger::log_debug(paste("Pagination complete. Total functions found:", length(all_lambda_functions)))

    if (length(all_lambda_functions) == 0) {
      logger::log_debug("[tube::find_lambda_by_pattern] no lambda functions found")
      return(NULL)
    }

    # Search for matches with each pattern using robust logic
    all_matches <- character(0)
    
    for (pattern in patterns) {
      logger::log_debug(paste("Searching for pattern:", pattern))
      
      # Search through all functions for this pattern
      for (i in seq_along(all_lambda_functions)) {
        func <- all_lambda_functions[[i]]
        
        # Safety check for function structure
        if (is.null(func$FunctionName) || is.na(func$FunctionName)) {
          next
        }

        # Safe grepl with error handling
        match_result <- tryCatch({
          grepl(pattern, func$FunctionName, ignore.case = TRUE)
        }, error = function(e) {
          logger::log_error(paste("Error in grepl for function", i, ":", e$message))
          FALSE
        })

        if (isTRUE(match_result)) {
          logger::log_debug(paste("MATCH found! Function:", func$FunctionName, "matches pattern:", pattern))
          
          if (return_first) {
            logger::log_debug(paste("[tube::find_lambda_by_pattern] returning first match:", func$FunctionName))
            return(func$FunctionName)
          } else {
            all_matches <- c(all_matches, func$FunctionName)
          }
        }
      }
    }
    
    if (return_first) {
      logger::log_debug("[tube::find_lambda_by_pattern] no matches found across all pages")
      return(NULL)
    } else {
      if (length(all_matches) > 0) {
        logger::log_debug(paste("[tube::find_lambda_by_pattern] returning", length(all_matches), "total matches"))
        return(unique(all_matches))
      } else {
        logger::log_debug("[tube::find_lambda_by_pattern] no matches found across all pages")
        return(NULL)
      }
    }
    
  }, error = function(e) {
    logger::log_error(paste("[tube::find_lambda_by_pattern] error:", e$message))
    return(NULL)
  })
}
