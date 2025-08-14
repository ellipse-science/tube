# AWS Lambda utility functions

#' List all Lambda functions
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
    response <- lambda_client$list_functions()
    
    if (is.null(response$Functions) || length(response$Functions) == 0) {
      logger::log_debug("[tube::list_lambda_functions] no lambda functions found")
      return(character(0))
    }
    
    function_names <- sapply(response$Functions, function(f) f$FunctionName)
    logger::log_debug(paste("[tube::list_lambda_functions] found", length(function_names), "lambda functions"))
    
    return(function_names)
    
  }, error = function(e) {
    logger::log_error(paste("[tube::list_lambda_functions] error listing lambdas:", e$message))
    return(character(0))
  })
}

#' Find lambda function by pattern matching
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
    # List all lambda functions
    response <- lambda_client$list_functions()
    
    if (is.null(response$Functions) || length(response$Functions) == 0) {
      logger::log_debug("[tube::find_lambda_by_pattern] no lambda functions found")
      return(NULL)
    }
    
    # Get function names
    function_names <- sapply(response$Functions, function(f) f$FunctionName)
    logger::log_debug(paste("[tube::find_lambda_by_pattern] searching", length(function_names), "lambda functions"))
    
    # Search for matches with each pattern
    all_matches <- character(0)
    for (pattern in patterns) {
      matches <- function_names[grepl(pattern, function_names, ignore.case = TRUE)]
      if (length(matches) > 0) {
        logger::log_debug(paste("[tube::find_lambda_by_pattern] found", length(matches), "match(es) with pattern:", pattern))
        if (return_first) {
          logger::log_debug(paste("[tube::find_lambda_by_pattern] returning first match:", matches[1]))
          return(matches[1])
        } else {
          all_matches <- c(all_matches, matches)
        }
      }
    }
    
    if (return_first) {
      logger::log_debug("[tube::find_lambda_by_pattern] no matches found")
      return(NULL)
    } else {
      if (length(all_matches) > 0) {
        logger::log_debug(paste("[tube::find_lambda_by_pattern] returning", length(all_matches), "total matches"))
        return(unique(all_matches))
      } else {
        logger::log_debug("[tube::find_lambda_by_pattern] no matches found")
        return(NULL)
      }
    }
    
  }, error = function(e) {
    logger::log_error(paste("[tube::find_lambda_by_pattern] error:", e$message))
    return(NULL)
  })
}
