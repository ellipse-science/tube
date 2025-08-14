#' Returns the public datalake bucket name
#'
#' Technically the function returns all the buckets for which
#' the name contains the string `publicdatalakebucket`
#'
#' In our infrastructure, there is only one per AWS account (DEV/PROD)
#'
#' It is used mainly by the ellipse_connect function and related
#' discovery functions to interact with user-uploaded files
#' in the public datalake.
#'
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @returns A string containing the name of the public datalake bucket
list_public_datalake_bucket <- function(credentials) {
  logger::log_debug("[tube::list_public_datalake_bucket] entering function")

  public_datalake_list <- list_s3_buckets(credentials, "publicdatalakebucket")

  logger::log_debug("[tube::list_public_datalake_bucket] returning results")
  public_datalake_list
}

#' Returns the public datalake GLUE database name
#'
#' Technically the function returns all the databases of which
#' the name contains the string `publicdatalake` but in our
#' infrastructure, there is only one per AWS account (DEV/PROD)
#'
#' It is used mainly by the ellipse_connect function to retrieve the
#' schema with which to instantiate the DBI connection for the
#' public datalake containing user-uploaded files with metadata.
#'
#' @param credentials A list of AWS credentials in the format compliant
#' with the paws package
#' @returns A string containing the name of the public datalake database
list_public_datalake_database <- function(credentials) {
  logger::log_debug("[tube::list_public_datalake_database] entering function")

  public_datalake_database <- list_glue_databases(credentials, "publicdatalake")

  logger::log_debug("[tube::list_public_datalake_database] returning results")
  public_datalake_database
}

#' Upload file to public datalake with metadata
#' @description Upload a file to the public datalake S3 bucket with rich metadata
#' @param credentials AWS credentials from get_aws_credentials()
#' @param file_path Local file path to upload
#' @param dataset_name Dataset name for organization
#' @param tag Tag for versioning/categorization
#' @param metadata List of metadata including system and user fields
#' @return TRUE if successful, FALSE otherwise
#' @keywords internal
upload_file_to_public_datalake <- function(credentials, file_path, dataset_name, tag, metadata) {
  logger::log_debug("[tube::upload_file_to_public_datalake] entering function")

  # Get public datalake bucket
  bucket <- list_public_datalake_bucket(credentials)
  if (is.null(bucket) || length(bucket) == 0) {
    cli::cli_alert_danger("Impossible de trouver le bucket public datalake")
    return(FALSE)
  }

  # If multiple buckets, use the first one
  if (length(bucket) > 1) {
    bucket <- bucket[1]
  }

  # Create S3 key: dataset_name/tag/filename
  filename <- basename(file_path)
  key <- paste(dataset_name, tag, filename, sep = "/")

  # Prepare AWS metadata (must be strings)
  aws_metadata <- list(
    name = dataset_name,
    tag = tag
  )

  # Add system metadata if provided
  system_fields <- c(
    "creation_date", "consent_expiry_date", "data_destruction_date",
    "sensitivity_level", "ethical_stamp"
  )
  for (field in system_fields) {
    if (!is.null(metadata[[field]])) {
      aws_metadata[[field]] <- as.character(metadata[[field]])
    }
  }

  # Add custom user metadata as JSON if provided
  user_metadata <- metadata[!names(metadata) %in% system_fields]
  if (length(user_metadata) > 0) {
    aws_metadata[["user_metadata_json"]] <- jsonlite::toJSON(user_metadata, auto_unbox = TRUE)
  }

  # Upload file with metadata
  s3_client <- paws.storage::s3(
    config = c(
      credentials,
      close_connection = TRUE
    )
  )

  tryCatch({
    logger::log_debug(paste("[tube::upload_file_to_public_datalake] uploading to key:", key))
    s3_client$put_object(
      Bucket = bucket,
      Key = key,
      Body = file_path,
      Metadata = aws_metadata,
      ContentType = "application/octet-stream; charset=utf-8"
    )
    logger::log_debug("[tube::upload_file_to_public_datalake] upload successful")
    TRUE
  }, error = function(e) {
    logger::log_error(paste("[tube::upload_file_to_public_datalake] upload failed:", e$message))
    cli::cli_alert_danger("Échec du téléchargement: {e$message}")
    FALSE
  })
}

#' Invoke AWS Lambda to index public datalake content
#' @description Trigger the lambda function to update the public-data-lake-content table
#' @param credentials AWS credentials from get_aws_credentials()
#' @return TRUE if lambda invocation successful, FALSE otherwise
#' @keywords internal
invoke_datalake_indexing_lambda <- function(credentials) {
  logger::log_debug("[tube::invoke_datalake_indexing_lambda] entering function")

  # Create lambda client
  lambda_client <- paws.compute::lambda(
    config = c(
      credentials,
      close_connection = TRUE
    )
  )

  tryCatch({
    # Find the correct lambda function using the generic finder
    lambda_name <- find_datalake_indexing_lambda(credentials)

    if (is.null(lambda_name)) {
      logger::log_warn("[tube::invoke_datalake_indexing_lambda] no matching lambda function found")
      cli::cli_alert_warning("⚠️ Fonction lambda d'indexation introuvable - indexation manuelle requise")
      return(FALSE)
    }

    logger::log_debug(paste("[tube::invoke_datalake_indexing_lambda] using lambda:", lambda_name))
    logger::log_debug("[tube::invoke_datalake_indexing_lambda] invoking lambda")

    result <- lambda_client$invoke(
      FunctionName = lambda_name,
      InvocationType = "Event"  # Async invocation
    )

    success <- result$StatusCode %in% c(200, 202)
    if (success) {
      logger::log_debug("[tube::invoke_datalake_indexing_lambda] lambda invocation successful")
      cli::cli_alert_success("Indexation des données déclenchée avec succès")
    } else {
      logger::log_error(paste(
        "[tube::invoke_datalake_indexing_lambda] lambda invocation failed, status:", result$StatusCode
      ))
      cli::cli_alert_warning("L'indexation a été déclenchée mais le statut est incertain")
    }

    success
  }, error = function(e) {
    logger::log_error(paste("[tube::invoke_datalake_indexing_lambda] lambda invocation error:", e$message))
    cli::cli_alert_danger("Échec du déclenchement de l'indexation: {e$message}")
    FALSE
  })
}
