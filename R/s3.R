
list_s3_buckets <- function(type, credentials) {
  logger::log_debug("[tube::list_s3_buckets] entering function")

  logger::log_debug("[tube::list_s3_buckets] instanciating s3 client")
  s3_client <- paws.storage::s3(
    config = c(
      credentials,
      close_connection = TRUE)
  )

  logger::log_debug("[tube::list_s3_buckets] listing buckets")
  r <- s3_client$list_buckets()

  #TODO: error management if no bucket is returned

  logger::log_debug("[tube::list_s3_buckets] wrangling result")
  list <- unlist(r$Buckets)
  bucket_list <- list[grep(type, list)]
  bucket_list <- as.list(bucket_list)
  names(bucket_list) <- ""
  bucket_list <- unlist(bucket_list)

  logger::log_debug("[tube::list_s3_buckets] returning results")
  return(bucket_list)
}

list_s3_partitions <- function(bucket, credentials) {
  logger::log_debug("[tube::list_s3_partitions] entering function")

  logger::log_debug("[tube::list_s3_partitions] instanciating s3 client")
  s3_client <- paws.storage::s3(
    config = c(
      credentials,
      close_connection = TRUE)
  )

  logger::log_debug("[tube::list_s3_partitions] listing partitions")
  r <- s3_client$list_objects_v2(
    Bucket = bucket,
    Delimiter = "/"
  )

  # Make a unnamed list of it
  logger::log_debug("[tube::list_s3_partitions] wrangling result")
  partition_list <- r$CommonPrefixes
  partition_list <- lapply(partition_list, function(x) x$Prefix)
  logger::log_debug("[tube::list_s3_partitions] returning results")
  return(partition_list)
}
