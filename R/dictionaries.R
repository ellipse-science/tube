#' @export
get_dictionary <- function(datawarehouse_name, topic, lang = c("en","fr"), credentials) {
    logger::log_debug("[pumpr::get_dictionary] entering function")

  # TODO: checkmate parameters validations and error handling

  logger::log_debug("[pumpr::get_dictionary] instanciating s3 client")
  s3_client <- paws.storage::s3(
    config = c(
      credentials, 
      close_connection = TRUE)
  )

  # Get dictionary file from datawarehouse
  file_key <- paste("dict_", topic, sep = "")
  file_key <- paste("_dictionaries", "/", file_key, ".csv", sep="")

  object <- s3_client$get_object( 
    Bucket = datawarehouse_name,
    Key = file_key
  )

  raw_to_char <- object$Body %>% rawToChar
  csv_text <- textConnection(raw_to_char)
  dict_df <- utils::read.csv2(csv_text, encoding = "UTF-8")

  # Filter on language provided in lang if language is a dictionary feature
  if (!is.null(dict_df$language)) {
    dict_df <- dict_df[dict_df$language %in% lang, ]

    # Remove language column
    dict_df$language <- NULL
  }

  dict_list <- list()
  for (c in unique(dict_df$category)) {
    dict_list[[c]] <- dict_df$item[dict_df$category == c]
  }

  # Convert dataframe to quanteda dict and return it
  qdict <- quanteda::dictionary(as.list(dict_list))
  return(qdict)
}