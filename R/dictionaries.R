#' @export
get_dictionary <- function(session, topic, lang = c("en","fr")) {
  logger::log_debug("[pumpr::get_dictionary] entering function")

  table <- get_datawarehouse_table(session, paste("dict_", topic, sep = ""))

  # Filter on language provided in lang if language is a dictionary feature
  if (!is.null(table$language)) {
    table <- table[table$language %in% lang, ]
  }

  return(table)
}