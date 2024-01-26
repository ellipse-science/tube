#' @export
get_dictionary <- function(session, topic, lang = c("en", "fr")) {
  logger::log_debug("[tube::get_dictionary] entering function")

  table <- get_datawarehouse_table(
    session,
    paste("dict-", topic, sep = ""),
    columns = list(),
    filter = list()
  )

  # Filter on language provided in lang if language is a dictionary feature
  if (!is.null(table$language)) {
    table <- table[table$language %in% lang, ]
  }

  logger::log_debug("[tube::get_dictionary] returning...")
  return(table)
}