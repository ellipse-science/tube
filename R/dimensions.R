#' @export
get_dimension <- function(session, dimension, filter = "") {
  logger::log_debug("[tube::get_dimention] entering function")

  table <- get_datawarehouse_table(
    session,
    paste("dim-", dimension, sep = ""),
    columns = list(),
    filter = filter
  )

  return(table)
}


# TODO: create the get_parliament_members, get_parties, etc. functions
