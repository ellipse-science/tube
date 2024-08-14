#' Déterminer le type de colonne d'in dataframe
#' @param column La colonne à analyser
#' Les types de colonne supportés sont: string, int, decimal et date
#' @return Le type de colonne
get_column_type <- function(column) {
  if (is.character(column)) {
    return("character")
  } else if (inherits(column, "Date")) {
    return("date")
  } else if (is.numeric(column)) {
    if (all(column == as.integer(column))) {
      return("integer")
    } else {
      return("decimal")
    }
  } else {
    return(class(column))
  }
}
