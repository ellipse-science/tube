#' Déterminer le type de colonne d'in dataframe
#' @param column La colonne à analyser
#' Les types de colonne supportés sont: string, int, decimal et date
#' @return Le type de colonne
#get_column_type <- function(column) {
#  # Remove NA values temporarily
#  non_na_column <- na.omit(column)
#
#  if (is.character(non_na_column)) {
#    return("character")
#  } else if (inherits(non_na_column, "Date")) {
#    return("date")
#  } else if (is.numeric(non_na_column)) {
#    if (all(non_na_column == as.integer(non_na_column))) {
#      return("integer")
#    } else {
#      return("decimal")
#    }
#  } else {
#    return("unsupported")
#  }
#}

get_column_type <- function(column) {
  # Remove NA values temporarily
  non_na_column <- na.omit(column)
  if (is.integer(non_na_column)) {
    "integer"
  } else if (inherits(non_na_column, "Date")) {
    "date"
  } else if (is.double(non_na_column)) {
    "decimal"
  } else if (is.character(non_na_column)) {
    "character"
  } else {
    "unsupported"
  }
}