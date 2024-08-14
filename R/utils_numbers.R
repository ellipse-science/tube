#' Détecter si un nombre est un entier
#' @param x Le nombre à valider
#' @return true si le nombre est un entier, false sinon
is_integer <- function(x) {
  # Valider s'il s'agit d'un nombre d'abord
  if (!is.numeric(x)) {
    return(FALSE)
  }

  if (x == as.integer(x)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Détecter si un nombre est un nombre décimal
#' @param x Le nombre à valider
#' @return true si le nombre est un nombre décimal, false sinon
#' @examples
#' is_decimal(1.5)
#' is_decimal(1)
#' is_decimal(1L)
is_decimal <- function(x) {
  # valider s'il s'agit d'un nombre d'abord

  if (!is.numeric(x)) {
    return(FALSE)
  }

  if (is_integer(x)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
