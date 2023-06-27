#' display the package version
#'
#' @param none
#'
#' @examples
#' \dontrun{
#'   v <- version()
#'   print(v)
#' }
#'
#' @export
version <- function() {
  return("0.0.1")
}


#' convert a url into a string
#'
#' @param url
#'
#' @examples
#' \dontrun{
#'   r <- convert_url_to_key(url)
#'   print(r)
#' }
#'
#' @export
convert_url_to_key <- function(url) {
  r <- gsub(" |-|:|/|\\.|&|\\?|=", "_", paste(format(Sys.time(), "%Y-%m-%d-%H-%M"), url, sep="_"))
  r <- gsub("https?___", "", r)
  return(r)
}
