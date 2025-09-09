#' convert a url into a string
#'
#' @param url The URL to convert to a key string
#' @return A cleaned string suitable for use as a key
#'
#' @examples
#' \dontrun{
#' r <- convert_url_to_key(url)
#' print(r)
#' }
#'
#' @export
convert_url_to_key <- function(url) {
  r <- gsub(" |-|:|/|\\.|&|\\?|=", "_", url)
  r <- gsub("https?___", "", r)
  r <- gsub("_$", "", r)
  r
}
