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
  r <- gsub(" |-|:|/|\\.|&|\\?|=", "_", url)
  r <- gsub("https?___", "", r)
  r <- gsub("_$", "", r)
  return(r)
}

