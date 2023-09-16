#' @export 
aws_connect <- function(id, key) {
  r <- list(
    credentials = list(
      creds = list(
        access_key_id = id,
        secret_access_key = key
      )
    )
  )

  return(r)
}
