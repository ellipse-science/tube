#' Fonction pour interagir avec l'utilisateur
#' @description Fonctions pour poser des questions à l'utilisateur à laquelle
#' il peut répondre par oui ou non
#' @param question La question à poser
#' @return La réponse de l'utilisateur
ask_yes_no <- function(question) {
  answer <- readline(prompt = paste(question, "(oui/non) "))
  return(invisible(tolower(answer) %in% c("oui", "o")))
}

#' Fonction pour interagir avec l'utilisateur
#' @description Fonctions pour poser des questions à l'utilisateur à laquelle
#' il peut répondre par 1 ou 2
#' @param question La question à poser
#' @return La réponse de l'utilisateur
ask_1_2 <- function(question) {
  while (TRUE) {
    answer <- readline(prompt = paste(question, "(1/2) "))
    if (answer %in% c("1", "2")) {
      return(invisible(answer))
    }
    cli::cli_alert_danger("Veuillez répondre par 1 ou 2.")
  }
}

suppress_console_output <- function(expr) {
  temp_file <- tempfile()
  temp_conn <- file(temp_file, open = "wt")
  
  sink(temp_conn, type = "output")
  sink(temp_conn, type = "message", append = TRUE)
  
  on.exit({
    sink(type = "message")
    sink(type = "output")
    close(temp_conn)
    file.remove(temp_file)
  }, add = TRUE)
  
  result <- tryCatch({
    force(expr)
  }, error = function(e) {
    stop(e) # Re-throw the caught error
  })
  
  result
}
