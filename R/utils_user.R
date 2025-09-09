#' Fonction pour interagir avec l'utilisateur
#' @description Fonctions pour poser des questions à l'utilisateur à laquelle
#' il peut répondre par oui ou non
#' @param question La question à poser
#' @param unattended_option Option pour mode non-interactif ("oui"/"non")
#' @return La réponse de l'utilisateur
ask_yes_no <- function(question, unattended_option = NULL) {
  if (!is.null(unattended_option)) {
    return(tolower(unattended_option) %in% c("oui", "o"))
  }
  answer <- readline(prompt = paste0(cli::symbol$fancy_question_mark, question, " (oui/non) "))
  return(invisible(tolower(answer) %in% c("oui", "o")))
}

#' Fonction pour interagir avec l'utilisateur
#' @description Fonctions pour poser des questions à l'utilisateur à laquelle
#' il peut répondre par 1 ou 2
#' @param question La question à poser
#' @param unattended_option Option pour mode non-interactif (doit être "1" ou "2")
#' @return La réponse de l'utilisateur
ask_1_2 <- function(question, unattended_option = NULL) {
  if (!is.null(unattended_option)) {
    if (unattended_option %in% c("1", "2")) {
      return(unattended_option)
    } else {
      stop("L'option unattended de ask_1_2 doit être 1 ou 2")
    }
  }
  while (TRUE) {
    answer <- readline(prompt = paste0(cli::symbol$fancy_question_mark, question, " (1/2) "))
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

  on.exit(
    {
      sink(type = "message")
      sink(type = "output")
      close(temp_conn)
      file.remove(temp_file)
    },
    add = TRUE
  )

  result <- tryCatch(
    {
      force(expr)
    },
    error = function(e) {
      stop(e) # Re-throw the caught error
    }
  )

  result
}


print_list_with_nulls <- function(lst) {
  for (name in names(lst)) {
    value <- lst[[name]]
    if (is.null(value)) {
      cli::cli_text(cli::col_cyan(paste0(name, ": ", "NULL")))
    } else {
      cli::cli_text(cli::col_cyan(paste0(name, ": ", value)))
    }
  }
}
