is_csv_file <- function(filename) {
  # check the content of the file to ensure it is a valid CSV
  tryCatch(
    {
      invisible(read.csv(filename))
      TRUE
    },
    error = function(e) {
      cli::cat_line("Error: ", e$message)
      FALSE
    }
  )
}

is_rtf_file <- function(filename) {
  # check the content of the file to ensure it is a valid RTF
  tryCatch(
    {
      # Read the first few bytes of the file
      con <- file(filename, "rb")
      header <- readBin(con, what = "raw", n = 5)
      close(con)

      # Convert the raw bytes to a character string
      header_str <- rawToChar(header)

      # Check if the file starts with the RTF header
      if (startsWith(header_str, "{\\rtf")) {
        TRUE
      } else {
        FALSE
      }
    },
    error = function(e) {
      cli::cat_line("Error: ", e$message)
      FALSE
    }
  )
}

# Check if the file or folder provided is valid
# returns a list of files if the file or folder is valid
parse_landing_zone_input <- function(file_or_folder, folder_content) {
  if (file.exists(file_or_folder)) {
    if (file.info(file_or_folder)$isdir) {
      cli::cli_alert_info("Le chemin fourni est un répertoire.")

      folder_content <- list.files(file_or_folder, full.names = TRUE)

      # remove folders from this list
      folder_content <- folder_content[!file.info(folder_content)$isdir]

      # check that it's not empty
      if (length(folder_content) == 0) {
        cli::cli_alert_danger("Oups, le répertoire fourni est vide! 😅")
        return(NULL)
      }

      # check that the folder contains only one file type
      if (length(unique(tools::file_ext(folder_content))) > 1) {
        cli::cli_alert_danger("Oups, le répertoire fourni contient des fichiers de types différents! 😅")
        return(NULL)
      }

      # check that the folder contains only csv or rtf files
      if (!all(tools::file_ext(folder_content) %in% c("csv", "rtf"))) {
        cli::cli_alert_danger("Oups, le répertoire fourni contient des fichiers qui ne sont ni des fichiers CSV ni des fichiers RTF! 😅")
        return(NULL)
      }

      cli::cli_alert_info(paste("Validation de l'intégrité des données"))
      # check that the csv files are valid
      if (any(tools::file_ext(folder_content) == "csv")) {
        csv_files <- folder_content[tools::file_ext(folder_content) == "csv"]
        # Use pblapply instead of sapply to apply is_csv_file with a progress bar
        valid_csv_files <- unlist(pbapply::pblapply(csv_files, is_csv_file))
        if (!all(valid_csv_files)) {
          cli::cli_alert_danger("Oups, le répertoire fourni contient des fichiers CSV qui ne sont pas valides! 😅")
          return(NULL)
        }
      }

      # check that the rtf files are valid
      if (any(tools::file_ext(folder_content) == "rtf")) {
        rtf_files <- folder_content[tools::file_ext(folder_content) == "rtf"]
        # Use pblapply instead of sapply to apply is_rtf_file with a progress bar
        valid_rtf_files <- unlist(pbapply::pblapply(rtf_files, is_rtf_file))
        if (!all(valid_rtf_files)) {
          cli::cli_alert_danger("Oups, le répertoire fourni contient des fichiers RTF qui ne sont pas valides! 😅")
          return(NULL)
        }
      }

      cli::cli_alert_info(paste("Il y a", length(folder_content), "fichiers CSV ou RTF dans le répertoire fourni."))
    } else {
      cli::cli_alert_info("Le chemin fourni est un fichier.")
      folder_content <- list(file_or_folder)
      switch(tools::file_ext(file_or_folder),
        "csv" = {
          if (!is_csv_file(file_or_folder)) {
            cli::cli_alert_danger("Oups, le fichier fourni est un fichier CSV qui n'est pas valide! 😅")
            return(NULL)
          }
        },
        "rtf" = {
          if (!is_rtf_file(file_or_folder)) {
            cli::cli_alert_danger("Oups, le fichier fourni est un fichier RTF qui n'est pas valide! 😅")
            return(NULL)
          }
        },
        {
          cli::cli_alert_warning("Oups!  Seuls les fichiers CSV et RTF sont supportés par tube! 😅")
          return(NULL)
        }
      )
    }
  } else {
    cli::cli_alert_danger("Oups, le chemin fourni n'existe pas! 😅")
    return(NULL)
  }

  return(folder_content)
}
