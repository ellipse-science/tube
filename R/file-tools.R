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
        cli::cli_alert_danger(paste(
          "Oups, le répertoire fourni contient des fichiers qui ne sont",
          "ni des fichiers CSV ni des fichiers RTF! 😅"
        ))
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

  folder_content
}

#' Read file based on extension with overflow handling
#' @param filepath Path to file
#' @param extension File extension
#' @keywords internal
read_file_by_extension <- function(filepath, extension) {
  ext <- tolower(extension)

  df <- switch(ext,
    "csv" = read_csv_with_overflow_handling(filepath),
    "dta" = haven::read_dta(filepath),
    "sav" = haven::read_sav(filepath),
    "rds" = readRDS(filepath),
    "rda" = read_rda_file(filepath),
    "xlsx" = readxl::read_excel(filepath),
    "xls" = readxl::read_excel(filepath),
    "dat" = read_dat_with_overflow_handling(filepath),
    stop("Format de fichier non supporté: ", ext)
  )

  # Convert to tibble for consistency
  tibble::as_tibble(df)
}

#' Read CSV file with overflow handling (extra values concatenated to last column)
#' @param filepath Path to CSV file
#' @keywords internal
read_csv_with_overflow_handling <- function(filepath) {
  # First, read normally to detect if there are overflow issues
  tryCatch(
    {
      df <- suppressWarnings(
        readr::read_csv(filepath, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
      )
      return(df)
    },
    error = function(e) {
      # If normal reading fails, try with overflow handling
      read_csv_with_manual_overflow(filepath)
    }
  )
}

#' Read DAT file with overflow handling
#' @param filepath Path to DAT file
#' @keywords internal
read_dat_with_overflow_handling <- function(filepath) {
  # Try to detect delimiter
  sample_lines <- readLines(filepath, n = 5)

  # Common delimiters to try
  delims <- c(",", "\t", ";", "|", " ")

  for (delim in delims) {
    tryCatch(
      {
        df <- suppressWarnings(
          readr::read_delim(filepath, delim = delim, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
        )
        if (ncol(df) > 1) {
          return(df)
        } # If we got multiple columns, probably right delimiter
      },
      error = function(e) {
        # Continue to next delimiter
      }
    )
  }

  # If all fail, treat as CSV
  read_csv_with_overflow_handling(filepath)
}

#' Read RDA file (load and return first object)
#' @param filepath Path to RDA file
#' @keywords internal
read_rda_file <- function(filepath) {
  env <- new.env()
  load(filepath, envir = env)
  objects <- ls(env)
  if (length(objects) == 0) {
    stop("Aucun objet trouvé dans le fichier RDA")
  }
  get(objects[1], envir = env)
}

#' Manual CSV reading with overflow concatenation
#' @param filepath Path to CSV file
#' @keywords internal
read_csv_with_manual_overflow <- function(filepath) {
  lines <- readLines(filepath)
  if (length(lines) == 0) {
    return(tibble::tibble())
  }

  # Get header from first line
  header_line <- lines[1]
  header <- strsplit(header_line, ",")[[1]]
  n_cols <- length(header)

  cli::cli_alert_warning("Détection de colonnes supplémentaires. Concaténation dans la dernière colonne.")

  # Process data lines
  data_rows <- list()

  for (i in 2:length(lines)) {
    if (nchar(trimws(lines[i])) == 0) next # Skip empty lines

    values <- strsplit(lines[i], ",")[[1]]

    if (length(values) > n_cols) {
      # Concatenate overflow values into last column
      overflow_values <- values[(n_cols + 1):length(values)]
      values[n_cols] <- paste(c(values[n_cols], overflow_values), collapse = ",")
      values <- values[1:n_cols]
    } else if (length(values) < n_cols) {
      # Pad with NAs if not enough values
      values <- c(values, rep(NA, n_cols - length(values)))
    }

    data_rows[[i - 1]] <- values
  }

  # Convert to dataframe
  df <- data.frame(matrix(unlist(data_rows), ncol = n_cols, byrow = TRUE), stringsAsFactors = FALSE)
  names(df) <- header

  tibble::as_tibble(df)
}
