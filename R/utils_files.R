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

        # Initialize progress bar for CSV validation
        cli::cli_progress_bar(
          name = "Validation des fichiers CSV",
          total = length(csv_files),
          format = "{cli::pb_name} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
          clear = TRUE
        )

        # Apply is_csv_file with CLI progress bar
        valid_csv_files <- logical(length(csv_files))
        for (i in seq_along(csv_files)) {
          valid_csv_files[i] <- is_csv_file(csv_files[i])
          cli::cli_progress_update()
        }

        if (!all(valid_csv_files)) {
          cli::cli_alert_danger("Oups, le répertoire fourni contient des fichiers CSV qui ne sont pas valides! 😅")
          return(NULL)
        }
      }

      # check that the rtf files are valid
      if (any(tools::file_ext(folder_content) == "rtf")) {
        rtf_files <- folder_content[tools::file_ext(folder_content) == "rtf"]

        # Initialize progress bar for RTF validation
        cli::cli_progress_bar(
          name = "Validation des fichiers RTF",
          total = length(rtf_files),
          format = "{cli::pb_name} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
          clear = TRUE
        )

        # Apply is_rtf_file with CLI progress bar
        valid_rtf_files <- logical(length(rtf_files))
        for (i in seq_along(rtf_files)) {
          valid_rtf_files[i] <- is_rtf_file(rtf_files[i])
          cli::cli_progress_update()
        }

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

  # Suppress messages from file reading functions
  df <- suppressWarnings(suppress_console_output({
    switch(ext,
      "csv" = read_csv_with_overflow_handling(filepath),
      "dta" = haven::read_dta(filepath),
      "sav" = haven::read_sav(filepath),
      "rds" = readRDS(filepath),
      "rda" = read_rda_file(filepath),
      "xlsx" = readxl::read_excel(filepath),
      "xls" = readxl::read_excel(filepath),
      "dat" = read_dat_with_overflow_handling(filepath),
      "xml" = read_xml_as_tabular(filepath),
      stop("Format de fichier non supporté: ", ext)
    )
  }))

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
      df <- readr::read_csv(filepath, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
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
  # Try to detect delimiter by reading sample lines
  readLines(filepath, n = 5)

  # Common delimiters to try
  delims <- c(",", "\t", ";", "|", " ")

  for (delim in delims) {
    tryCatch(
      {
        df <- readr::read_delim(
          filepath,
          delim = delim, col_types = readr::cols(.default = "c"), show_col_types = FALSE
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

#' Read XML file and convert to tabular format intelligently
#' @param filepath Path to XML file
#' @keywords internal
read_xml_as_tabular <- function(filepath) {
  tryCatch({
    # Read XML document
    xml_doc <- xml2::read_xml(filepath)
    
    # Strategy 1: Look for repeating elements that could form rows
    tabular_data <- extract_xml_tabular_data(xml_doc)
    
    if (!is.null(tabular_data) && nrow(tabular_data) > 0) {
      return(tabular_data)
    }
    
    # Strategy 2: If no clear tabular structure, flatten the XML
    flattened_data <- flatten_xml_to_tabular(xml_doc)
    
    return(flattened_data)
    
  }, error = function(e) {
    # Fallback: create a single-row dataframe with raw XML content
    cli::cli_alert_warning("XML parsing failed, storing as raw content: {e$message}")
    
    raw_content <- tryCatch({
      readLines(filepath, warn = FALSE)
    }, error = function(e2) {
      "Error reading XML file"
    })
    
    tibble::tibble(
      xml_content = paste(raw_content, collapse = "\n"),
      parse_error = e$message
    )
  })
}

#' Extract tabular data from XML by finding repeating elements
#' @param xml_doc XML document object
#' @keywords internal
extract_xml_tabular_data <- function(xml_doc) {
  # Find all element names in the document
  all_nodes <- xml2::xml_find_all(xml_doc, "//*")
  element_names <- xml2::xml_name(all_nodes)
  
  # Count occurrences of each element name
  element_counts <- table(element_names)
  
  # Look for elements that appear multiple times (potential rows)
  repeated_elements <- names(element_counts[element_counts > 1])
  
  if (length(repeated_elements) == 0) {
    return(NULL)
  }
  
  # Try each repeated element as potential row data
  for (element_name in repeated_elements) {
    xpath <- paste0("//", element_name)
    elements <- xml2::xml_find_all(xml_doc, xpath)
    
    if (length(elements) < 2) next  # Need at least 2 rows
    
    # Try to convert these elements to tabular data
    row_data <- extract_rows_from_elements(elements)
    
    if (!is.null(row_data) && nrow(row_data) > 1) {
      # Add metadata about the source element
      row_data$xml_source_element <- element_name
      return(row_data)
    }
  }
  
  NULL
}

#' Extract rows from XML elements
#' @param elements XML nodes that represent potential rows
#' @keywords internal
extract_rows_from_elements <- function(elements) {
  tryCatch({
    # Extract data from each element
    rows <- lapply(elements, function(element) {
      # Get all child elements and their values
      children <- xml2::xml_children(element)
      
      if (length(children) == 0) {
        # If no children, use the element's text content
        element_text <- xml2::xml_text(element)
        if (nchar(trimws(element_text)) > 0) {
          return(list(content = element_text))
        } else {
          return(NULL)
        }
      }
      
      # Create a named list from child elements
      row_data <- list()
      for (child in children) {
        child_name <- xml2::xml_name(child)
        child_value <- xml2::xml_text(child)
        
        # Handle duplicate column names by adding suffix
        if (child_name %in% names(row_data)) {
          counter <- 1
          while (paste0(child_name, "_", counter) %in% names(row_data)) {
            counter <- counter + 1
          }
          child_name <- paste0(child_name, "_", counter)
        }
        
        row_data[[child_name]] <- child_value
      }
      
      row_data
    })
    
    # Filter out NULL rows
    rows <- rows[!sapply(rows, is.null)]
    
    if (length(rows) == 0) {
      return(NULL)
    }
    
    # Get all unique column names
    all_columns <- unique(unlist(lapply(rows, names)))
    
    # Ensure all rows have the same columns (fill missing with NA)
    standardized_rows <- lapply(rows, function(row) {
      missing_cols <- setdiff(all_columns, names(row))
      if (length(missing_cols) > 0) {
        row[missing_cols] <- NA
      }
      row[all_columns]  # Reorder columns consistently
    })
    
    # Convert to data frame
    df <- data.frame(
      do.call(rbind, lapply(standardized_rows, function(x) as.data.frame(x, stringsAsFactors = FALSE))),
      stringsAsFactors = FALSE
    )
    
    tibble::as_tibble(df)
    
  }, error = function(e) {
    NULL
  })
}

#' Flatten XML to tabular format when no clear row structure exists
#' @param xml_doc XML document object
#' @keywords internal
flatten_xml_to_tabular <- function(xml_doc) {
  tryCatch({
    # Get root element
    root <- xml2::xml_root(xml_doc)
    
    # Extract all text nodes with their paths
    all_nodes <- xml2::xml_find_all(xml_doc, "//*[text()]")
    
    if (length(all_nodes) == 0) {
      # No text content found
      return(tibble::tibble(
        xml_structure = "No text content found",
        root_element = xml2::xml_name(root)
      ))
    }
    
    # Create a flattened representation
    flattened_data <- data.frame(
      element_path = character(length(all_nodes)),
      element_name = character(length(all_nodes)),
      element_value = character(length(all_nodes)),
      stringsAsFactors = FALSE
    )
    
    for (i in seq_along(all_nodes)) {
      node <- all_nodes[[i]]
      flattened_data$element_name[i] <- xml2::xml_name(node)
      flattened_data$element_value[i] <- xml2::xml_text(node)
      
      # Create a simple path representation
      path_elements <- character()
      current <- node
      while (!is.null(current) && xml2::xml_name(current) != xml2::xml_name(root)) {
        path_elements <- c(xml2::xml_name(current), path_elements)
        current <- xml2::xml_parent(current)
      }
      flattened_data$element_path[i] <- paste(c(xml2::xml_name(root), path_elements), collapse = "/")
    }
    
    return(tibble::as_tibble(flattened_data))
    
  }, error = function(e) {
    # Ultimate fallback
    tibble::tibble(
      error = "XML flattening failed",
      error_message = e$message
    )
  })
}

#' Display an image file using R's built-in viewer capabilities
#' @param filepath Path to image file
#' @keywords internal
display_image_file <- function(filepath) {
  if (!file.exists(filepath)) {
    stop("Image file not found: ", filepath, call. = FALSE)
  }
  
  # Get file extension
  ext <- tolower(tools::file_ext(filepath))
  
  # Validate it's an image
  if (!ext %in% c("png", "jpg", "jpeg")) {
    stop("Unsupported image format: ", ext, call. = FALSE)
  }
  
  tryCatch({
    # Check if we're in RStudio first
    is_rstudio <- exists(".rs.invokeShinyPaneViewer") || Sys.getenv("RSTUDIO") == "1"
    
    if (is_rstudio && requireNamespace("magick", quietly = TRUE)) {
      # Method 1: RStudio with magick - use viewer pane
      img <- magick::image_read(filepath)
      print(img)  # Auto-displays in RStudio viewer
      return(invisible(img))
    } else {
      # Method 2: VS Code or other IDEs - try multiple approaches
      cli::cli_alert_info("Ouverture de l'image avec le viewer système...")
      cli::cli_alert_info("Fichier: {filepath}")
      
      success <- FALSE
      
      if (.Platform$OS.type == "windows") {
        system_result <- system(paste("start", shQuote(filepath)), wait = FALSE)
        success <- (system_result == 0)
      } else {
        # Try multiple Linux approaches in order of preference

        # 1. Try VS Code directly
        if (Sys.which("code") != "" && !success) {
          cli::cli_alert_info("Tentative d'ouverture avec VS Code...")
          system_result <- system(paste("code", shQuote(filepath)), wait = FALSE,
            ignore.stdout = TRUE, ignore.stderr = TRUE)
          if (system_result == 0) {
            success <- TRUE
            cli::cli_alert_success("✅ Image ouverte dans VS Code")
          }
        }
        
        # 2. Try eog (Eyes of GNOME)
        if (Sys.which("eog") != "" && !success) {
          cli::cli_alert_info("Tentative d'ouverture avec eog...")
          system_result <- system(paste("eog", shQuote(filepath), "2>/dev/null &"), wait = FALSE)
          if (system_result == 0) {
            success <- TRUE
            cli::cli_alert_success("✅ Image ouverte avec eog")
          }
        }
        
        # 3. Try feh
        if (Sys.which("feh") != "" && !success) {
          cli::cli_alert_info("Tentative d'ouverture avec feh...")
          system_result <- system(paste("feh", shQuote(filepath), "2>/dev/null &"), wait = FALSE)
          if (system_result == 0) {
            success <- TRUE
            cli::cli_alert_success("✅ Image ouverte avec feh")
          }
        }
        
        # 4. Try ImageMagick display
        if (Sys.which("display") != "" && !success) {
          cli::cli_alert_info("Tentative d'ouverture avec ImageMagick display...")
          system_result <- system(paste("display", shQuote(filepath), "2>/dev/null &"), wait = FALSE)
          if (system_result == 0) {
            success <- TRUE
            cli::cli_alert_success("✅ Image ouverte avec ImageMagick display")
          }
        }
        
        # 5. Fallback to xdg-open
        if (!success) {
          cli::cli_alert_info("Tentative d'ouverture avec xdg-open...")
          system_result <- system(paste("xdg-open", shQuote(filepath)), wait = FALSE)
          if (system_result == 0) {
            success <- TRUE
            cli::cli_alert_success("✅ Image ouverte avec xdg-open")
          }
        }
      }
      
      if (!success) {
        cli::cli_alert_warning("Impossible d'ouvrir l'image automatiquement.")
        cli::cli_alert_info("📁 Fichier disponible à: {filepath}")
        cli::cli_alert_info("💡 Vous pouvez l'ouvrir manuellement avec VS Code ou votre viewer d'images préféré")
      }
      
      return(invisible(filepath))
    }
    
  }, error = function(e) {
    cli::cli_alert_danger("Impossible d'afficher l'image: {e$message}")
    cli::cli_alert_info("Fichier image disponible à: {filepath}")
    invisible(filepath)
  })
}

#' Display an HTML file by opening it in a web browser
#'
#' Opens an HTML file in the default web browser with multiple fallback options for
#' Linux/Mac/Windows. Tries multiple browser opening methods in order of preference:
#' - Linux: xdg-open, firefox, chrome/chromium
#' - Mac: open command
#' - Windows: start command
#'
#' Temp files should persist as browser needs time to load them.
#'
#' @param filepath Path to HTML file to display
#' @return Invisibly returns the filepath
#' @keywords internal
#' @seealso \code{\link{display_image_file}} for image display
display_html_file <- function(filepath) {
  if (!file.exists(filepath)) {
    stop("HTML file not found: ", filepath, call. = FALSE)
  }
  
  # Get file extension
  ext <- tolower(tools::file_ext(filepath))
  
  # Validate it's HTML
  if (!ext %in% c("html", "htm")) {
    stop("Unsupported file format: ", ext, ". Expected HTML.", call. = FALSE)
  }
  
  tryCatch({
    cli::cli_alert_info("Ouverture du fichier HTML...")
    cli::cli_alert_info("Fichier: {filepath}")
    
    success <- FALSE
    
    # Check if we're in RStudio first - it has the best HTML viewing support
    is_rstudio <- exists(".rs.invokeShinyPaneViewer") || Sys.getenv("RSTUDIO") == "1"
    
    if (is_rstudio && requireNamespace("rstudioapi", quietly = TRUE)) {
      # RStudio: use viewer pane - works for both local and RStudio Server
      tryCatch({
        rstudioapi::viewer(filepath)
        success <- TRUE
        cli::cli_alert_success("✅ HTML ouvert dans le Viewer RStudio")
        return(invisible(filepath))
      }, error = function(e) {
        cli::cli_alert_warning("Échec du viewer RStudio: {e$message}")
      })
    }
    
    # Detect VS Code Remote environment
    vscode_browser <- Sys.getenv("BROWSER")
    in_vscode_remote <- nzchar(Sys.getenv("VSCODE_IPC_HOOK_CLI")) && 
                        grepl("browser.sh", vscode_browser, fixed = TRUE)
    
    # For VS Code Remote: use VS Code's browser helper script
    if (!success && in_vscode_remote && nzchar(vscode_browser)) {
      cli::cli_alert_info("Détection de VS Code Remote - tentative d'ouverture...")
      tryCatch({
        # Use VS Code's browser helper which properly forwards to client
        file_url <- paste0("file://", normalizePath(filepath, winslash = "/"))
        system_result <- system2(vscode_browser, args = c(file_url), wait = FALSE, 
                                 stdout = FALSE, stderr = FALSE)
        if (system_result == 0 || is.null(system_result)) {
          success <- TRUE
          cli::cli_alert_success("✅ HTML envoyé au client")
        }
      }, error = function(e) {
        cli::cli_alert_warning("Échec du helper VS Code: {e$message}")
      })
    }
    
    # For local environments: use system commands
    if (!success && .Platform$OS.type == "windows") {
      # Windows: use start command
      system_result <- system(paste("start", shQuote(filepath)), wait = FALSE)
      success <- (system_result == 0)
      if (success) {
        cli::cli_alert_success("✅ HTML ouvert dans le navigateur par défaut")
      }
    } else if (!success) {
      # Linux/Mac: try multiple browser opening approaches
      
      # 1. Try xdg-open (standard Linux)
      if (Sys.which("xdg-open") != "" && !success) {
        cli::cli_alert_info("Tentative d'ouverture avec xdg-open...")
        system_result <- system(paste("xdg-open", shQuote(filepath), "2>/dev/null"), wait = FALSE)
        if (system_result == 0) {
          success <- TRUE
          cli::cli_alert_success("✅ HTML ouvert dans le navigateur par défaut")
        }
      }
      
      # 2. Try Mac open
      if (Sys.which("open") != "" && !success) {
        cli::cli_alert_info("Tentative d'ouverture avec open (Mac)...")
        system_result <- system(paste("open", shQuote(filepath)), wait = FALSE)
        if (system_result == 0) {
          success <- TRUE
          cli::cli_alert_success("✅ HTML ouvert avec open")
        }
      }
      
      # 3. Try firefox
      if (Sys.which("firefox") != "" && !success) {
        cli::cli_alert_info("Tentative d'ouverture avec Firefox...")
        system_result <- system(paste("firefox", shQuote(filepath), "2>/dev/null"), wait = FALSE)
        if (system_result == 0) {
          success <- TRUE
          cli::cli_alert_success("✅ HTML ouvert dans Firefox")
        }
      }
      
      # 4. Try chromium/chrome
      chrome_browsers <- c("chromium-browser", "chromium", "google-chrome", "chrome")
      for (browser in chrome_browsers) {
        if (Sys.which(browser) != "" && !success) {
          cli::cli_alert_info("Tentative d'ouverture avec {browser}...")
          system_result <- system(paste(browser, shQuote(filepath), "2>/dev/null"), wait = FALSE)
          if (system_result == 0) {
            success <- TRUE
            cli::cli_alert_success("✅ HTML ouvert dans {browser}")
            break
          }
        }
      }
    }
    
    if (!success) {
      cli::cli_alert_warning("Impossible d'ouvrir le fichier HTML automatiquement.")
      cli::cli_alert_info("📁 Fichier disponible à: {filepath}")
      cli::cli_alert_info("💡 Vous pouvez l'ouvrir manuellement dans votre navigateur")
    }

    invisible(filepath)

  }, error = function(e) {
    cli::cli_alert_danger("Impossible d'afficher le fichier HTML: {e$message}")
    cli::cli_alert_info("Fichier HTML disponible à: {filepath}")
    invisible(filepath)
  })
}
