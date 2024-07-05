is_csv_file <- function(filename) {
  # check the content of the file to ensure it is a valid CSV
  tryCatch({
    invisible(read.csv(filename))
    return(TRUE)
  }, error = function(e) {
    cli::cat_line("Error: ", e$message)
    return(FALSE)
  })
}

is_rtf_file <- function(filename) {
  # check the content of the file to ensure it is a valid RTF
  tryCatch({
    # Read the first few bytes of the file
    con <- file(filename, "rb")
    header <- readBin(con, what = "raw", n = 5)
    close(con)
  
    # Convert the raw bytes to a character string
    header_str <- rawToChar(header)
  
    # Check if the file starts with the RTF header
    if (startsWith(header_str, "{\\rtf")) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }, error = function(e) {
    cli::cat_line("Error: ", e$message)
    return(FALSE)
  })
}

file_ext <- function(filename) {
  # get the extension of a file
  return(tools::file_ext(filename))
}
