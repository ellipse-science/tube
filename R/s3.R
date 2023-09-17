


#' retreieves and returns an R object to an S3 bucket
#' @param PARM1
#' @param PARM2
#' @param PARM3
#' @param PARM4
#'
#' @examples
#' \dontrun{
#'   # put some sample code here as an example
#' }
#'
#' @export
get_r_object_from_datalake <- function(
                                  s3_client, 
                                  bucket, 
                                  prefix, 
                                  key, 
                                  history_version = "") {

  logger::log_debug("[pumpr::get_r_object_from_datalake] entering function")
  logger::log_info(
    paste(
      "[pumpr::get_r_object_from_datalake] retrieving object",
      paste(prefix, key, sep="/"),
      "from bucket",
      bucket
    )
  )

  # TODO: checkmate parameters validations and error handling

  # put the object in s3 bucket 
  # TODO: mettre la gestion des erreur autour de ce code
  object <- s3_client$get_object( 
    Bucket = bucket,
    Key = paste(prefix, paste(key,".json",sep=""), sep="/")
  )

  logger::log_debug("[pumpr::get_r_object_from_datalake] exiting function and returning rawToChar object")

  raw_to_char <- object$Body %>%  rawToChar
  json_to_list <- jsonlite::fromJSON(raw_to_char)

  return(json_to_list)

  #TODO : Error management
}







#' retrieves a list and returns a dataframe of R objects from an S3 bucket through GLUE
#' @param PARM1
#' @param PARM2
#' @param PARM3
#' @param PARM4
#'
#' @examples
#' \dontrun{
#'   # put some sample code here as an example
#' }
#'
#' @export



#' retrieves a list and returns a dataframe of R objects from an S3 bucket through GLUE
#' @param PARM1
#' @param PARM2
#' @param PARM3
#' @param PARM4
#'
#' @examples
#' \dontrun{
#'   # put some sample code here as an example
#' }
#'
#' @export
commit_dataframe_to_datawarehouse <- function(
                                          datawarehouse,
                                          dataframe,
                                          prefix,
                                          key,
                                          pipeline_handler = "local") {
  
  checkmate::assert_choice(datawarehouse$type, c("bucket"))

  if (datawarehouse$type == "bucket") {
    if (pipeline_handler == "lambda") {
      s3_client <- paws.storage::s3()
    } else {
      s3_client <<- paws.storage::s3(
        config = list(
          credentials = list(creds = datawarehouse$credentials),
          region = Sys.getenv("AWS_REGION")
        )
      )
    }

    td <- tempdir()
    filename <- tempfile()

    arrow::write_parquet(dataframe, filename)

    s3_client$put_object(
      Bucket = datawarehouse$name,
      Body = filename,
      Key = paste(prefix, paste(key, ".parquet", sep=""), sep="/")
    )  
  }

}



#' retrieves a list and returns a dataframe of R objects from an S3 bucket through GLUE
#' @param PARM1
#' @param PARM2
#' @param PARM3
#' @param PARM4
#'
#' @examples
#' \dontrun{
#'   # put some sample code here as an example
#' }
#'
#' @export
get_dataset <- function(
                                          datawarehouse,
                                          prefix,
                                          pipeline_handler = "local") {
  
  checkmate::assert_choice(datawarehouse$type, c("bucket"))

  if (datawarehouse$type == "bucket") {
    if (pipeline_handler == "lambda") {
      s3_client <- paws.storage::s3()
    } else {
      s3_client <<- paws.storage::s3(
        config = list(
          credentials = list(
            creds = datawarehouse$credentials
          ),
          region = Sys.getenv("AWS_REGION")
        )
      )
    }

    s3_object <- s3_client$get_object(
      Bucket = datawarehouse$name, 
      Key = paste(prefix, filename, sep="/")
    )

    df <- arrow::read_parquet(s3_object$Body)

    return(df)  
  }

}