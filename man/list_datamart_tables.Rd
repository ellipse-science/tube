% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datamart.R
\name{list_datamart_tables}
\alias{list_datamart_tables}
\title{Returns the datamarts GLUE tables names}
\usage{
list_datamart_tables(credentials, datamart_name, simplify = TRUE)
}
\arguments{
\item{credentials}{A list of AWS credentials in the format compliant
with the paws package}
}
\value{
If simplify = true it returns a tibble with columns:
\itemize{
\item \code{table_name} : Name of the table in the data warehouse
\item \code{col_name} : Name of the column
\item \code{col_type} : Data type of the column
\item \code{is_partition} : Logical indicating wether or not the column is
partitionned
If simplify = false it returns the raw output from the AWS Glue API
}
}
\description{
It is currently not used as the ellipse_discover function
does the job through the list_glue_tables function
We're keeping it for now just in case we need it later
}
