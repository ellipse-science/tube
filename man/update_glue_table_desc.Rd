% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glue.R
\name{update_glue_table_desc}
\alias{update_glue_table_desc}
\title{Update the description of a glue table}
\usage{
update_glue_table_desc(creds, schema, table, desc)
}
\arguments{
\item{schema}{The DBI schema to list the tables from}

\item{table}{The name of the table to change the description of}

\item{desc}{A string contaning the new description of the table}

\item{credentials}{A list of AWS credentials in the format compliant
with the paws package}
}
\value{
A boolean indicating wether or not the description was updated
successfully
}
\description{
Update the description of a glue table
}
\examples{
update_glue_table_desc(credentials, DBI::dbGetInfo(con)$dbms.name, "my_table", "new description of my_table")
}
