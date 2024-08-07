% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3.R
\name{list_s3_buckets}
\alias{list_s3_buckets}
\title{List S3 buckets}
\usage{
list_s3_buckets(credentials, type)
}
\arguments{
\item{credentials}{A list of AWS credentials in the format compliant
with the paws package
#' @param type The type of bucket to list}
}
\value{
A list of S3 buckets
}
\description{
List all the S3 buckets that match the type
}
