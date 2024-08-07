% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aws.R
\name{get_aws_credentials}
\alias{get_aws_credentials}
\title{Returns the AWS credentials in a format that is
compliant with the paws functions calls}
\usage{
get_aws_credentials(env)
}
\arguments{
\item{env}{The environnement ("DEV" or "PROD")}
}
\value{
a list structure compliant with the paws functions calls containing the AWS credentials
for the specified environment
}
\description{
It is used to get the credentials from the environment variables
set in ~/.Renviron in the following format:
}
\details{
#dev
AWS_ACCESS_KEY_ID_DEV=\if{html}{\out{<the access key id for the DEV account>}}
AWS_SECRET_ACCESS_KEY_DEV=\if{html}{\out{<the secret access key for the DEV account >}}
#prod
AWS_ACCESS_KEY_ID_PROD=\if{html}{\out{<the access key id for the PROD account>}}
AWS_SECRET_ACCESS_KEY_PROD=\if{html}{\out{<the access key id for the PROD account>}}

The function checks if the credentials are valid by trying to list the buckets
in the account. If the credentials are not valid, the function returns NULL
and displays an error message.

The value returned must be passed to all functions that use paws functions
}
