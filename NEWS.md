# tube 0.5.5

* Implementation guardrails to prevent publish, unpublish and describe in datawarehouse

# tube 0.5.4

* Implementation of ellipse_process, used to update a data table with unprocessed data.  This is useful when ingesting data manually in Ellipse.

# tube 0.5.3

* Fix glue_client authentication parameter causing multi-env-database connections to be confused.

# tube 0.5.2

* Fix warning when `ellipse_discover` is called for a table that has no tags.

# tube 0.5.1

* Fix `ellipse_partitions` function which would break when `ellipse_discover` returned a list.

# tube 0.5.0
* Version 0.5.0 adds the table properties to set the foundations for the built-in datacatalog.  In other words, when we publish a table in the datamart, we can assign tags to this table in the form of a named list (tag_name = "tag_value").  Morover, we can also provide a description for this table.  In turn, the function `ellipse_discover` will render these tage and description which essentially enrich the data catalog of the CAPP.
* 0.5.1 will see an additional function to update the properties of a table only without having to publich or republish it.  Also in the dataplatform, a new property (table_description and table_tags) will be added to the aws_infra pipelines for tagging and describing datawarehouse tables also.
# tube 0.4.1
* Implementation of `ellipse_unpublish` to remove a table from a datamart.  When no table are left in a datamart, the datamart is automatically removed from the data platform.

# tube 0.4.0
* Implentation of the `ellipse_publish` function to publish a dataframe as a table in a datamart.  A datamart is a collection of one or many tables that are typically the result of refiners.  When publishing a dataframe into a datamart, if it does not already exist, the user will be asked if he wants to create it.  The same goes with the table to publish the data into.  Data in a dataframe to publish can be either added to an existing table of a datamart, or the existing table can be overwritten.

Currently `ellipse_publish`is only to be used intreactively by the user from within the R console.  In a subsequent version of this package, we will provide an unattended version of ir in order to support refiners meant to be fully automated.

Upon exit the `ellipse_publish` function prompts the user to process the data in order make it available (run the glue job) in the data platform through the `ellipse_query` functions immediately, or later according to the regular 6 hours schedule.

* This version features the possibility to work wirth multiple ellipse connections at the same time in the datawarehouse and in the datamarts, in both the DEV or the PROD environment of the data platform.

* This version also features some refactoring of non-exported functions and code readability improvements.

# tube 0.3.0
* Implementation of the `ellipse_ingest` function to inject factual and dimensional data or dictionaries into the data platform.

Essentially, the ingestion consists in uploading local files into the data platform for them to be processed and transformed into structured data tables in the data warehouse.  The process consists in providing either a local file name or a local folder name containing files to upload to the landing zone of the data platform.

In the background, the files are uploaded in an S3 bucket, inside a prefix named after the pipeline, under the DEFAULT partition.

During this process, a metadata tag is added to the file: either `version` for dimensional data or `batch` for factual data. This facilitates the subsequent loading of these data into R using a filter to retrieve them. It also allows for their specific identification. This metadata is found in a column of the data schema in the data warehouse.

Then, an extractor retrieves them from there to inject them into the data lake. This triggers an event that will run the loader, which will place the structured data into the data warehouse.

# tube 0.2.1

* Fix: Allow multiple connections in a given R session

# tube 0.2.0

* abstract all technical interface functions.  Only expose `ellipse_` functions
* you must use the `tube::ellipse_`functions to interact with the data platforme
* Add 2 placeholders for the functions to ingest data into the data platform

# tube 0.1.1

# tube 0.1.0

# tube 0.0.12

* `ellipse_connect()` can now connect to DEV or PROD environments
* `ellipse_connect()` now has a `database` parameter for Glue/Athena

# tube 0.0.11

# tube 0.0.10

# tube 0.0.9

# tube 0.0.8

# tube 0.0.7

# tube 0.0.6

# tube 0.0.5

# tube 0.0.4

# tube 0.0.3

# tube 0.0.2

# tube 0.0.1.9006

* Add `upload_to_landing_zone`

# tube 0.0.1.9005

* Add missing import for `logger`

# tube 0.0.1.9004

* Change behavior of `ellipse_partitions`

# tube 0.0.1.9003

* Add functions to help discovery
* Expand README

# tube 0.0.1.9002

* Add `simplify = TRUE` argument to `list_datawarehouse_tables` to enable human-readable output.
* Enable markdown processing for `roxygen2` comments.
* Fix imports
