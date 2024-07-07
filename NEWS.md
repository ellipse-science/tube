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
