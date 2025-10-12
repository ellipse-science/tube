# tube 0.8.0

## 🖼️ New Feature: Image File Support

### Image Upload and Download
* **NEW**: Support for PNG and JPEG image files in public datalake
* **NEW**: Seamless integration with existing `ellipse_push()` and `ellipse_query()` functions
* **NEW**: Automatic image file detection and specialized handling
* **NEW**: Interactive image viewer integration for R IDEs (RStudio/VSCode)
* **NEW**: Simplified metadata collection for images (no ethics/consent required)

### Enhanced File Format Support
* **EXTENDED**: File format validation now includes PNG, JPG, JPEG extensions
* **EXTENDED**: Content type mapping with proper MIME types for images
* **EXTENDED**: User interface messages updated to mention image support
* **NEW**: `display_image_file()` function with multiple fallback display methods

### Image-Specific Features
* **NEW**: `is_image_dataset()` helper function for automatic image detection  
* **NEW**: `handle_image_dataset()` for interactive image selection and display
* **NEW**: Streamlined metadata workflow - images skip ethical approval questions
* **NEW**: Support for magick, png, and jpeg packages for optimal image display

### Technical Improvements
* **DEPENDENCIES**: Added magick, png, jpeg as suggested packages
* **TESTING**: Comprehensive test suite for image functionality
* **DOCUMENTATION**: Updated function documentation with image examples
* **LINTING**: All code follows project linting standards

---

# tube 0.7.0

## 🔧 Code Quality & Development Infrastructure

### Enhanced Development Workflow
* **NEW**: Comprehensive linting integration with GitHub Actions
* **NEW**: Modular QA pipeline in `/tools/` directory with orchestration script
* **NEW**: Automated code style enforcement and quality checks
* **IMPROVED**: Package structure and documentation consistency

## 📋 Consolidated Changes from 0.6.2 and 0.6.3

### Enhanced Data Management (0.6.3)
* **FIXED**: Resolved all R CMD check issues for stable GitHub releases
* **FIXED**: Corrected accent encoding issues in French interface
* **DOCS**: Fixed all undocumented function parameters
* **DOCS**: Cleaned up development references from user documentation  
* **INFRASTRUCTURE**: Restored and improved GitHub Actions workflow
* **INFRASTRUCTURE**: Completed testing infrastructure cleanup

### Advanced File Deletion Control (0.6.2)
* **NEW**: `ellipse_unpush()` enhanced with granular `filename` parameter support
* **NEW**: Three-tier deletion system:
  - Full dataset deletion: `ellipse_unpush(con, "dataset")`
  - Tag-specific deletion: `ellipse_unpush(con, "dataset", tag = "v1")`
  - File-specific deletion: `ellipse_unpush(con, "dataset", tag = "v1", filename = "file.csv")`
* **FIXED**: S3 folder listing regex issue for root-level folders
* **FIXED**: User metadata JSON double-wrapping and field name compatibility
* **IMPROVED**: Enhanced `ellipse_push()` validation and error handling
* **IMPROVED**: Reordered metadata collection flow for better user experience

---

# tube 0.6.1 (BETA)

## 🆕 New Features

### Enhanced Data Deletion Control
* **NEW**: `ellipse_unpush()` now supports granular file deletion with `filename` parameter
* **NEW**: Three deletion modes available:
  - Full dataset deletion: `ellipse_unpush(con, "dataset")`
  - Tag-specific deletion: `ellipse_unpush(con, "dataset", tag = "v1")`
  - File-specific deletion: `ellipse_unpush(con, "dataset", tag = "v1", filename = "file.csv")`
* **NEW**: Parameter validation ensures `filename` requires `tag` to be specified
* **NEW**: File existence validation before deletion attempts
* **ENHANCED**: Improved user confirmation dialogs with specific deletion details

### XML File Support
* **NEW**: XML file format support for public datalake uploads
* **NEW**: Intelligent XML-to-tabular conversion with multiple parsing strategies
* **NEW**: Support for both simple repeating elements and complex nested XML structures
* **NEW**: Graceful error handling for malformed XML files
* **NEW**: Added `xml2` package dependency for robust XML processing

### File Format Support
* **ENHANCED**: File format detection now includes XML files
* **ENHANCED**: Updated all user-facing prompts to mention XML support
* **ENHANCED**: Content type mapping for XML files (`text/xml`)

## 🔧 Technical Improvements
* **IMPROVED**: Extended `read_file_by_extension()` function with XML parsing capability
* **IMPROVED**: Updated supported file extensions across all public datalake functions
* **IMPROVED**: Enhanced `ellipse_unpush()` with improved error handling and validation
* **IMPROVED**: Added comprehensive parameter validation for file deletion operations
* **IMPROVED**: Backward compatibility maintained for all existing `ellipse_unpush()` usage patterns

---

# tube 0.6.0 (BETA)

## 🚀 Major New Features

### Public Datalake Integration
* **NEW**: `ellipse_push()` - Upload data files to the public datalake with full metadata support
* **NEW**: `ellipse_query()` - Query and download data from the public datalake
* **NEW**: `ellipse_discover()` enhanced with public datalake dataset discovery
* **NEW**: Interactive file browser for `ellipse_push()` with intuitive navigation
* **NEW**: Comprehensive metadata collection including ethics, consent, and data governance

### Enhanced User Experience
* **NEW**: CLI-based progress bars for all file operations
* **NEW**: Smart file format detection (CSV, DTA, SAV, RDS, RDA, XLSX, XLS, DAT, XML)
* **NEW**: Automatic AWS Lambda indexing trigger for uploaded data
* **NEW**: Rich metadata formatting and display

### Data Governance & Compliance
* **NEW**: Built-in data sensitivity levels (1-5 scale)
* **NEW**: Consent expiry date tracking
* **NEW**: Data destruction date management
* **NEW**: Ethical approval stamp system
* **NEW**: Custom metadata fields support

## 🐛 Bug Fixes
* Fixed progress bar display issues across different terminal environments
* Improved error handling for AWS credential validation
* Enhanced file validation with better user feedback

## 📚 Documentation
* Added comprehensive [CONCEPTS.md](CONCEPTS.md) explaining platform architecture
* Updated README with public datalake usage examples
* Enhanced function documentation with real-world examples

## 🔧 Technical Improvements
* Unified error handling patterns across all functions
* Improved logging for debugging and monitoring
* Better AWS service integration and reliability

---

# tube 0.5.6

* Support for double data type in datamarts tables

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
