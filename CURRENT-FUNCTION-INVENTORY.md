# Current Function Inventory - Post Feature 007, Pre Feature 008

## 📊 COMPREHENSIVE FUNCTION MAPPING

### Core Status
- **Feature 007**: ✅ Complete - Public Datalake Discovery
- **Feature 008**: 🎯 Ready - Public Datalake Query Implementation  
- **Current Branch**: `feature/008-public-datalake-query`

### Feature 008 Target: ellipse_query() Extension

#### Current Implementation (R/ellipse.R lines 362-390)
```r
ellipse_query <- function(con, table) {
  schema_name <- DBI::dbGetInfo(con)$dbms.name
  tables <- DBI::dbGetQuery(con, paste0("SHOW TABLES IN ", schema_name))$tab_name
  
  if (!table %in% tables) {
    cli::cli_alert_danger("La table demandée est inconnue.")
    return(NULL)
  }
  
  return(dplyr::tbl(con, table))
}
```

#### Extension Required for Feature 008
1. **Detect Query Type**: Table name vs SQL query
2. **SQL Query Support**: For public datalake connections  
3. **Backward Compatibility**: Existing behavior for datawarehouse/datamarts
4. **Error Handling**: Comprehensive validation and user-friendly messages

### Public Datalake Functions (NEW - Feature 007)

#### Infrastructure Functions
```r
# R/public-datalake.R
list_public_datalake_bucket()    # List S3 objects in publicdatalakebucket
list_public_datalake_database()  # List Glue tables in public datalake database
```

#### Discovery Utility Functions (Internal)
```r
# R/utils_public_datalake_discovery.R
format_public_datalake_all_datasets()      # Format all datasets overview
format_public_datalake_pattern_search()    # Format pattern search results
format_public_datalake_dataset_details()   # Format specific dataset details
format_public_datalake_tag_details()       # Format specific tag details
```

### Extended Functions (Modified in Feature 007)

#### Core Ellipse Functions
```r
# R/ellipse.R
ellipse_connect(env, database)    # EXTENDED: Now supports database = "datalake"
ellipse_discover(con, object, tag) # EXTENDED: Added tag parameter + public datalake logic
```

#### Validation Functions
```r
# R/utils_check_params.R
check_database(database)  # EXTENDED: Now accepts "datawarehouse", "datamarts", "datalake"
```

### R/datalake.R - Data Lake Operations
- `list_datalake_bucket()` - List data lake storage buckets
- Data lake management functions
- Raw data storage operations

### R/datamart.R - Data Mart Operations  
- `list_datamarts_bucket()` - List data mart buckets
- `list_datamarts_database()` - List data mart databases
- Data mart table and schema management
- Business-ready data access functions

### R/datawarehouse.R - Data Warehouse Operations
- `list_datawarehouse_bucket()` - List data warehouse buckets  
- `list_datawarehouse_database()` - List data warehouse databases
- `list_datawarehouse_tables()` - List tables in data warehouse
- Structured data storage functions
- Enterprise reporting data management

### R/ellipse.R - Ellipse Platform Integration
High-level workflow orchestration functions:
- `ellipse_connect()` - Platform connection management
- `ellipse_disconnect()` - Clean disconnection
- `ellipse_discover()` - Resource discovery
- `ellipse_describe()` - Resource description
- `ellipse_ingest()` - Data ingestion workflows
- `ellipse_process()` - Data processing workflows
- `ellipse_publish()` - Data publishing workflows
- `ellipse_unpublish()` - Remove published data
- `ellipse_query()` - Query operations
- `ellipse_partitions()` - Partition management

### R/file-tools.R - File Manipulation Utilities
- File validation functions
- File format detection
- File processing utilities
- Cross-platform file operations

### R/glue.R - AWS Glue Integration
Data catalog and ETL management:
- `list_glue_databases()` - List Glue catalog databases
- `list_glue_tables()` - List tables in Glue catalog
- `list_glue_jobs()` - List ETL jobs
- `list_glue_table_properties()` - Table metadata
- `run_glue_job()` - Execute ETL jobs
- `update_glue_table_desc()` - Update table descriptions
- `update_glue_table_tags()` - Manage table tags
- `delete_glue_table()` - Remove tables from catalog
- `get_column_type()` - Column type information
- `glue_table_list_to_tibble()` - Data transformation utilities

### R/landing.R - Landing Zone Operations
Initial data ingestion and staging:
- `list_landing_zone_bucket()` - List landing zone buckets
- `upload_file_to_landing_zone()` - Upload files to landing zone
- Landing zone management functions
- Initial data validation and staging

### R/s3.R - Core S3 Operations
Cross-platform S3 functionality:
- `list_s3_buckets()` - List S3 buckets by type
- `list_s3_folders()` - List folders in S3 buckets  
- `list_s3_partitions()` - List S3 partitions
- `upload_file_to_s3()` - Generic S3 file upload
- `delete_s3_folder()` - Remove S3 folders
- S3 bucket and object management

### R/utils_check_params.R - Parameter Validation
Input validation and error checking:
- `check_env()` - Environment validation
- `check_database()` - Database connection validation  
- `check_params_before_*()` - Various parameter validation functions
- `check_file_versioning_before_ingest()` - File validation
- `check_pipeline_before_ingest()` - Pipeline validation

### R/utils_data.R - Data Utilities
- Data transformation functions
- Data structure utilities
- Data validation helpers

### R/utils_url.R - URL Management
- `convert_url_to_key()` - URL to S3 key conversion
- URL parsing and manipulation
- Key generation utilities

### R/utils_user.R - User Interface Utilities
User interaction and feedback:
- `ask_yes_no()` - Interactive yes/no prompts
- `ask_1_2()` - Interactive choice prompts  
- User confirmation functions

### R/utils-pipe.R - Pipe Operations
- Pipe operator implementation
- Workflow chaining utilities

---

## CONCEPTUAL ANALYSIS

### Data Flow Architecture Pattern:
```
LANDING ZONE (Initial Ingestion)
    ↓
DATA LAKE (Raw Storage)
    ↓
DATA WAREHOUSE (Structured Storage)
    ↓
DATA MARTS (Business-Ready Views)
```

### Current Naming Patterns:

#### Bucket Operations:
- `list_landing_zone_bucket()` - Landing zone
- `list_datalake_bucket()` - Data lake
- `list_datawarehouse_bucket()` - Data warehouse  
- `list_datamarts_bucket()` - Data marts

#### Database Operations:
- `list_datawarehouse_database()` - Data warehouse
- `list_datamarts_database()` - Data marts
- `list_glue_databases()` - Glue catalog

#### Table Operations:
- `list_datawarehouse_tables()` - Data warehouse tables
- `list_glue_tables()` - Glue catalog tables

#### Upload Operations:
- `upload_file_to_landing_zone()` - Landing zone specific
- `upload_file_to_s3()` - Generic S3 upload

### Conceptual Inconsistencies to Address:

1. **Naming Convention Variations**:
   - "landing_zone" vs "landing" 
   - "datawarehouse" vs "data_warehouse"
   - "datamarts" vs "data_marts"

2. **Function Scope Overlaps**:
   - S3 functions vs zone-specific functions
   - Generic vs specialized operations

3. **Abstraction Levels**:
   - Low-level AWS operations
   - High-level Ellipse workflows
   - Medium-level zone operations

4. **Missing Standardization**:
   - Some zones have database operations, others don't
   - Inconsistent parameter patterns
   - Varying error handling approaches

---

## PREPARATION FOR FEATURE 006

### Key Questions to Address:
1. Should all data zones have consistent function patterns?
2. How should generic S3 functions relate to zone-specific functions?
3. What is the canonical terminology for each concept?
4. How should the conceptual hierarchy be structured?
5. What concepts need clearer definitions for feature 007?

### Areas Needing Formalization:
- **Landing Zone**: Purpose, scope, and operations
- **Data Lake**: Structure and data lifecycle
- **Data Warehouse**: Organization and access patterns  
- **Data Marts**: Business logic and user access
- **Ellipse Platform**: Orchestration and workflow management
- **Cross-cutting concerns**: Security, monitoring, governance

### Foundation Requirements for Feature 007:
- Clear conceptual definitions
- Consistent naming patterns
- Well-defined relationships
- Extensible architecture
- Solid documentation framework

---

This inventory serves as the foundation for concept analysis and formalization in Feature 006.
