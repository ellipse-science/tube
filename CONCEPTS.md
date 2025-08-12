# Tube Package: Data Platform Concepts

## Overview

The `tube` R package provides a comprehensive interface to an AWS-based enterprise data platform. This document formalizes the core concepts and their relationships, serving as the definitive reference for understanding the data platform architecture.

## Data Platform Architecture

### Conceptual Data Flow
```
Raw Data Sources
       ↓
  Landing Zone ────────→ Datalake (Private)
       ↓                     ↓
   ETL Pipeline        Raw Data Storage
       ↓                     ↓
  Data Warehouse ←──── Data Processing
       ↓
   Datamarts ←─────── Business Logic
       ↓
  Business Users
```

---

## Core Data Platform Concepts

### 1. Landing Zone

#### **Business Definition**
The **Landing Zone** is the initial entry point for all external data into the platform. It serves as a secure, temporary staging area where raw files are deposited before being processed by automated pipelines.

#### **Technical Implementation**
- **Bucket Pattern**: `"landingzonebucket"`
- **Function Naming**: `*_landing_zone_*`
- **Primary Functions**:
  - `list_landing_zone_bucket()` - Returns landing zone bucket name
  - `list_landing_zone_partitions()` - Lists organized data partitions
  - `upload_file_to_landing_zone()` - Uploads files with metadata

#### **Business Purpose**
- **Data Ingestion**: Receives raw data from external sources
- **Quality Gate**: First point of validation and metadata application
- **Pipeline Trigger**: Initiates automated data processing workflows
- **Audit Trail**: Maintains records of all incoming data

#### **Key Characteristics**
- Temporary storage (data moves to datalake after processing)
- Supports both batch and version-based file organization
- Applies metadata for downstream processing
- Used by `ellipse_ingest()` for user-initiated data uploads

---

### 2. Datalake (Private)

#### **Business Definition**
The **Datalake** is the central repository for raw, unprocessed data from automated extraction pipelines. It stores data in its native format, preserving all original information for future processing and analysis.

#### **Technical Implementation**
- **Bucket Pattern**: `"datalakebucket"`
- **Function Naming**: `*_datalake_*`
- **Primary Functions**:
  - `list_datalake_bucket()` - Returns datalake bucket name

#### **Business Purpose**
- **Raw Data Preservation**: Maintains original data formats and structures
- **Pipeline Source**: Feeds automated ETL processes
- **Historical Archive**: Long-term storage of all extracted data
- **Data Lineage**: Source of truth for all downstream transformations

#### **Key Characteristics**
- Fed by automated lambda-based extractors
- Scheduled extractions from websites and APIs
- Private access (not user-uploadable)
- Immutable storage pattern
- **Note**: A future "Public Datalake" concept will be introduced for user uploads

---

### 3. Data Warehouse

#### **Business Definition**
The **Data Warehouse** contains structured, processed data optimized for analytical queries and reporting. Data is organized in a schema that supports efficient business intelligence operations.

#### **Technical Implementation**
- **Bucket Pattern**: `"datawarehousebucket"`
- **Database Pattern**: `"datawarehouse"`
- **Function Naming**: `*_datawarehouse_*`
- **Primary Functions**:
  - `list_datawarehouse_bucket()` - Returns warehouse bucket name
  - `list_datawarehouse_database()` - Returns warehouse database name
  - `list_datawarehouse_tables()` - Lists available analytical tables

#### **Business Purpose**
- **Structured Analytics**: Optimized for complex analytical queries
- **Enterprise Reporting**: Supports business intelligence tools
- **Historical Analysis**: Time-series and trend analysis
- **Cross-Domain Integration**: Combines data from multiple sources

#### **Key Characteristics**
- Structured schema design
- Optimized for query performance
- Supports large-scale analytical workloads
- Fed by processed data from datalake

---

### 4. Datamarts

#### **Business Definition**
**Datamarts** are collections of domain-specific, business-ready datasets organized by topic or business function. Each datamart contains tables that serve specific analytical needs or business processes.

#### **Technical Implementation**
- **Bucket Pattern**: `"datamartbucket"`
- **Database Pattern**: `"datamart"`
- **Function Naming**: `*_datamarts_*` (for infrastructure), `*_datamart_*` (for specific operations)
- **Primary Functions**:
  - `list_datamarts_bucket()` - Returns datamarts bucket name
  - `list_datamarts_database()` - Returns datamarts database name
  - `list_datamart_tables()` - Lists tables in specific datamart
  - `upload_dataframe_to_datamart()` - Publishes user-created datasets

#### **Business Purpose**
- **Topic-Focused Analytics**: Domain-specific data collections
- **User Contributions**: Platform for publishing user-generated datasets
- **Business Logic**: Apply business rules and calculations
- **Self-Service Analytics**: Enable business users to create and share data

#### **Key Characteristics**
- Multiple datamarts per platform (collections approach)
- User-publishable via `ellipse_publish()`
- Business-ready data with applied transformations
- Topic or functional organization (e.g., sales, marketing, operations)

---

## Supporting Infrastructure Concepts

### 5. Athena Query Engine

#### **Business Definition**
**Athena** provides serverless SQL query capabilities across all data platform storage layers, enabling interactive analysis without infrastructure management.

#### **Technical Implementation**
- **Bucket Pattern**: `"athenaqueryresults"`
- **Function Naming**: `*_athena_*`
- **Primary Functions**:
  - `list_athena_staging_bucket()` - Returns query results storage location

#### **Business Purpose**
- **Interactive Querying**: Ad-hoc SQL analysis across platform
- **Results Management**: Stores and manages query outputs
- **Performance Optimization**: Serverless scaling for varying workloads

---

### 6. AWS Glue Data Catalog

#### **Business Definition**
**Glue** provides the metadata catalog and ETL orchestration services that make data discoverable and processable across the platform.

#### **Technical Implementation**
- **Function Naming**: `*_glue_*`
- **Primary Functions**:
  - `list_glue_databases()` - Lists metadata schemas
  - `list_glue_tables()` - Lists cataloged tables
  - `run_glue_job()` - Executes ETL processes
  - `update_glue_table_*()` - Manages table metadata

#### **Business Purpose**
- **Data Discovery**: Makes data searchable and discoverable
- **ETL Orchestration**: Manages data transformation workflows
- **Metadata Management**: Maintains data lineage and structure information
- **Schema Evolution**: Handles changes in data structure over time

---

### 7. Ellipse Orchestration Layer

#### **Business Definition**
**Ellipse** is the high-level orchestration layer that provides simplified, business-friendly interfaces to the underlying data platform complexity.

#### **Technical Implementation**
- **Function Naming**: `ellipse_*`
- **Exported Functions** (user-facing):
  - `ellipse_connect()` - Establishes platform session
  - `ellipse_discover()` - Finds available datasets
  - `ellipse_query()` - Retrieves data with business filters
  - `ellipse_ingest()` - Uploads files to platform
  - `ellipse_publish()` - Publishes datasets to datamarts
  - `ellipse_process()` - Triggers data processing workflows

#### **Business Purpose**
- **Simplified Interface**: Hides infrastructure complexity
- **Workflow Orchestration**: Coordinates multi-step data operations
- **Business Logic**: Applies domain-specific rules and validations
- **User Experience**: Provides intuitive data platform interactions

---

## Data Flow Relationships

### **Ingestion Flow**
1. **User Upload**: `ellipse_ingest()` → Landing Zone
2. **Processing**: Landing Zone → ETL Pipeline → Datalake
3. **Transformation**: Datalake → Data Warehouse
4. **Business Logic**: Data Warehouse → Datamarts

### **Query Flow**
1. **Discovery**: `ellipse_discover()` → Browse available datasets
2. **Connection**: `ellipse_connect()` → Establish session
3. **Analysis**: `ellipse_query()` → Retrieve and analyze data

### **Publishing Flow**
1. **Preparation**: User creates R dataframe
2. **Publishing**: `ellipse_publish()` → Upload to datamarts
3. **Processing**: ETL converts to queryable tables
4. **Availability**: Data becomes discoverable via `ellipse_discover()`

---

## Naming Conventions

### **Function Naming Standards**
- **Infrastructure Operations**: `list_[concept]_bucket()`, `list_[concept]_database()`
- **Data Operations**: `upload_*_to_[concept]()`, `list_[concept]_tables()`
- **Ellipse Layer**: `ellipse_[action]()`
- **Utilities**: `check_*()`, `get_*()`, `convert_*()`

### **Bucket/Database Patterns**
- **Landing Zone**: `"landingzonebucket"`
- **Datalake**: `"datalakebucket"`
- **Data Warehouse**: `"datawarehousebucket"` / `"datawarehouse"`
- **Datamarts**: `"datamartbucket"` / `"datamart"`
- **Athena**: `"athenaqueryresults"`

---

## Future Concepts (Feature 007)

### **Public Datalake (Planned)**
A new concept will be introduced to handle user-uploaded files with metadata:

#### **Planned Purpose**
- **User File Storage**: Direct user uploads with rich metadata
- **Discovery Integration**: Feed `ellipse_discover()` with user-contributed data
- **Query Filtering**: Enable metadata-based filtering in `ellipse_query()`
- **Hybrid Architecture**: Complement private datalake for automated extractions

#### **Key Distinctions from Private Datalake**
- **User-Accessible**: Direct upload capability
- **Metadata-Rich**: Enhanced metadata for discovery and filtering
- **Business-Oriented**: Designed for business user workflows
- **Integration-Ready**: Built for ellipse layer consumption

---

## Package Architecture Summary

The `tube` package implements a layered architecture:

1. **Infrastructure Layer**: Direct AWS service interactions (`list_*`, `upload_*`)
2. **Platform Layer**: Data platform abstractions (datalake, datamarts, etc.)
3. **Orchestration Layer**: Business workflow management (`ellipse_*`)
4. **Utility Layer**: Supporting functions for validation and data handling

This architecture enables both technical users (direct infrastructure access) and business users (ellipse layer) to interact with the platform at their appropriate level of abstraction.
