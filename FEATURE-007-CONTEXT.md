# Feature 007 Context: Public Datalake Connection & Discovery

## 🚨 CRITICAL PROTOCOL REMINDER 🚨
**ITERATIVE SEQUENTIAL BRANCHING - ALWAYS FOLLOW:**
1. ✅ **COMMIT** current feature changes (Feature 006)
2. ✅ **PUSH** current branch 
3. ✅ **CREATE NEW BRANCH** from current branch for next feature
4. ✅ **ASK** what the next feature should be
5. ✅ **CONFIRM** branch creation and feature scope

**NEVER FORGET:** One feature = One branch. Always branch sequentially!

---

## Project Status After Feature 006

### ✅ Feature 006: Formalize and Review Core Concepts (COMPLETED)
**Branch**: `feature/006-formalize-concepts`
**Status**: Ready for commit and push

**Major Achievements**:
- **Fixed datalake logger inconsistency** in `R/datalake.R`
- **Created comprehensive CONCEPTS.md** with both technical and business perspectives
- **Updated README.md** with bilingual links to concept documentation
- **Formalized all existing concepts**: Landing Zone, Datalake (Private), Data Warehouse, Datamarts
- **Established clear naming conventions** and data flow relationships
- **Prepared conceptual foundation** for Feature 007

---

## Feature 007 Requirements: Public Datalake Connection & Discovery

### Business Context
Based on user requirements, Feature 007 will implement **ONLY** the connection and discovery aspects of a **Public Datalake** to complement the existing private datalake:

#### **Current State (Private Datalake)**
- Fed by **automated lambda extractors**
- **Scheduled extractions** from websites and APIs
- **Private access** - not user-uploadable
- **Raw data storage** for pipeline processing

#### **New Requirement (Public Datalake - Connection & Discovery Only)**
- **Dedicated database connection** via `ellipse_connect("DEV", "datalake")`
- **Discovery capabilities** via `ellipse_discover()` with rich metadata
- **S3 file structure**: `s3://publicdatalakebucket/dataset_name/tag/files`
- **AWS user-defined metadata** attached to each file
- **Glue table integration** for metadata-based discovery

#### **Important Scope Limitation**
**Feature 007 ONLY focuses on:**
- ✅ `ellipse_connect()` - connecting to public datalake 
- ✅ `ellipse_discover()` - discovering datasets with metadata
- ❌ `ellipse_ingest()` - **NOT in this feature** (next feature)
- ❌ `ellipse_query()` - **NOT in this feature** (next feature)
- ❌ Upload functionality - **NOT in this feature** (next feature)

### Technical Implementation Requirements

#### **Database Configuration**
Based on the expected output from `ellipse_connect("DEV", "datalake")`:
- **Database Name**: `gluestackpublicdatalakedbbeb173fb` (AWS-generated)
- **Connection Type**: Standard Athena connection via `ellipse_connect()`
- **Bucket**: `bucket-stack-publicdatalakebucket82f867fc-mw1og7tmix56` (AWS-generated)

#### **S3 File Structure**
Files are organized as: `s3://publicdatalakebucket/dataset_name/tag/files`
- **Level 1**: `dataset_name` (e.g., "test-datagotchi", "datagotchi")
- **Level 2**: `tag` (e.g., "elxnca2025", "elxn2025")
- **Level 3**: Actual data files with same format but potentially different schemas

#### **Metadata Architecture**
**AWS User-Defined Metadata Fields (attached to each file):**
- `name` - Dataset name
- `tag` - Dataset tag/version
- `creation_date` - When dataset was created
- `consent_expiry_date` - When consent expires
- `data_destruction_date` - When data should be destroyed
- `sensitivity_level` - Data sensitivity classification
- `ethical_stamp` - Ethical approval status
- `user_metadata_json` - Additional custom metadata as JSON

#### **Glue Table Integration**
- **Single Glue table** contains aggregated metadata about all files
- **Lambda function** rebuilds this table (triggered by future `ellipse_ingest()`)
- **Discovery data source**: `ellipse_discover()` queries this Glue table, not S3 directly
- **No actual file content** in Glue table, only metadata about files

#### **Expected Function Categories**
Following established naming conventions:

1. **Infrastructure Functions** (if needed):
   - `list_public_datalake_bucket()` - Returns public datalake bucket name
   - Additional infrastructure functions only if required

2. **Discovery Integration** (primary focus):
   - Extend `ellipse_discover()` to work with public datalake connection
   - Support discovery patterns: `ellipse_discover(con)`, `ellipse_discover(con, name)`, `ellipse_discover(con, name, tag)`
   - Display rich metadata from Glue table

### Integration Points

#### **Ellipse Layer Integration**
The public datalake must integrate with existing ellipse functions:

1. **ellipse_connect()**:
   - Enable connection to public datalake via `ellipse_connect("DEV", "datalake")`
   - Return connection object that works with existing ellipse functions
   - Maintain backward compatibility with existing database connections

2. **ellipse_discover()**:
   - Include public datalake datasets in discovery results when connected to "datalake" database
   - Display rich metadata for filtering and selection
   - Support three discovery patterns:
     - `ellipse_discover(con)` - All datasets
     - `ellipse_discover(con, name)` - Datasets matching name pattern
     - `ellipse_discover(con, name, tag)` - Specific name/tag combination
   - Maintain backward compatibility with existing databases

#### **Existing Infrastructure Reuse**
Feature 007 should maximize reuse of existing functions:

- **Database Operations**: Extend existing `check_database()` function for "datalake" support
- **AWS Credentials**: Use existing `get_aws_credentials()` infrastructure
- **Glue Integration**: Reuse existing Glue table querying patterns
- **Validation**: Extend existing parameter validation functions

### File Structure Expectations

#### **New Files Likely Needed**
Based on current package structure:

```
R/
├── public-datalake.R     # New file for public datalake functions (if needed)
├── ellipse.R             # Extend existing ellipse functions
├── utils_check_params.R  # Add validation for "datalake" database parameter

tests/testthat/
├── test-public-datalake-functions.R  # New test file (if new functions created)
├── test-ellipse-main.R              # Extend existing tests
└── run_public_datalake_tests.R      # New test runner (if needed)
```

#### **Documentation Updates**
- **CONCEPTS.md**: Add Public Datalake section
- **README.md**: Update examples to include public datalake usage
- **Roxygen documentation**: Complete function documentation for any new functions

### Testing Strategy

#### **Required Test Coverage**
Following Feature 006 standards:
- **100% test coverage** for all new functions
- **Real AWS connections** (not mocked)
- **Environment variables** properly configured
- **Integration tests** with existing ellipse functions

#### **Test Categories**
1. **Connection Tests**: `ellipse_connect("DEV", "datalake")` functionality
2. **Discovery Tests**: Integration with ellipse_discover() for all three patterns
3. **Infrastructure Tests**: Any new bucket or infrastructure functions (if needed)
4. **Error Handling**: Validation and error scenarios
5. **Integration Tests**: Compatibility with existing ellipse functions

### Development Approach

#### **Incremental Implementation**
Following Feature 006 methodology:

1. **Phase 1**: Connection Support
   - Extend `ellipse_connect()` to support "datalake" database parameter
   - Add validation for "datalake" in `check_database()`
   - Complete testing and documentation

2. **Phase 2**: Discovery Integration
   - Extend `ellipse_discover()` to work with public datalake connection
   - Support all three discovery patterns with rich metadata display
   - Maintain backward compatibility

3. **Phase 3**: Infrastructure Functions (if needed)
   - `list_public_datalake_bucket()` (only if required)
   - Complete integration testing

#### **Code Reuse Strategy**
**CRITICAL**: Follow Feature 006 principle of maximum code reuse:
- **Analyze existing functions** before creating new ones
- **Extend existing functions** where possible
- **Create new functions only** when absolutely necessary
- **Ask before creating** new functions

### Quality Standards

#### **Maintained from Feature 006**
- **100% linting compliance** (0 violations)
- **100% test coverage** (all functions tested)
- **Real AWS testing** (no mocked connections)
- **Complete documentation** (roxygen2 for all functions)
- **Consistent naming conventions** (follow established patterns)

#### **Integration Requirements**
- **Backward compatibility** with existing functions
- **Consistent API design** across all ellipse functions
- **Error handling consistency** with existing patterns
- **Performance considerations** for large-scale usage

---

## Critical Development Rules for Feature 007

### **Sequential Branching Protocol**
1. **Feature 006 must be completed** (commit and push)
2. **Create feature/007-public-datalake-connection-discovery** branch
3. **One conversation for Feature 007** implementation
4. **No cross-feature work** in same conversation

### **Code Quality Gates**
- All linting checks pass (`lintr::lint_package()`)
- 100% test coverage (`covr::package_coverage()`)
- All tests pass (`devtools::test()`)
- Package check passes (`devtools::check()`)
- Real AWS connections tested
- Environment variables documented

### **Communication Protocol**
- **Ask first, code second** - present all plans for approval
- **Analyze existing code** before creating new functions
- **Question design decisions** when unclear
- **Document all new concepts** in CONCEPTS.md
- **Maintain naming consistency** with established patterns

---

## Expected Deliverables for Feature 007

### **Functional Deliverables**
- [ ] `ellipse_connect("DEV", "datalake")` connection support
- [ ] `ellipse_discover()` integration with public datalake metadata
- [ ] Infrastructure functions (if needed)
- [ ] Complete test coverage for all new functionality

### **Documentation Deliverables**
- [ ] Updated CONCEPTS.md with Public Datalake section
- [ ] Complete roxygen2 documentation for all functions
- [ ] Updated README.md with public datalake examples
- [ ] Integration examples and usage patterns

### **Quality Deliverables**
- [ ] 100% linting compliance maintained
- [ ] 100% test coverage for new functions
- [ ] All existing tests continue to pass
- [ ] Package integrity maintained (devtools::check passes)
- [ ] Real AWS integration tested and validated

---

## Handoff Instructions for Feature 007 Conversation

### **Start of Feature 007 Conversation Protocol**
1. **Read this context document** thoroughly
2. **Analyze current codebase state** after Feature 006 completion
3. **Present detailed implementation plan** for approval
4. **Get confirmation** before starting any coding
5. **Follow sequential development phases** as outlined above

### **Critical Questions to Ask at Start**
- How should public datalake bucket naming differ from private datalake?
- What specific metadata fields are required for public uploads?
- How should ellipse_discover() display public datalake data differently?
- What level of integration is needed with existing ellipse_ingest()?
- Are there specific business rules for public datalake access?


### **Expected output from ellipse_discover
r$> con <- tube::ellipse_connect("DEV", "datalake")
ℹ Environnement: DEV
ℹ Database: datalake
ℹ Connexion en cours...
ℹ Compartiment: pipeline-stack-athenaqueryresultsbucket6f63bbe4-1hrrrojv867l3
ℹ Base de données: gluestackpublicdatalakedbbeb173fb
ℹ Pour déconnecter: tube::ellipse_disconnect(objet_de_connexion)
✔ Connexion établie avec succès! 👍


r$> tube::ellipse_discover(con)
INFO: (Data scanned: 4.52 KB)
                  table_name             tag file_count creation_date consent_expiry_date data_destruction_date sensitivity_level ethical_stamp user_metadata_preview
                      <char>          <char>      <num>        <char>              <char>                <char>            <char>        <char>                <char>
 1:               datagotchi        elxn2025          1    2025-03-25          2026-03-25            2099-12-31                 2          TRUE    {"data_destruct...
 2:               datagotchi      elxnca2025          1    2025-03-05          2026-03-05            2049-03-06                 2          TRUE    {"data_destruct...
 3:              datagotchi2        elxn2025          1    2025-03-01          2026-03-01            2099-03-01                 2          TRUE    {"data_destruct...
 4:              datagotchi3   elections2025          1    2025-03-18          2026-03-18            2099-01-01                 2         FALSE    {"data_destruct...
 5: from_checkbox_to_textbox             c2t          1    2025-05-01          3000-10-10            3002-10-10                 1          TRUE    {"data_destruct...
 6:          test-datagotchi      elxnca2025          2    2025-04-01          2025-10-01            2045-04-01                 1         false    {"data_destruct...
 7:          test-datagotchi      elxnqc2022          1    2020-01-01          2021-03-01            2099-01-01                 4          true    {"data_destruct...
 8:              test-survey        test-tag          2    2025-08-01          2026-08-01            2032-08-01                 2          true    {"data_destruct...
 9:            testmanyfiles withdiffschemas         10    2025-01-01          2026-01-01            2027-01-01                 5         FALSE    {"data_destruct...
10:             testmanysame      samesfiles          5    2020-01-01          2021-02-02            2022-03-03                 1          TRUE    {"data_destruct...



r$> tube::ellipse_discover(con, "data")
INFO: (Data scanned: 0 Bytes)
INFO: (Data scanned: 6.09 KB)
ℹ Found 4 dataset(s) matching pattern: data
ℹ Use ellipse_discover(con, 'exact_dataset_name') to view details for a specific dataset
$search_pattern
[1] "data"

$matching_tables
[1] "datagotchi"      "datagotchi2"     "datagotchi3"     "test-datagotchi"

$tables_summary
# A tibble: 4 × 3
  table_name      tags_count tags_list             
  <chr>                <int> <chr>                 
1 datagotchi               2 elxnca2025, elxn2025  
2 datagotchi2              1 elxn2025              
3 datagotchi3              1 elections2025         
4 test-datagotchi          2 elxnca2025, elxnqc2022

$note
[1] "Found 4 table(s). Use exact table name to get detailed information."


r$> tube::ellipse_discover(con, "test-datagotchi")
INFO: (Data scanned: 2.12 KB)
$name
[1] "test-datagotchi"

$tags_count
[1] 2

$tags
[1] "elxnca2025" "elxnqc2022"

$total_files
[1] 3

$tags_summary
# A tibble: 2 × 9
  tag        file_count creation_date consent_expiry_date data_destruction_date sensitivity_level ethical_stamp user_metadata_fields user_metadata_values           
  <chr>           <int> <chr>         <chr>               <chr>                 <chr>             <chr>         <chr>                <chr>                          
1 elxnca2025          2 2025-04-01    2025-10-01          2045-04-01            1                 false         title, authors, year Datagotchi, Cath, Alex..., 2025
2 elxnqc2022          1 2020-01-01    2021-03-01          2099-01-01            4                 true          title, authors, year Datagotchi..., toto, titi, 2025

$all_files
# A tibble: 3 × 5
  tag        file_name file_path                                                                                        file_extension file_size_bytes
  <chr>      <chr>     <chr>                                                                                            <chr>                    <dbl>
1 elxnca2025 test.csv  s3://bucket-stack-publicdatalakebucket82f867fc-mw1og7tmix56/test-datagotchi/elxnca2025/test.csv  csv                         66
2 elxnca2025 test1.csv s3://bucket-stack-publicdatalakebucket82f867fc-mw1og7tmix56/test-datagotchi/elxnca2025/test1.csv csv                         58
3 elxnqc2022 test1.csv s3://bucket-stack-publicdatalakebucket82f867fc-mw1og7tmix56/test-datagotchi/elxnqc2022/test1.csv csv                         58

$note
[1] "Table contains 2 tag(s). Use ellipse_discover(con, object, tag) to view specific tag details."


r$> tube::ellipse_discover(con, "test-datagotchi", "elxnqc2022")
INFO: (Data scanned: 1.01 KB)
$name
[1] "test-datagotchi"

$tag
[1] "elxnqc2022"

$file_count
[1] 1

$creation_date
[1] "2020-01-01"

$consent_expiry_date
[1] "2021-03-01"

$data_destruction_date
[1] "2099-01-01"

$sensitivity_level
[1] "4"

$ethical_stamp
[1] "true"

$files
# A tibble: 1 × 4
  file_name file_path                                                                                        file_extension file_size_bytes
  <chr>     <chr>                                                                                            <chr>                    <dbl>
1 test1.csv s3://bucket-stack-publicdatalakebucket82f867fc-mw1og7tmix56/test-datagotchi/elxnqc2022/test1.csv csv                         58

$user_metadata
$user_metadata$title
[1] "Datagotchi elections 2020"

$user_metadata$authors
[1] "toto, titi"

$user_metadata$year
[1] 2025




### **Success Criteria**
Feature 007 will be considered complete when:
- Users can connect to public datalake via `ellipse_connect("DEV", "datalake")`
- Public datalake data appears in `ellipse_discover()` results with rich metadata
- All three discovery patterns work: `ellipse_discover(con)`, `ellipse_discover(con, name)`, `ellipse_discover(con, name, tag)`
- All quality gates pass (linting, testing, documentation)
- Integration with existing ellipse functions is seamless
- Backward compatibility is maintained

**Ready for Feature 006 completion and Feature 007 handoff!**
