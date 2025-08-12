# Feature 008 Context - Public Datalake Ingest & Query

## 🎯 NEXT FEATURE: PUBLIC DATALAKE INGEST & QUERY

### Feature 008 Scope
Implement the remaining public datalake functionality:
1. **ellipse_ingest()** - Upload files to public datalake
2. **ellipse_query()** - Query public datalake data
3. **ellipse_process()** - Process public datalake data (if needed)

### 🔗 Continuation Strategy

#### Foundation Ready (Feature 007 Complete)
- ✅ **Connection:** `ellipse_connect("DEV", "datalake")` working
- ✅ **Discovery:** `ellipse_discover()` with all patterns working
- ✅ **Infrastructure:** S3 and Glue database access established
- ✅ **Testing:** Framework and AWS integration validated

#### Implementation Approach for Feature 008
1. **REUSE FIRST:** Analyze existing ellipse_ingest() and ellipse_query() implementations
2. **EXTEND:** Add public datalake support to existing functions (don't create new ones)
3. **MINIMAL:** Create new utility functions only if absolutely necessary
4. **CONSISTENT:** Follow patterns established in Feature 007

### 📋 Pre-Development Analysis Required

Before writing any code, the next conversation must:

1. **Analyze Existing Functions**
   ```r
   # Study these functions thoroughly:
   ellipse_ingest()    # Current implementation patterns
   ellipse_query()     # Current implementation patterns
   ellipse_process()   # Current implementation patterns
   ```

2. **Identify Reuse Opportunities**
   - How do current functions handle datawarehouse/datamarts?
   - What parameters need extension for public datalake?
   - Which validation functions can be reused?
   - What error handling patterns exist?

3. **Plan Extension Strategy**
   - Where to add "datalake" database switch cases?
   - What public datalake-specific logic is needed?
   - How to maintain backward compatibility?

### 🎯 Success Criteria for Feature 008

#### Functional Requirements
```r
# These must work after Feature 008:
con <- ellipse_connect("DEV", "datalake")

# File upload to public datalake
ellipse_ingest(con, file_path = "data.csv", 
               dataset_name = "my-dataset", 
               tag = "v1.0", 
               metadata = list(...))

# Query public datalake data  
result <- ellipse_query(con, "SELECT * FROM my-dataset WHERE tag = 'v1.0'")

# Process if needed
ellipse_process(con, ...)

ellipse_disconnect(con)
```

#### Technical Requirements
- ✅ 100% test coverage for new functionality
- ✅ All existing tests continue to pass
- ✅ Real AWS integration validated
- ✅ Comprehensive error handling
- ✅ Consistent user experience
- ✅ No breaking changes to existing APIs

### 📁 File Organization

#### Files to Extend (Don't Create New)
- `R/ellipse.R` - Add public datalake support to existing functions
- `R/utils_check_params.R` - Extend parameter validation if needed

#### Files to Create (Only if Necessary)
- `R/utils_public_datalake_ingest.R` - Only if ingest logic is complex
- `tests/testthat/test-public-datalake-ingest-query.R` - New test file
- `tests/testthat/run_ingest_query_tests.R` - New test runner

### 🔍 Key Questions for Next Conversation

The next conversation should start by asking:

1. **Scope Confirmation:** "Should Feature 008 include ellipse_ingest, ellipse_query, and ellipse_process, or just the first two?"

2. **Implementation Analysis:** "Let me analyze the existing ellipse_ingest() and ellipse_query() functions to understand the current patterns..."

3. **Reuse Strategy:** "Based on the existing patterns, here's how we can extend these functions for public datalake support..."

4. **Parameter Design:** "What parameters should ellipse_ingest() accept for public datalake uploads? (dataset_name, tag, metadata, etc.)"

5. **Data Format:** "What format should uploaded data be in? CSV, Parquet, JSON?"

### 🧪 Testing Strategy

#### Required Test Coverage
1. **Ingest Tests:**
   - Successful file upload to publicdatalakebucket
   - Metadata creation in public-data-lake-content table
   - Error handling for invalid files/parameters
   - Multiple file format support

2. **Query Tests:**
   - Basic SELECT queries on public datalake data
   - Filtered queries by dataset/tag
   - JOIN operations across datasets
   - Error handling for invalid queries

3. **Integration Tests:**
   - Full workflow: connect → ingest → discover → query → disconnect
   - Cross-database compatibility (datawarehouse, datamarts, datalake)
   - Real AWS data persistence validation

### 📈 Expected Deliverables

#### Code Deliverables
- Extended ellipse_ingest() function with public datalake support
- Extended ellipse_query() function with public datalake support  
- Extended ellipse_process() function (if needed)
- Comprehensive test suite (targeting 40+ new tests)
- Updated documentation and examples

#### Quality Deliverables
- 100% test coverage for new functionality
- All linting passes
- Package builds successfully
- Real AWS integration validated
- Performance benchmarks for large file uploads

### ⚡ Quick Start Commands for Next Conversation

```bash
# Load the current state
Rscript -e "devtools::load_all(.)"

# Check Feature 007 functionality still works
Rscript tests/testthat/run_public_datalake_tests.R

# Analyze existing functions
Rscript -e "args(ellipse_ingest); args(ellipse_query)"

# Create new branch for Feature 008
git checkout -b feature/008-public-datalake-ingest-query
```

---

## 🚀 READY FOR FEATURE 008 IMPLEMENTATION

**Previous Feature:** ✅ Feature 007 Complete  
**Next Feature:** 🎯 Feature 008 - Public Datalake Ingest & Query  
**Foundation:** ✅ Solid infrastructure ready  
**Approach:** 🔄 Extend existing functions, maximize reuse
