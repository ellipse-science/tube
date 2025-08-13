# Feature 008 Context - Public Datalake Ingest & Query

## 🎯 NEXT FEATURE: PUBLIC DATALAKE INGEST & QUERY

### Feature 008 Scope - FOCUSED ON QUERY ONLY
Implement public datalake query functionality:
1. **ellipse_query()** - Query public datalake data (FEATURE 008)
2. **ellipse_ingest()** - Upload files and process to public datalake (FEATURE 009 - FUTURE)

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
   # Study this function thoroughly:
   ellipse_query()     # Current implementation patterns
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

# Query public datalake data  
result <- ellipse_query(con, "SELECT * FROM my-dataset WHERE tag = 'v1.0'")
result <- ellipse_query(con, "SELECT * FROM \"public-data-lake-content\" LIMIT 10")

# Discovery still works (from Feature 007)
ellipse_discover(con)
ellipse_discover(con, "dataset-name")

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
- `tests/testthat/test-public-datalake-query.R` - New test file
- `tests/testthat/run_query_tests.R` - New test runner

### 🔍 Key Questions for Next Conversation

The next conversation should start by asking:

1. **Scope Confirmation:** "Feature 008 will focus on ellipse_query() for public datalake support. ellipse_ingest() will be Feature 009."

2. **Implementation Analysis:** "Let me analyze the existing ellipse_query() function to understand the current patterns..."

3. **Reuse Strategy:** "Based on the existing patterns, here's how we can extend ellipse_query() for public datalake support..."

4. **Query Capabilities:** "What types of queries should work on public datalake? Table queries, filtered queries, aggregations?"

### 🧪 Testing Strategy

#### Required Test Coverage
1. **Query Tests:**
   - Basic SELECT queries on public-data-lake-content table
   - Filtered queries by dataset/tag/metadata
   - COUNT and aggregation queries
   - LIMIT and ORDER BY queries
   - Error handling for invalid queries
   - Complex WHERE clauses with JSON metadata

2. **Integration Tests:**
   - Full workflow: connect → discover → query → disconnect
   - Cross-database compatibility (datawarehouse, datamarts, datalake)
   - Real AWS Athena query execution
   - Query result formatting and data types

### 📈 Expected Deliverables

#### Code Deliverables
- Extended ellipse_query() function with public datalake support
- Comprehensive test suite (targeting 20+ new query tests)
- Updated documentation and examples for query functionality

#### Quality Deliverables
- 100% test coverage for query functionality
- All linting passes
- Package builds successfully
- Real AWS Athena integration validated
- Query performance benchmarks

### ⚡ Quick Start Commands for Next Conversation

```bash
# Load the current state
Rscript -e "devtools::load_all(.)"

# Check Feature 007 functionality still works
Rscript tests/testthat/run_public_datalake_tests.R

# Analyze existing functions
Rscript -e "args(ellipse_query)"

# Create new branch for Feature 008
git checkout -b feature/008-public-datalake-query
```

---

## 🚀 READY FOR FEATURE 008 IMPLEMENTATION

**Previous Feature:** ✅ Feature 007 Complete  
**Next Feature:** 🎯 Feature 008 - Public Datalake Query (ellipse_query support)  
**Future Feature:** 📋 Feature 009 - Public Datalake Ingest (ellipse_ingest support)  
**Foundation:** ✅ Solid infrastructure ready  
**Approach:** 🔄 Extend existing ellipse_query(), maximize reuse
