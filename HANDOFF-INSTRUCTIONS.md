# Handoff Instructions for Feature 008

## 🎯 FEATURE 008: PUBLIC DATALAKE QUERY IMPLEMENTATION

### Context Summary
- **Current State**: Feature 007 (Public Datalake Discovery) complete and committed
- **Next Task**: Extend `ellipse_query()` function to support public datalake querying
- **Branch**: `feature/008-public-datalake-query` (already created)
- **Scope**: Query functionality only - ingest will be Feature 009

### Foundation Analysis Complete
✅ **Connection**: `ellipse_connect("DEV", "datalake")` working  
✅ **Discovery**: `ellipse_discover()` with all patterns working  
✅ **Infrastructure**: S3 and Glue database access established  
✅ **Testing**: Framework and AWS integration validated  

### Current ellipse_query() Analysis
**Function Location**: `/workspaces/tube/R/ellipse.R` lines 362-390

**Current Implementation Pattern**:
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

**Current Limitation**: Only works with table names, not SQL queries. Public datalake needs both patterns.

### Extension Strategy Required

#### Pattern 1: Table Name Query (Current)
```r
# Should work for public datalake:
result <- ellipse_query(con, "public-data-lake-content")
```

#### Pattern 2: SQL Query (New for public datalake)
```r
# Should work for public datalake:
result <- ellipse_query(con, "SELECT * FROM \"public-data-lake-content\" WHERE tag = 'v1.0'")
```

### Implementation Approach

1. **Detect Query Type**: 
   - If `table` parameter contains SQL keywords (SELECT, WITH, etc.) → SQL query
   - Otherwise → Table name (existing behavior)

2. **Public Datalake Logic**:
   - For SQL queries: Use `DBI::dbGetQuery()` directly
   - For table names: Use existing `dplyr::tbl()` approach
   - Maintain existing datawarehouse/datamarts behavior

3. **Error Handling**:
   - Validate SQL syntax for public datalake queries
   - Provide meaningful error messages
   - Handle Athena-specific query limitations

### Success Criteria
```r
# After Feature 008, these must work:
con <- ellipse_connect("DEV", "datalake")

# Table name queries (existing pattern)
result1 <- ellipse_query(con, "public-data-lake-content")

# SQL queries (new pattern for public datalake)
result2 <- ellipse_query(con, "SELECT * FROM \"public-data-lake-content\" LIMIT 10")
result3 <- ellipse_query(con, "SELECT name, tag, COUNT(*) FROM \"public-data-lake-content\" GROUP BY name, tag")

# Existing databases should still work unchanged
con_dw <- ellipse_connect("DEV", "datawarehouse")
result4 <- ellipse_query(con_dw, "existing_table")  # Table name only

ellipse_disconnect(con)
ellipse_disconnect(con_dw)
```

### Required Files to Modify
- ✅ `R/ellipse.R` - Extend `ellipse_query()` function
- ✅ `tests/testthat/test-public-datalake-query.R` - New comprehensive test file
- ✅ `man/ellipse_query.Rd` - Update documentation (auto-generated from roxygen)

### Testing Strategy
**Required Coverage (20+ tests)**:
- Basic table name queries on public-data-lake-content
- SQL SELECT queries with various WHERE clauses
- Aggregation queries (COUNT, SUM, GROUP BY)
- Complex queries with JSON metadata filtering
- Error handling for invalid SQL and table names
- Cross-database compatibility testing
- Real AWS Athena integration validation

### Next Conversation Should Start With
1. **Load current state**: `Rscript -e "devtools::load_all(.)"`
2. **Verify Feature 007 works**: `Rscript tests/testthat/run_public_datalake_tests.R`
3. **Analyze current function**: Review `ellipse_query()` implementation
4. **Create implementation plan**: Design dual-mode query support
5. **Implement and test**: With comprehensive test coverage

---

## 🚀 READY FOR FEATURE 008 CONVERSATION

**Current Branch**: `feature/008-public-datalake-query`  
**Context File**: `FEATURE-008-CONTEXT.md` (complete specification)  
**Handoff File**: `HANDOFF-INSTRUCTIONS.md` (this file)  
**Foundation**: ✅ Solid public datalake infrastructure ready  
**Approach**: 🔄 Extend existing `ellipse_query()`, maintain backward compatibility