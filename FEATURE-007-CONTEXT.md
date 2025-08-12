# Feature 007 Context: Public Datalake Implementation

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

## Feature 007 Requirements: Public Datalake

### Business Context
Based on user requirements, Feature 007 will implement a **Public Datalake** concept to complement the existing private datalake:

#### **Current State (Private Datalake)**
- Fed by **automated lambda extractors**
- **Scheduled extractions** from websites and APIs
- **Private access** - not user-uploadable
- **Raw data storage** for pipeline processing

#### **New Requirement (Public Datalake)**
- **User-uploadable files** via tube package
- **Rich metadata application** for discovery and filtering
- **Integration with ellipse_discover()** for data discovery
- **Metadata-based filtering** in ellipse_query()
- **Business user-oriented** design

### Technical Implementation Requirements

#### **Expected Function Categories**
Based on established naming conventions from Feature 006:

1. **Infrastructure Functions**:
   - `list_public_datalake_bucket()` - Returns public datalake bucket name
   - `list_public_datalake_partitions()` - Lists organized data partitions
   - Additional infrastructure functions as needed

2. **Upload Functions**:
   - `upload_file_to_public_datalake()` - Direct user uploads with metadata
   - Integration with existing `ellipse_ingest()` workflow

3. **Metadata Functions**:
   - Functions to apply, manage, and query metadata
   - Integration with Glue catalog for discoverability

4. **Discovery Integration**:
   - Extend `ellipse_discover()` to include public datalake data
   - Enable metadata-based filtering capabilities

#### **Expected Bucket Pattern**
Following established conventions:
- **Bucket Pattern**: `"publicdatalakebucket"` or `"datalakepublicbucket"`
- **Function Naming**: `*_public_datalake_*`

#### **Metadata Architecture**
- **Rich metadata application** for user uploads
- **Discovery integration** - feed ellipse_discover()
- **Query filtering** - enable metadata-based filtering in ellipse_query()
- **Business-oriented** metadata structure

### Integration Points

#### **Ellipse Layer Integration**
The public datalake must integrate with existing ellipse functions:

1. **ellipse_discover()**:
   - Include public datalake datasets in discovery results
   - Display metadata for filtering and selection
   - Maintain backward compatibility

2. **ellipse_query()**:
   - Enable querying of public datalake data
   - Support metadata-based filtering
   - Consistent interface with existing data sources

3. **ellipse_ingest()**:
   - Potentially extend to support public datalake uploads
   - Or create new ellipse function for public uploads

#### **Existing Infrastructure Reuse**
Feature 007 should maximize reuse of existing functions:

- **S3 Operations**: Reuse `list_s3_buckets()`, `upload_file_to_s3()`, etc.
- **Glue Integration**: Extend existing Glue functions for metadata management
- **AWS Credentials**: Use existing `get_aws_credentials()` infrastructure
- **Validation**: Extend existing parameter validation functions

### File Structure Expectations

#### **New Files Likely Needed**
Based on current package structure:

```
R/
├── public-datalake.R     # New file for public datalake functions
├── ellipse.R             # Extend existing ellipse functions
├── glue.R               # Extend for public datalake metadata
└── utils_check_params.R  # Add validation for public datalake params

tests/testthat/
├── test-public-datalake-functions.R  # New test file
├── test-ellipse-main.R              # Extend existing tests
└── run_public_datalake_tests.R      # New test runner
```

#### **Documentation Updates**
- **CONCEPTS.md**: Add Public Datalake section
- **README.md**: Update examples to include public datalake usage
- **Roxygen documentation**: Complete function documentation

### Testing Strategy

#### **Required Test Coverage**
Following Feature 006 standards:
- **100% test coverage** for all new functions
- **Real AWS connections** (not mocked)
- **Environment variables** properly configured
- **Integration tests** with existing ellipse functions

#### **Test Categories**
1. **Infrastructure Tests**: Bucket operations, metadata management
2. **Upload Tests**: File upload with metadata application
3. **Discovery Tests**: Integration with ellipse_discover()
4. **Query Tests**: Metadata-based filtering in ellipse_query()
5. **Error Handling**: Validation and error scenarios

### Development Approach

#### **Incremental Implementation**
Following Feature 006 methodology:

1. **Phase 1**: Core infrastructure functions
   - `list_public_datalake_bucket()`
   - Basic S3 operations for public datalake
   - Complete testing and documentation

2. **Phase 2**: Upload functionality
   - `upload_file_to_public_datalake()`
   - Metadata application system
   - Integration testing

3. **Phase 3**: Discovery integration
   - Extend `ellipse_discover()`
   - Include public datalake in results
   - Maintain backward compatibility

4. **Phase 4**: Query integration
   - Extend `ellipse_query()` for metadata filtering
   - Support public datalake data queries
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
2. **Create feature/007-public-datalake** branch
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
- [ ] Public datalake infrastructure functions
- [ ] User upload functionality with metadata
- [ ] Discovery integration (ellipse_discover extension)
- [ ] Query integration (ellipse_query extension)
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

### **Success Criteria**
Feature 007 will be considered complete when:
- Users can upload files to public datalake with metadata
- Public datalake data appears in ellipse_discover() results
- Metadata-based filtering works in ellipse_query()
- All quality gates pass (linting, testing, documentation)
- Integration with existing ellipse functions is seamless
- Backward compatibility is maintained

**Ready for Feature 006 completion and Feature 007 handoff!**
