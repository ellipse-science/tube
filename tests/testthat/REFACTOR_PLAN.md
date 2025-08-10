## Refactoring Plan for tests/testthat Structure

### Current Analysis:
- ❌ MIXED: `helper-functions.R` has MOCK functions (violates "no mocking" requirement)
- ✅ GOOD: `helper-aws-real.R` has REAL AWS functions (follows requirements)
- ❌ INCONSISTENT: Some tests use mocks, others expect real connections
- ✅ GOOD: Individual test runners exist
- ❌ UNCLEAR: `setup.R` has mock DBI methods

### Required Changes:

1. **REMOVE MOCKING** (per coding instructions: "use real connections, not mocks")
   - Replace `helper-functions.R` mock functions with real connection helpers
   - Remove mock setup from `setup.R` 
   - Update all tests to use real connections only

2. **CONSOLIDATE HELPERS**
   - Merge `helper-functions.R` and `helper-aws-real.R` into single `helper-functions.R`
   - All helpers should provide REAL connection utilities
   - Remove any mocking infrastructure

3. **CONSISTENT TEST FILES**
   - `test-aws-credentials.R` - REAL AWS tests ✅ (already correct)
   - `test-ellipse-main.R` - Convert to REAL database connections
   - `test-basic-functions.R` - Convert to REAL function tests

4. **INDIVIDUAL TEST RUNNERS** ✅ (already exist)
   - `run_aws_tests.R` - AWS functions only
   - `run_ellipse_tests.R` - Ellipse functions only  
   - `run_basic_tests.R` - Basic utility functions only
   - `run_all_tests.R` - All tests combined

### Final Structure:
```
tests/testthat/
├── helper-functions.R          # REAL connection helpers (no mocks)
├── setup.R                     # Real environment setup (no mocks)
├── test-aws-credentials.R      # Real AWS tests ✅
├── test-ellipse-functions.R    # Real database connection tests
├── test-basic-functions.R      # Real utility function tests
├── run_aws_tests.R            # AWS test runner ✅
├── run_ellipse_tests.R        # Ellipse test runner ✅
├── run_basic_tests.R          # Basic test runner ✅
└── run_all_tests.R            # Master test runner ✅
```

### Implementation Steps:
1. Backup current structure
2. Remove all mocking functions
3. Consolidate real helpers
4. Update test files to use real connections
5. Verify all tests work with real AWS/DB connections
