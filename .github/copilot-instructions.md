# Copilot Instructions for R Package Development

## 🚨 CRITICAL REMINDER: ITERATIVE SEQUENTIAL BRANCHING 🚨

**BEFORE STARTING ANY NEW FEATURE:**
1. ✅ **COMMIT** current feature changes
2. ✅ **PUSH** current branch 
3. ✅ **CREATE NEW BRANCH** from current branch for next feature
4. ✅ **ASK** what the next feature should be
5. ✅ **CONFIRM** branch creation and feature scope

**NEVER FORGET:** One feature = One branch. Always branch sequentially!

## Table of Contents

1. [Core Principles](#core-principles)
   - [Ask First, Code Second](#1-ask-first-code-second)
   - [Question Everything](#2-question-everything)

2. [Development Workflow](#development-workflow)
   - [Iterative Branching Strategy](#iterative-branching-strategy)
   - [Feature Development Cycle](#feature-development-cycle)
   - [Pre-Development Checklist](#pre-development-checklist)
   - [Required Questions to Ask](#required-questions-to-ask)

3. [Documentation Requirements](#documentation-requirements)
   - [Roxygen2 Documentation](#roxygen2-documentation)
   - [Internal Functions](#internal-functions)
   - [Package-Level Documentation](#package-level-documentation)

4. [Unit Testing Framework](#unit-testing-framework)
   - [Mandatory Testing Rule](#mandatory-testing-rule)
   - [Testing Structure](#testing-structure)
   - [Required Test Coverage](#required-test-coverage)
   - [Test Development Rules](#test-development-rules)

5. [Guardrails and Safety Measures](#guardrails-and-safety-measures)
   - [Code Review Checkpoints](#code-review-checkpoints)
   - [Input Validation Pattern](#input-validation-pattern)
   - [Error Handling Standards](#error-handling-standards)

6. [Package Structure Requirements](#package-structure-requirements)
   - [Essential Files](#essential-files)
   - [DESCRIPTION File Requirements](#description-file-requirements)

7. [Development Process](#development-process)
   - [Step-by-Step Workflow](#step-by-step-workflow)

8. [Quality Assurance](#quality-assurance)
   - [Automated Checks](#automated-checks)
   - [Linting Requirements](#linting-requirements)
   - [Manual Review Points](#manual-review-points)

9. [CI/CD Integration](#cicd-integration)
   - [GitHub Actions Workflow](#github-actions-workflow)
   - [Environment Variables Configuration](#environment-variables-configuration)
   - [Pre-commit Hooks](#pre-commit-hooks-recommended)
   - [Branch Protection Rules](#branch-protection-rules)

10. [Linting Integration](#linting-integration)
    - [Pre-Development Linting Check](#pre-development-linting-check)
    - [During Development](#during-development)
    - [Linting Workflow Integration](#linting-workflow-integration)

11. [Communication Protocol](#communication-protocol)
    - [When to Ask Questions](#when-to-ask-questions)
    - [How to Present Plans](#how-to-present-plans)
    - [Required Confirmations](#required-confirmations)

12. [Protected Files - Require Plan Approval](#protected-files---require-plan-approval)
    - [README.md Changes](#readmemd-changes)
    - [Other Protected Documentation Files](#other-protected-documentation-files)

13. [File Creation Requirements](#file-creation-requirements)

14. [Environment Variables Best Practices](#environment-variables-best-practices)
    - [Function Implementation](#function-implementation)
    - [Testing Environment Variables](#testing-environment-variables)
    - [.Renviron Documentation](#renviron-documentation)

15. [Emergency Stops](#emergency-stops)

---

## Core Principles

### 1. **ASK FIRST, CODE SECOND**
- **NEVER make direct edits without presenting a clear plan first**
- Always present proposed changes as bullet points for approval
- Ask clarifying questions when requirements are unclear or ambiguous
- Seek confirmation before implementing any significant changes

### 2. **Question Everything**
- If any instruction is unclear, ask for clarification immediately
- When multiple approaches are possible, present options and ask for preference
- Verify understanding of requirements before proceeding
- Ask about edge cases and potential complications
- **ALWAYS analyze existing codebase before creating new functions**

### 3. **REUSE FIRST, BUILD SECOND**
- **MANDATORY: Analyze existing codebase before writing any new code**
- **Reuse existing functions** whenever possible
- **Extend existing functions** rather than creating duplicates
- **Create new functions ONLY when absolutely necessary**
- **Ask before creating new functions**: "Can this be accomplished with existing code?"

## Development Workflow

### Iterative Branching Strategy

**CORE PRINCIPLE: ONE FEATURE = ONE BRANCH**

#### Branching Philosophy
- Build **one feature at a time** in sequential branches
- Each branch represents a **complete, working feature**
- If a feature implementation fails, **abandon the branch** and start fresh from the previous working branch
- **Final integration branch** will combine all successful feature branches

#### Branch Naming Convention
```
main
├── feature/001-basic-setup
├── feature/002-core-functions
├── feature/003-data-validation
├── feature/004-api-integration
└── feature/final-integration
```

#### **MANDATORY WORKFLOW:**

1. **Before Starting ANY Work:**
   ```
   COPILOT MUST ASK: "What specific feature should we work on next?"
   ```

2. **Feature Definition Phase:**
   ```
   COPILOT MUST ASK:
   • "What exactly should this feature accomplish?"
   • "What functions/files will this feature include?"
   • "Are there dependencies on previous features?"
   • "What are the acceptance criteria for this feature?"
   ```

3. **Branch Creation Protocol:**
   ```
   COPILOT MUST:
   • Create a new branch: `git checkout -b feature/XXX-feature-name`
   • Confirm branch creation before any coding
   • Work exclusively on that branch until feature is complete
   ```

4. **Feature Completion Detection:**
   ```
   COPILOT MUST AUTOMATICALLY STOP AND ASK:
   "This feature appears complete. Should we:
   • Create a new branch for the next feature?
   • What should the next feature be?"
   ```

### Feature Development Cycle

#### Phase 1: Feature Planning
**ALWAYS start each feature with:**
```
## Feature: [Feature Name]

## Codebase Analysis:
• [Existing functions that could be reused]
• [Existing patterns that should be followed]
• [Similar functionality already implemented]
• [Helper functions that can be leveraged]

## Reuse Strategy:
• [How existing code will be used]
• [What extensions to existing functions are needed]
• [Why new functions are necessary (if any)]

## Proposed Implementation:
• [Specific function 1] - [What it does] - [REUSE existing function X]
• [Specific function 2] - [What it does] - [EXTEND existing function Y]
• [New function 3] - [What it does] - [WHY new function is necessary]
• [Files to create/modify]
• [Tests to write]
• [Documentation to create]

## Questions before starting:
• [Clarifications needed]
• [Design decisions required]
• "Can any of these be accomplished by reusing existing code?"

## Definition of Done:
• [ ] All functions implemented
• [ ] 100% test coverage
• [ ] All linting passes
• [ ] Documentation complete
• [ ] Feature works as intended
• [ ] Maximum code reuse achieved
```

#### Phase 2: Implementation Sequence
**STRICT ORDER - NO EXCEPTIONS:**
1. **Analyze existing codebase thoroughly**
2. **Identify reusable functions and patterns**
3. **Write comprehensive tests first** (including tests for code reuse)
4. Write roxygen documentation
5. **Implement by reusing/extending existing code first**
6. **Create new functions only if absolutely necessary**
7. Verify all tests pass
8. Run full linting and package checks
9. **STOP** and ask about next feature

#### Phase 3: Feature Completion
When a feature is complete, **COPILOT MUST:**
```
✅ Feature [Name] Complete!

## Completion Checklist:
• [ ] All tests pass (100% coverage)
• [ ] Linting passes
• [ ] Documentation complete
• [ ] Package check passes

## Ready for next feature:
WHAT FEATURE SHOULD WE WORK ON NEXT?

Should I create a new branch for the next feature?
```

### Branch Management Rules

#### **NEVER:**
- Work on multiple features simultaneously
- Continue coding if tests fail
- Merge incomplete features
- Modify previous feature branches once complete

#### **ALWAYS:**
- Ask which feature to work on before starting
- Create dedicated branch for each feature
- Complete entire feature before moving on
- Test feature thoroughly before declaring it complete
- **Stop and ask** when feature appears finished

#### **Error Recovery Protocol:**
If implementation gets stuck or goes wrong:
```
COPILOT MUST IMMEDIATELY:
1. STOP all coding
2. Present the problem clearly
3. Ask: "Should we:
   • Continue debugging this approach?
   • Abandon this branch and restart the feature?
   • Modify the feature requirements?"
```

### Integration Strategy
- **Final branch** (`feature/final-integration`) will merge all working features
- Each feature branch must be **independently functional**
- **No dependencies** between feature branches unless explicitly planned
- Final integration includes comprehensive testing of combined features

### Pre-Development Checklist
Before making any changes, always:

1. **Present a clear plan** in bullet points format:
   ```
   ## Proposed Changes:
   • [Specific change 1]
   • [Specific change 2]
   • [Files to be modified/created]
   • [Dependencies to be added]
   • [Tests to be written]
   
   ## Questions for clarification:
   • [Any unclear requirements]
   • [Design decisions needed]
   ```

2. **Wait for explicit approval** before proceeding with implementation

### Required Questions to Ask
Always ask these questions when they apply:

- **Codebase analysis**: "I found these existing functions [list]. Can we reuse any of them?"
- **Code duplication check**: "This looks similar to [existing function]. Should I extend that instead?"
- **Function scope**: "What should this function do when given invalid inputs?"
- **Dependencies**: "Should I use existing packages or implement from scratch?"
- **Performance**: "Are there performance requirements I should consider?"
- **Compatibility**: "What R versions should this support?"
- **API design**: "Should this function be exported or internal?"
- **Error handling**: "How should errors be communicated to users?"
- **Reuse validation**: "Before creating new code, can this be accomplished with existing functions?"

## Documentation Requirements

### Roxygen2 Documentation
Every exported function MUST include:

```r
#' @title Brief title (required)
#' @description Detailed description (required)
#' @param param_name Description of parameter, including type and constraints
#' @return Description of return value, including type and structure
#' @examples
#' # Example usage (required for exported functions)
#' result <- my_function(param = "value")
#' @export
#' @seealso Related functions
#' @author Author name
#' @keywords keywords for help system
```

### Internal Functions
Internal functions should have minimal roxygen documentation:
```r
#' Internal helper function for [purpose]
#' @param param_name Brief description
#' @return Brief return description
#' @keywords internal
```

### Package-Level Documentation
Maintain these documentation files:
- `README.md` - Installation, basic usage, examples
- `NEWS.md` - Version history and changes
- `DESCRIPTION` - Package metadata
- Vignettes for complex workflows

## Unit Testing Framework

### Testing Structure
```
tests/
├── testthat/
│   ├── test-function-name.R
│   ├── test-edge-cases.R
│   ├── test-integration.R
│   └── helper-functions.R
└── testthat.R
```

### **MANDATORY TESTING RULE**
**NO CODE CAN BE PUSHED WITHOUT COMPLETE UNIT TESTS**
- Every function must have tests before it can be committed
- **IF COPILOT CREATES A NEW FUNCTION, COPILOT MUST IMMEDIATELY CREATE THE ASSOCIATED TESTS**
- Every line of code must be covered by tests
- Tests must pass locally before any push attempts
- CI/CD will reject pushes without adequate test coverage

### Required Test Coverage
For EVERY function, write tests for:

1. **Happy path scenarios**
   ```r
   test_that("function works with valid inputs", {
     result <- my_function(valid_input)
     expect_equal(result, expected_output)
   })
   ```

2. **Edge cases**
   ```r
   test_that("function handles edge cases", {
     expect_equal(my_function(NULL), expected_null_behavior)
     expect_equal(my_function(character(0)), expected_empty_behavior)
   })
   ```

3. **Error conditions**
   ```r
   test_that("function throws appropriate errors", {
     expect_error(my_function(invalid_input), "specific error message")
     expect_warning(my_function(problematic_input), "warning message")
   })
   ```

4. **Type validation**
   ```r
   test_that("function validates input types", {
     expect_error(my_function("string"), "must be numeric")
     expect_error(my_function(list()), "must be atomic vector")
   })
   ```

### Test Development Rules
- **BLOCKING REQUIREMENT: Write tests BEFORE implementing functions** (TDD approach)
- **100% test coverage required** - no code without tests
- Test with different R versions if supporting multiple versions
- Include performance tests for critical functions
- Test with realistic data sizes
- **All tests must pass before any commit**
- Tests using environment variables must use `.Renviron` file

## Guardrails and Safety Measures

### Code Review Checkpoints
Before any code is finalized, verify:

- [ ] **Codebase analysis completed** - existing functions reviewed for reuse
- [ ] **Maximum code reuse achieved** - no unnecessary duplication
- [ ] **Code passes ALL lintr checks** (run `lintr::lint_package()`)
- [ ] **ALL functions have complete unit tests** (100% coverage required)
- [ ] **ALL tests pass locally** (run `devtools::test()`)
- [ ] All functions have complete roxygen documentation
- [ ] Code follows consistent style (use `styler` package)
- [ ] No hardcoded paths or system-specific dependencies
- [ ] Environment variables are properly configured in `.Renviron`
- [ ] Error messages are user-friendly and informative
- [ ] All exported functions are actually meant to be exported
- [ ] `.lintr` configuration is respected and not modified without permission
- [ ] **New functions justified** - couldn't be accomplished through reuse

### Input Validation Pattern
Every function should validate inputs:

```r
my_function <- function(x, y = NULL) {
  # Input validation
  if (!is.numeric(x)) {
    stop("x must be numeric", call. = FALSE)
  }
  if (length(x) == 0) {
    stop("x cannot be empty", call. = FALSE)
  }
  
  # Function logic here
}
```

### Error Handling Standards
- Use `stop()` for fatal errors with `call. = FALSE`
- Use `warning()` for non-fatal issues
- Provide clear, actionable error messages
- Include information about what input caused the error

## Package Structure Requirements

### Essential Files
```
package-name/
├── R/                          # R source code
├── man/                        # Documentation (auto-generated)
├── tests/testthat/            # Unit tests
├── vignettes/                 # Long-form documentation
├── data/                      # Package data
├── inst/                      # Installed files
├── DESCRIPTION                # Package metadata
├── NAMESPACE                  # Exports (auto-generated)
├── README.md                  # Package overview
├── NEWS.md                    # Change log
└── .Rbuildignore             # Files to ignore in build
```

### DESCRIPTION File Requirements
Must include appropriate:
- Version numbers (semantic versioning)
- Dependencies with version constraints
- License information
- Author and maintainer information
- Package description and title

## Development Process

### Step-by-Step Workflow

1. **Planning Phase**
   - Present detailed plan in bullet points
   - Ask clarifying questions
   - Wait for approval

2. **Implementation Phase**
   - Write roxygen documentation first
   - Write unit tests second
   - Implement function third
   - Run tests and ensure they pass

3. **Review Phase**
   - Check code style with `styler::style_pkg()`
   - Run `devtools::check()` to ensure package integrity
   - Verify documentation with `devtools::document()`
   - Confirm all tests pass with `devtools::test()`

4. **Integration Phase**
   - Update NEWS.md with changes
   - Update package version if needed
   - Ensure examples in documentation work

## Quality Assurance

### Automated Checks
Run these commands regularly:
```r
# Linting (MANDATORY - must pass before any commits)
lintr::lint_package()

# Style checking
styler::style_pkg()

# Documentation generation
devtools::document()

# Run tests
devtools::test()

# Full package check
devtools::check()

# Test coverage
covr::package_coverage()
```

### Linting Requirements
- **ALWAYS check `.lintr` file** in project root for linting rules
- **ALL code must pass linting** before any commits or pull requests
- Run `lintr::lint_package()` after every code change
- Fix ALL linting issues - no exceptions
- If linting rules seem problematic, ASK before suggesting changes to `.lintr`

### Manual Review Points
- Are function names intuitive and consistent?
- Do examples in documentation actually work?
- Are error messages helpful to end users?
- Is the API consistent across similar functions?
- Are there any breaking changes that need version bumps?
- **Does ALL code pass lintr checks with project's `.lintr` configuration?**
- Do all functions follow the linting style guide?

## Communication Protocol

### When to Ask Questions
Ask questions when:
- Requirements are ambiguous or incomplete
- Multiple valid approaches exist
- Performance vs. simplicity trade-offs arise
- Breaking changes might be necessary
- Dependencies need to be added
- API design decisions need to be made

### How to Present Plans
Always structure proposals as:
```
## Proposed Implementation:
• [Specific action 1]
• [Specific action 2]
• [Files that will be created/modified]

## Questions for you:
• [Specific question 1]
• [Specific question 2]

## Potential concerns:
• [Any risks or limitations]
```

### Required Confirmations
Get explicit approval for:
- Adding new dependencies
- Changing function signatures
- Modifying exported APIs
- Major refactoring
- Performance optimizations that change behavior
- **Creating new functions** (must include test creation plan)
- **Any modifications to README.md or protected documentation files**
- **Starting work on a new feature** (must get feature definition approval)
- **Creating new branches** for features
- **Abandoning a feature branch** if implementation fails

## CI/CD Integration

### GitHub Actions Workflow
Create `.github/workflows/R-CMD-check.yml` with these mandatory checks:

```yaml
name: R-CMD-check
on: [push, pull_request]

jobs:
  R-CMD-check:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3
    
    - uses: r-lib/actions/setup-r@v2
      with:
        r-version: 'release'
    
    - uses: r-lib/actions/setup-pandoc@v2
    
    - name: Install dependencies
      run: |
        install.packages(c("remotes", "rcmdcheck", "lintr", "covr"))
        remotes::install_deps(dependencies = TRUE)
      shell: Rscript {0}
    
    - name: Check .Renviron exists
      run: |
        if (!file.exists(".Renviron")) {
          stop(".Renviron file is required for environment variables")
        }
      shell: Rscript {0}
    
    - name: Lint package (MUST PASS)
      run: |
        lints <- lintr::lint_package()
        if (length(lints) > 0) {
          print(lints)
          stop("Linting failed. Fix all linting issues before merging.")
        }
      shell: Rscript {0}
    
    - name: Run tests (MUST PASS)
      run: |
        test_results <- devtools::test()
        if (any(test_results$failed > 0)) {
          stop("Tests failed. All tests must pass before merging.")
        }
      shell: Rscript {0}
    
    - name: Check test coverage (MUST BE 100%)
      run: |
        coverage <- covr::package_coverage()
        coverage_percent <- covr::percent_coverage(coverage)
        cat("Test coverage:", coverage_percent, "%\n")
        if (coverage_percent < 100) {
          stop("Test coverage must be 100%. Current coverage: ", coverage_percent, "%")
        }
      shell: Rscript {0}
    
    - name: Check package
      run: |
        rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "error")
      shell: Rscript {0}
```

### Environment Variables Configuration
- **ALWAYS use `.Renviron` file** for environment variables
- Load environment variables at package startup if needed:
  ```r
  .onLoad <- function(libname, pkgname) {
    # Load .Renviron if it exists
    if (file.exists(".Renviron")) {
      readRenviron(".Renviron")
    }
  }
  ```
- **Test environment variables** in dedicated test files
- **NEVER hardcode sensitive values** - always use environment variables
- Document required environment variables in README.md

### CI/CD Environment Setup
Add this to your CI/CD workflow to handle environment variables:

```yaml
    - name: Setup test environment variables
      run: |
        # Copy .Renviron to home directory for CI testing
        if [ -f .Renviron ]; then
          cp .Renviron ~/.Renviron
        fi
      shell: bash
```

### Pre-commit Hooks (Recommended)
Set up pre-commit hooks to run locally:

```bash
# Install pre-commit
pip install pre-commit

# Create .pre-commit-config.yaml
repos:
- repo: local
  hooks:
  - id: lintr
    name: lintr
    entry: Rscript -e "lintr::lint_package()"
    language: system
    files: \\.R$
    pass_filenames: false
  - id: roxygenize
    name: roxygenize
    entry: Rscript -e "roxygen2::roxygenize()"
    language: system
    files: \\.R$
    pass_filenames: false
```

### Branch Protection Rules
Configure these rules in your repository:
- Require status checks to pass before merging
- Require branches to be up to date before merging
- Include "R-CMD-check" as required status check
- **Linting must pass** - no exceptions

## Linting Integration

### Pre-Development Linting Check
Before starting any coding session:
1. Run `lintr::lint_package()` to check current state
2. Fix any existing linting issues first
3. Proceed with new development

### During Development
- Run `lintr::lint()` on individual files as you work
- Fix linting issues immediately - don't accumulate technical debt
- If uncertain about a linting rule, ASK for clarification

### Linting Workflow Integration
```r
# Add this to your development workflow
lint_and_check <- function() {
  cat("Running linting checks...\n")
  lints <- lintr::lint_package()
  
  if (length(lints) > 0) {
    cat("❌ Linting failed!\n")
    print(lints)
    return(FALSE)
  } else {
    cat("✅ Linting passed!\n")
    return(TRUE)
  }
}

# Use before commits
if (lint_and_check()) {
  cat("Ready to commit!\n")
} else {
  cat("Fix linting issues before committing.\n")
}
```

### Code Organization
- One function per file (generally)
- Group related helper functions together
- Use consistent naming conventions (snake_case for functions, PascalCase for S4 classes)
- Include examples in all exported function documentation

### Data Management
- Include sample datasets in `data/` folder
- Document datasets with roxygen
- Keep data files small (<1MB when possible)
- Use appropriate compression for larger datasets

### Version Control Integration
- Update NEWS.md for every user-facing change
- Use semantic versioning (MAJOR.MINOR.PATCH)
- Tag releases appropriately
- Maintain backwards compatibility when possible

### Performance Considerations
- Profile code for bottlenecks when performance matters
- Consider vectorization over loops
- Use appropriate data structures
- Document time/space complexity for algorithms

---

## Emergency Stops

**STOP IMMEDIATELY** and ask for clarification if:
- You're unsure about the intended behavior
- The request seems to conflict with R package best practices
- You're about to make breaking changes
- **Tests are failing** and the fix isn't obvious
- **Linting issues** cannot be resolved without changing logic
- Dependencies would significantly increase package size
- **Environment variables** are needed but not defined in `.Renviron`
- **Test coverage** would drop below 100%
- **About to modify README.md or other protected files** without a plan
- **Creating a new function** without having created its test file first
- **A feature appears to be complete** - ask about next feature and branching
- **Implementation is getting complex** - consider if feature should be split
- **Considering working on multiple features** simultaneously (NOT ALLOWED)

Remember: **It's always better to ask than to assume!**

## Feature Completion and Branching Protocol

### When Feature is Complete
**COPILOT MUST AUTOMATICALLY DETECT AND STOP when:**
- All planned functions for a feature are implemented and tested
- All tests pass with 100% coverage
- All linting passes
- Feature works as originally specified

**Then IMMEDIATELY ask:**
```
🎉 Feature [Name] appears complete!

## Completion Status:
✅ All functions implemented
✅ 100% test coverage
✅ All tests passing
✅ Linting passes
✅ Documentation complete

## Next Steps:
Should I:
• Create a new branch for the next feature?
• What feature should we work on next?
• Are you satisfied with this feature implementation?
```

### Branch Transition Protocol
**Between features, COPILOT MUST:**
1. **Confirm current feature is truly complete**
2. **Ask what the next feature should be**
3. **Get approval to create new branch**
4. **Name the branch appropriately** (feature/XXX-feature-name)
5. **Start the new feature planning cycle**

### Feature Isolation Rules
- **One feature per branch** - no exceptions
- **No cross-feature dependencies** unless explicitly planned
- **Each feature must be independently testable**
- **No mixing of feature work** in the same development session

## Environment Variables Best Practices

### Function Implementation
When functions need environment variables:

```r
#' Function that uses environment variables
#' @param config_var Optional override for CONFIG_VAR environment variable
#' @return Processed result
#' @examples
#' # Assumes CONFIG_VAR is set in .Renviron
#' result <- my_function()
my_function <- function(config_var = NULL) {
  # Try parameter first, then environment variable
  if (is.null(config_var)) {
    config_var <- Sys.getenv("CONFIG_VAR")
    if (nzchar(config_var) == 0) {
      stop("CONFIG_VAR environment variable must be set in .Renviron", 
           call. = FALSE)
    }
  }
  
  # Function logic here
}
```

### Testing Environment Variables
**ALWAYS test both scenarios:**
- When environment variable is present
- When environment variable is missing
- When environment variable has invalid values
- Test cleanup to restore original state

### .Renviron Documentation
Document all required environment variables in README.md:

```markdown
## Environment Variables

This package requires the following environment variables to be set in `.Renviron`:

- `CONFIG_VAR`: Description of what this variable controls
- `API_KEY`: API key for external service (if applicable)
- `DEBUG_MODE`: Set to "TRUE" for verbose logging (optional)

Example `.Renviron` file:
```
CONFIG_VAR=production
API_KEY=your_api_key_here
DEBUG_MODE=FALSE
```
```

**ABSOLUTE REQUIREMENTS FOR PUSHING:**
1. ✅ All linting checks pass (`lintr::lint_package()`)
2. ✅ 100% test coverage (`covr::package_coverage()`)
3. ✅ All tests pass (`devtools::test()`)
4. ✅ Package check passes (`devtools::check()`)
5. ✅ All environment variables documented and tested