# Contributing to the tube Package

Thank you for your interest in contributing to the tube package! This guide will help you set up your development environment and understand our development workflow.

## 🚀 Quick Start

### 1. Environment Setup

Run the one-click setup script to configure your development environment:

```bash
Rscript setup-dev-environment.R
```

This script will:
- Install all required development packages
- Validate linting configuration
- Test the QA pipeline
- Provide setup recommendations

### 2. Development Dependencies

The following R packages are required for development:

```r
# Core development packages
install.packages(c(
  "devtools",      # Package development
  "lintr",         # Code linting  
  "rcmdcheck",     # Package checking
  "styler",        # Code styling
  "roxygen2",      # Documentation
  "usethis",       # Development utilities
  "cli"            # Better console output
))

# Future testing packages (currently disabled)
install.packages(c(
  "testthat",      # Testing framework
  "covr"           # Coverage analysis
))
```

## 🛠️ Development Workflow

### Daily Development Cycle

1. **Make your changes** to R code or documentation
2. **Run linting check**:
   ```bash
   Rscript tools/qa-pipeline.R --lint-only
   ```
3. **Fix any linting issues** reported
4. **Run full QA pipeline**:
   ```bash
   Rscript tools/qa-pipeline.R
   ```
5. **Commit and push** your changes

### QA Tools

The `/tools/` directory contains modular QA scripts:

- **`qa-pipeline.R`** - Main orchestration script
- **`lint.R`** - Code linting checks
- **`check.R`** - Package validation 
- **`test.R`** - Unit tests (commented out)
- **`coverage.R`** - Test coverage (commented out)

#### QA Pipeline Options:

```bash
# Lint only (fastest check)
Rscript tools/qa-pipeline.R --lint-only

# Full pipeline (all enabled checks)
Rscript tools/qa-pipeline.R

# Individual components
Rscript tools/lint.R
Rscript tools/check.R
```

## 📋 Code Standards

### Linting Rules

We use a custom `.lintr` configuration that:
- Enforces 120-character line length
- Uses snake_case naming convention
- Allows both single and double quotes (for SQL queries)
- Excludes specific linters that cause false positives

### Code Style Guidelines

- **Function names**: Use `snake_case`
- **Line length**: Maximum 120 characters
- **Indentation**: 2 spaces
- **Comments**: Use `#'` for roxygen documentation
- **French language**: Use proper encoding for French text

### Documentation Requirements

All exported functions must include:

```r
#' @title Brief title (required)
#' @description Detailed description (required) 
#' @param param_name Description of parameter
#' @return Description of return value
#' @examples
#' # Example usage
#' result <- my_function(param = "value")
#' @export
```

## 🔍 Quality Assurance

### Mandatory Checks

Before any commit, ensure:
- ✅ **All linting checks pass** (`Rscript tools/qa-pipeline.R --lint-only`)
- ✅ **Package check passes** (`Rscript tools/qa-pipeline.R`) 
- ✅ **No breaking changes** to existing functionality
- ✅ **Documentation is complete** for new functions

### CI/CD Integration

Our GitHub Actions workflow automatically:
- Runs linting checks (blocking)
- Performs package validation
- Tests across multiple R versions
- Validates documentation

## 🌍 Special Considerations

### French Language Support

This package provides a French-language interface:
- Use proper French text with accents
- ASCII checks are disabled in CI/CD
- Include English comments in code for international contributors

### Environment Variables

- Use `.Renviron` file for local configuration
- Document required environment variables in README.md
- Never commit sensitive values

## 🚨 Before You Submit

### Pre-commit Checklist

- [ ] Run `Rscript setup-dev-environment.R` (if first contribution)
- [ ] All linting checks pass
- [ ] Package validation successful  
- [ ] New functions have complete documentation
- [ ] No hardcoded sensitive values
- [ ] Changes follow existing code patterns

### Pull Request Guidelines

1. **Branch naming**: Use descriptive names (`feature/new-function`, `fix/bug-description`)
2. **Commit messages**: Use conventional commits format
3. **Description**: Clearly explain what changes and why
4. **Testing**: Verify your changes work as expected

## 🆘 Getting Help

### Common Issues

**Linting failures**: Check `.lintr` configuration and fix reported issues
**Package check warnings**: Review R CMD check output for guidance  
**Environment setup**: Re-run `setup-dev-environment.R`

### Resources

- [R Package Development](https://r-pkgs.org/)
- [Writing R Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html)
- [Advanced R](https://adv-r.hadley.nz/)

### Contact

For questions or support:
- Open an issue on GitHub
- Check existing documentation
- Review similar implementations in the codebase

---

**Thank you for contributing to the tube package! 🚀**
