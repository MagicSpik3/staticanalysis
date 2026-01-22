# Makefile
#all: check test coverage

# 1. Build Check
#check:
#	@echo "Running Build Check..."
#	Rscript -e 'devtools::check(document = FALSE, error_on = "error")'

# 2. Tinytest Execution
#test:
#	@echo "Running Tiny Tests..."
	# We run test_all(".") and throw an error if any test is not TRUE
#	Rscript -e 'out <- tinytest::test_all("."); if(!all(tinytest::anyPass(out))) stop("Tests Failed")'

# 3. Coverage (Covr auto-detects tinytest)
#coverage:
#	@echo "Running Coverage..."
#	Rscript tools/ci_coverage.R

#clean:
#	rm -f coverage-report.html
#	rm -rf *.Rcheck


# Makefile
all: check test coverage lint style

# 1. Build Check
check:
	@echo "Running Build Check..."
	Rscript -e 'devtools::check(document = FALSE, error_on = "error")'

# 2. Revert to Testthat
test:
	@echo "Running Unit Tests..."
	Rscript -e 'devtools::test()'

# 3. Coverage (works with testthat automatically)
coverage:
	@echo "Running Coverage..."
	Rscript tools/ci_coverage.R

# 4. Helpers
lint:
	@echo "Running Linter..."
	Rscript -e 'lints <- lintr::lint_package(); print(lints); if(length(lints) > 0) stop("Linter found issues!")'

style:
	@echo "Checking Code Style..."
	Rscript -e 'if(any(styler::style_pkg(dry = "on")$changed)) stop("Code is not styled! Run styler::style_pkg() locally.")'
