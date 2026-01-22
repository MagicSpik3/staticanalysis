# Makefile
all: check test coverage

# 1. Build Check
check:
	@echo "Running Build Check..."
	Rscript -e 'devtools::check(document = FALSE, error_on = "error")'

# 2. Tinytest Execution
test:
	@echo "Running Tiny Tests..."
	# We run test_all(".") and throw an error if any test is not TRUE
	Rscript -e 'out <- tinytest::test_all("."); if(!all(tinytest::anyPass(out))) stop("Tests Failed")'

# 3. Coverage (Covr auto-detects tinytest)
coverage:
	@echo "Running Coverage..."
	Rscript tools/ci_coverage.R

clean:
	rm -f coverage-report.html
	rm -rf *.Rcheck
