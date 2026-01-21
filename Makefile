# Makefile
all: check test coverage

# 1. The Build Check (Strict)
check:
	@echo "Running Build Check..."
	Rscript -e 'devtools::check(document = FALSE, error_on = "error")'

# 2. Standard Unit Tests
test:
	@echo "Running Unit Tests..."
	Rscript -e 'devtools::test()'

# 3. Your Advanced Coverage
coverage:
	@echo "Running Complex Coverage..."
	Rscript tools/ci_coverage.R

clean:
	rm -f coverage-report.html
	rm -rf *.Rcheck
