# Makefile
all: test coverage

test:
	Rscript -e 'devtools::test()'

coverage:
	Rscript -e 'covr::report(file = "coverage-report.html")'
	@echo "Coverage report generated at coverage-report.html"

check:
	Rscript -e 'devtools::check()'

clean:
	rm -f coverage-report.html
