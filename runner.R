# runner
# This loads all your functions into memory immediately
devtools::install()
devtools::load_all()
lintr::lint_package()
usethis::use_package("diffobj")

library(staticanalysis)
g <- visualize_rules("tests/testthat/artifacts/accountant_rules.csv")
print(g)
