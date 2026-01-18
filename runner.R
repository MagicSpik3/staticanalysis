# runner
# This loads all your functions into memory immediately
devtools::install()
devtools::load_all()
lintr::lint_package()
usethis::use_package("diffobj")
here::dr_here()
devtools::check()
usethis::proj_sitrep()
ls("package:staticanalysis")
list.files("tests/testthat", recursive = TRUE)

# show the tre
fs::dir_tree(".", recurse = TRUE)

devtools::load_all()
sapply(ls("package:staticanalysis"), get)


library(staticanalysis)
g <- visualize_rules("tests/testthat/artifacts/accountant_rules.csv")
print(g)



package_overview <- function() {
  cat("Package root:", usethis::proj_get(), "\n\n")

  cat("R functions:\n")
  print(list.files("R", pattern = "\\.R$", full.names = TRUE))
  cat("\n")

  cat("Tests:\n")
  print(list.files("tests/testthat", pattern = "\\.R$", full.names = TRUE))
  cat("\n")

  devtools::load_all(quiet = TRUE)
  cat("Exported functions:\n")
  print(getNamespaceExports("staticanalysis"))
}


package_overview()


devtools::load_all() # Load new functions
devtools::test()     # Run new tests
