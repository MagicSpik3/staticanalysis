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
