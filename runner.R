# runner
# This loads all your functions into memory immediately
usethis::use_package("fs")      # For file system handling
usethis::use_package("dplyr")   # For counting frequencies
usethis::use_package("purrr")   # For iteration
usethis::use_package("diffobj")
usethis::proj_sitrep()
usethis::use_package("rlang")
usethis::use_package("devtools", type = "Suggests")
usethis::use_package("here", type = "Suggests")
usethis::use_package("lintr", type = "Suggests")
usethis::use_package("styler", type = "Suggests")

usethis::use_build_ignore(".lintr")
usethis::use_build_ignore("runner.R")
usethis::use_build_ignore("tests/testthat/artifacts") # Optional: ignore test artifacts

# 1. Promote 'rlang' to a real dependency (Required for .data)
usethis::use_package("rlang")

# 2. Move Dev Tools to "Suggests" (Optional, for development only)
# This stops R from installing them on the production server
usethis::use_package("devtools", type = "Suggests")
usethis::use_package("here", type = "Suggests")
usethis::use_package("lintr", type = "Suggests")
usethis::use_package("styler", type = "Suggests")
usethis::use_package("usethis", type = "Suggests")

# 3. Add 'diffobj' and 'xmlparsedata' (Required for the Audit tool we are about to build)
usethis::use_package("diffobj", type = "Suggests")
usethis::use_package("xmlparsedata", type = "Suggests")



library(staticanalysis)

devtools::install()
devtools::load_all()
devtools::document()

# 1. Remove the old corrupted installation
#remove.packages("staticanalysis")

# 2. Load the source code afresh
devtools::load_all()

# 3. Document (updates NAMESPACE)
devtools::document()

# 4. Check
devtools::check()

styler::style_pkg()
lintr::lint_package()
here::dr_here()
#devtools::check()
ls("package:staticanalysis")
list.files("tests/testthat", recursive = TRUE)

report <- scan_dependencies(".")
print(report$undeclared_ghosts)


# show the tre
fs::dir_tree(".", recurse = TRUE)

#devtools::load_all()
#sapply(ls("package:staticanalysis"), get)


#g <- visualize_rules("tests/testthat/artifacts/accountant_rules.csv")
#print(g)


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

#devtools::load_all() # Load new functions
devtools::test()     # Run new tests
