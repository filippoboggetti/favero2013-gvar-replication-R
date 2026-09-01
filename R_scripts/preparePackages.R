preparePackages <- function(listofpackages) {
  for (pkg in listofpackages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
}
