
.onLoad <- function(libname, pkgname) {
  options("getSymbols.warning4.0"=FALSE)
  options(scipen = 14)
  packageStartupMessage("Written by Joo, Seokhoon.")
}
