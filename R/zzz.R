
.onLoad <- function(libname, pkgname) {
  Sys.setlocale("LC_MESSAGES", "en_US.UTF-8")
  Sys.setlocale("LC_ALL"     , "en_US.UTF-8")
  options("getSymbols.warning4.0"=FALSE)
  options(scipen = 14)
  packageStartupMessage("Written by Joo, Seokhoon.")
}
