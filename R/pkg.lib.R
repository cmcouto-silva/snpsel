#' @title Load multiple packages in R
#' @description This function loads multiple packages according to a vector of names, and install them if they're not already installed. Only packages avaialable on CRAN will be installed.
#' @param packages A vector with package names.
#' @return Packages attached into namespace.
#' @examples
#' \dontrun{
#' required_packages <- c("tidyverse", "dplyr", "lubridate")
#' pkg.lib(packages = required_packages)
#' }
#' @export

pkg.lib <- function(...) {
  pkgs <- as.character(sys.call(0))[-1]
  sapply(pkgs, function(pkg){
    if(!requireNamespace(pkg)) install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
    cat(paste0('  Package "', pkg, '" successfully loaded!\n'))
  })
  return(invisible())
}
