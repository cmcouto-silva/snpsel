#' @title Load multiple packages in R
#' @description This function loads multiple packages according to their names, and install them if they're not already installed.
#' @param ... Package names (with or without double quotes)
#' @details Only packages available on CRAN's repository will be automatically installed using this functions.
#' For packages stored in another repository, please install them separately according to proper instructions.
#' @return Packages attached into namespace.
#' @examples
#' \dontrun{
#' pkg.lib(data.table, dplyr, knitr)
#' }
#' @importFrom utils install.packages
#' @export
#' @author Cainã Max Couto-Silva

pkg.lib <- function(...) {
  pkgs <- as.character(sys.call(0))[-1]

  sapply(pkgs, function(pkg){
    if(!requireNamespace(pkg, quietly = T)) {
      install.packages(pkg, dependencies = TRUE, quiet = T)
    }

    tryCatch({
    library(pkg, character.only = TRUE)
    cat(paste0('  Package "', pkg, '" successfully loaded!\n'))
    }, error = function(e) cat(paste0("  unable to load ‘", pkg, "’ package\n")))
    })

  return(invisible())
}

