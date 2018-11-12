#' @export
#' @author Cainã Max Couto-Silva

ignore.path <- function(file_path) {
  sapply(file_path, function(fpath) {
    if(grepl("/", fpath)) fpath <- gsub(".*/(.+)", "\\1", fpath)
    if(grepl("/$", fpath)) fpath <- unlist(strsplit(fpath, "/$"))
    return(fpath)
  })
}

