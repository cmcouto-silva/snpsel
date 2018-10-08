#' @export
#' @author Cainã Max Couto-Silva
#'
ignore.path <- function(file_path) {
if(grepl("/", file_path)) file_path <- gsub(".*/(.+)", "\\1", file_path)
if(grepl("/$", file_path)) file_path <- unlist(strsplit(file_path, "/$"))
return(file_path)
}
